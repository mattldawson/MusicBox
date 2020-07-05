module environ_conditions_mod

!---------------------------------------------------------------------
! This module retrieves the enviromental conditions from a named file
!---------------------------------------------------------------------

!   USE ccpp_kinds, ONLY: rk => kind_phys
  USE ccpp_kinds, ONLY: kind_phys

  use input_file,  only: input_file_type, MAX_ATT_LEN
  use input_slice, only: slice_type

  implicit none

  type, public :: environ_conditions
     private
     type(input_file_type) :: inputfile
     type(slice_type) :: slice
     real(kind_phys) :: wghts(2) = 0._kind_phys
     integer :: num_times
     real, allocatable :: times(:)
   contains
     procedure :: getvar => environ_conditions_getvar
     procedure :: getcol => environ_conditions_getcol
     procedure :: getsrf => environ_conditions_getsrf
     procedure :: press_mid => environ_conditions_press_mid
     procedure :: press_int => environ_conditions_press_int
     procedure :: dtime => environ_conditions_dtime
     procedure :: ntimes => environ_conditions_ntimes
     procedure :: get_times => environ_conditions_times
     procedure :: get_units => environ_conditions_units
     procedure :: nlayers => environ_conditions_nlayers
     procedure :: levnum => environ_conditions_levnum
     procedure :: environ_conditions_update_ndx
     procedure :: environ_conditions_update_flt
     generic   :: update => environ_conditions_update_ndx, environ_conditions_update_flt
  end type environ_conditions
  
contains


  function environ_conditions_create( infilepath, lat, lon, lev ) result(env_cond)
    
    character(len=*), intent(in) :: infilepath
    real, intent(in) ::  lat, lon
    real, intent(in), optional :: lev
    
    type(environ_conditions), pointer :: env_cond
    character(len=MAX_ATT_LEN) :: units
    
    allocate(env_cond)

    call env_cond%inputfile%open( infilepath )
    if (present(lev)) then
       env_cond%slice = env_cond%inputfile%set_slice( beglat=lat,endlat=lat, beglon=lon,endlon=lon, beglev=lev,endlev=lev)
    else
       env_cond%slice = env_cond%inputfile%set_slice( beglat=lat,endlat=lat, beglon=lon,endlon=lon )
    end if

    env_cond%slice%ntimes = 1

    env_cond%num_times = env_cond%inputfile%get_ntimes()
    allocate(env_cond%times(env_cond%num_times))

    units = env_cond%inputfile%get_units('time')
    if (index(units,'days')>0) then
       ! convert to seconds
       env_cond%times = 24._kind_phys*3600._kind_phys* env_cond%inputfile%get_times()
    else if (index(units,'sec')>0) then
       env_cond%times = env_cond%inputfile%get_times()
    else
       write(*,*) 'ERROR: Do not recognize time units in file: '//trim(infilepath)
       call abort()
    end if
 
  end function environ_conditions_create

  subroutine environ_conditions_update_ndx(this, record_num)
    class(environ_conditions), intent(inout) :: this
    integer, intent(in) :: record_num
    
    this%slice%begtime = record_num
  end subroutine environ_conditions_update_ndx

  subroutine environ_conditions_update_flt(this, time)
    class(environ_conditions), intent(inout) :: this
    real(kind_phys), intent(in) :: time

    integer :: ndx

    if (time<=this%times(1)) then
       this%slice%begtime = 1
       this%slice%ntimes = 1
       this%wghts = 0._kind_phys
    elseif (time>=this%times(this%num_times)) then
       this%slice%begtime = this%num_times
       this%slice%ntimes = 1      
       this%wghts = 0._kind_phys
    else
       findtime: do ndx = 1,this%num_times-1
          if (this%times(ndx)>=time) then
             exit findtime
          end if
       end do findtime
       ndx=ndx-1
       this%slice%begtime = ndx
       this%slice%ntimes = 2
       this%wghts(2) = (time - this%times(ndx))/(this%times(ndx+1)-this%times(ndx))
       this%wghts(1) = 1._kind_phys-this%wghts(2)
    end if
    
  end subroutine environ_conditions_update_flt

  function environ_conditions_units(this, var, abort) result(theUnits)
    class(environ_conditions), intent(inout) :: this
    character(len=*),  intent(in) :: var
    logical, optional, intent(in) :: abort
    character(len=MAX_ATT_LEN) :: theUnits

    theUnits = this%inputfile%get_units(var,abort=abort)

  end function environ_conditions_units
  

  function environ_conditions_getvar(this, var, default_value) result(thevalue)
    class(environ_conditions), intent(inout) :: this
    character(len=*), intent(in) :: var
    real(kind_phys), optional, intent(in) :: default_value

    real(kind_phys) :: thevalue

    real(kind_phys), pointer :: data(:,:,:,:)
    
    data => this%inputfile%extract(var, this%slice, default_value)
    if (this%slice%ntimes == 2) then
       thevalue = this%wghts(1)*data(1,1,1,1) + this%wghts(2)*data(1,1,1,2)
    else
       thevalue = data(1,1,1,1)
    end if
    

  end function environ_conditions_getvar
  
  function environ_conditions_getcol(this, var, nlev) result(thecol)
    class(environ_conditions), intent(inout) :: this
    character(len=*), intent(in) :: var
    integer,intent(in) :: nlev
    real(kind_phys) :: thecol(nlev)

    real(kind_phys), pointer :: data(:,:,:,:)
    
    data => this%inputfile%extract(var, this%slice )
    if (this%slice%ntimes == 2) then
       thecol(:) = this%wghts(1)*data(1,1,:,1) + this%wghts(2)*data(1,1,:,2)
    else
       thecol(:) = data(1,1,:,1)
    end if

  end function environ_conditions_getcol
  
  function environ_conditions_getsrf(this, var ) result(theval)
    class(environ_conditions), intent(inout) :: this
    character(len=*), intent(in) :: var
    real(kind_phys) :: theval

    real, pointer :: data(:,:,:)
    
    data => this%inputfile%extract_srf(var, this%slice )
    if (this%slice%ntimes == 2) then
       theval = this%wghts(1)*data(1,1,1) + this%wghts(2)*data(1,1,2)
    else
       theval = data(1,1,1)
    end if
 
  end function environ_conditions_getsrf
  
  function environ_conditions_press_mid(this,nlev) result(press_mid)
    class(environ_conditions), intent(inout) :: this
    integer,intent(in) :: nlev
    real(kind_phys) :: press_mid(nlev)

    real, pointer :: data_srf(:,:,:)
    real :: ps
    
    data_srf=>this%inputfile%extract_srf('PS',this%slice)
    if (this%slice%ntimes == 2) then
       ps = this%wghts(1)*data_srf(1,1,1) + this%wghts(2)*data_srf(1,1,2)
    else
       ps = data_srf(1,1,1)
    end if

    press_mid(:) = 1.e5*this%inputfile%get_hyam() + ps*this%inputfile%get_hybm()

  end function environ_conditions_press_mid
  
  function environ_conditions_press_int(this,nlev) result(press_int)
    class(environ_conditions), intent(inout) :: this
    integer,intent(in) :: nlev
    real(kind_phys) :: press_int(nlev)

    real, pointer :: data_srf(:,:,:)
    real :: ps
    
    data_srf=>this%inputfile%extract_srf('PS',this%slice)
    if (this%slice%ntimes == 2) then
       ps = this%wghts(1)*data_srf(1,1,1) + this%wghts(2)*data_srf(1,1,2)
    else
       ps = data_srf(1,1,1)
    end if

    press_int(:) = 1.e5*this%inputfile%get_hyai() + ps*this%inputfile%get_hybi()

  end function environ_conditions_press_int
  
  function environ_conditions_ntimes(this) result(ntimes)
    class(environ_conditions), intent(in) :: this

    integer :: ntimes
    ntimes = this%num_times
    
  end function environ_conditions_ntimes
  function environ_conditions_times(this) result(times)
    class(environ_conditions), intent(in) :: this

    real(kind_phys) :: times(this%num_times)
    times(:) = this%times(:)
    
  end function environ_conditions_times
  
  function environ_conditions_nlayers(this) result(nlyrs)
    class(environ_conditions), intent(in) :: this

    integer :: nlyrs
    nlyrs = this%inputfile%get_nlevels()
    
  end function environ_conditions_nlayers
  
  function environ_conditions_dtime(this) result(dtime)
    class(environ_conditions), intent(in) :: this

    real :: dtime
    dtime = real(int(86400.0*this%inputfile%get_dtime())) ! convert to seconds
  end function environ_conditions_dtime

  function environ_conditions_levnum(this) result(levnum)
    class(environ_conditions), intent(in) :: this
    integer :: levnum

    levnum = this%slice%beglev

  end function environ_conditions_levnum
  
end module environ_conditions_mod
