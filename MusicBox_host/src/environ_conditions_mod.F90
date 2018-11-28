module environ_conditions_mod

!---------------------------------------------------------------------
! This module retrieves the enviromental conditions from a named file
!---------------------------------------------------------------------

  use machine,     only: rk => kind_phys
  use input_file,  only: input_file_type
  use input_slice, only: slice_type

  implicit none

  type, public :: environ_conditions
     private
     type(input_file_type) :: inputfile
     type(slice_type) :: slice
   contains
     procedure :: getvar => environ_conditions_getvar
     procedure :: getcol => environ_conditions_getcol
     procedure :: getsrf => environ_conditions_getsrf
     procedure :: press_mid => environ_conditions_press_mid
     procedure :: press_int => environ_conditions_press_int
     procedure :: dtime => environ_conditions_dtime
     procedure :: ntimes => environ_conditions_ntimes
     procedure :: nlevels => environ_conditions_nlevels
     procedure :: levnum => environ_conditions_levnum
     procedure :: update => environ_conditions_update     
  end type environ_conditions
  
contains


  function environ_conditions_create( infilepath, lat, lon, lev ) result(env_cond)

    character(len=*), intent(in) :: infilepath
    real, intent(in) ::  lat, lon
    real, intent(in), optional :: lev
    
    type(environ_conditions), pointer :: env_cond

    allocate(env_cond)
    
    call env_cond%inputfile%open( infilepath )
    if (present(lev)) then
       env_cond%slice = env_cond%inputfile%set_slice( beglat=lat,endlat=lat, beglon=lon,endlon=lon, beglev=lev,endlev=lev)
    else
       env_cond%slice = env_cond%inputfile%set_slice( beglat=lat,endlat=lat, beglon=lon,endlon=lon )
    end if
     
    env_cond%slice%ntimes = 1

  end function environ_conditions_create

  subroutine environ_conditions_update(this, record_num )
    class(environ_conditions), intent(inout) :: this
    integer, intent(in) :: record_num
    
    this%slice%begtime = record_num
  end subroutine environ_conditions_update

  function environ_conditions_getvar(this, var ) result(thevalue)
    class(environ_conditions), intent(inout) :: this
    character(len=*), intent(in) :: var
    real(rk) :: thevalue

    real, pointer :: data(:,:,:,:)
    
    data => this%inputfile%extract(var, this%slice )
    thevalue = data(1,1,1,1)

  end function environ_conditions_getvar
  
  function environ_conditions_getcol(this, var, nlev) result(thecol)
    class(environ_conditions), intent(inout) :: this
    character(len=*), intent(in) :: var
    integer,intent(in) :: nlev
    real(rk) :: thecol(nlev)

    real, pointer :: data(:,:,:,:)
    
    data => this%inputfile%extract(var, this%slice )
    thecol(:) = data(1,1,:,1)

  end function environ_conditions_getcol
  
  function environ_conditions_getsrf(this, var ) result(theval)
    class(environ_conditions), intent(inout) :: this
    character(len=*), intent(in) :: var
    real(rk) :: theval

    real, pointer :: data(:,:,:)
    
    data => this%inputfile%extract_srf(var, this%slice )
    theval = data(1,1,1)

  end function environ_conditions_getsrf
  
  function environ_conditions_press_mid(this,nlev) result(press_mid)
    class(environ_conditions), intent(inout) :: this
    integer,intent(in) :: nlev
    real(rk) :: press_mid(nlev)

    real, pointer :: data_srf(:,:,:)
    real :: ps
    
    data_srf=>this%inputfile%extract_srf('PS',this%slice)
    ps = data_srf(1,1,1)

    press_mid(:) = 1.e5*this%inputfile%get_hyam() + ps*this%inputfile%get_hybm()

  end function environ_conditions_press_mid
  
  function environ_conditions_press_int(this,nlev) result(press_int)
    class(environ_conditions), intent(inout) :: this
    integer,intent(in) :: nlev
    real(rk) :: press_int(nlev)

    real, pointer :: data_srf(:,:,:)
    real :: ps
    
    data_srf=>this%inputfile%extract_srf('PS',this%slice)
    ps = data_srf(1,1,1)

    press_int(:) = 1.e5*this%inputfile%get_hyai() + ps*this%inputfile%get_hybi()

  end function environ_conditions_press_int
  
  function environ_conditions_ntimes(this) result(ntimes)
    class(environ_conditions), intent(in) :: this

    integer :: ntimes
    ntimes = this%inputfile%get_ntimes()
    
  end function environ_conditions_ntimes
  
  function environ_conditions_nlevels(this) result(nlevs)
    class(environ_conditions), intent(in) :: this

    integer :: nlevs
    nlevs = this%inputfile%get_nlevels()
    
  end function environ_conditions_nlevels
  
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
