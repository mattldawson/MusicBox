module environ_conditions_mod
  use input_file, only: input_file_type
  use input_slice, only : slice_type

  implicit none

  type, public :: environ_conditions
     private
     type(input_file_type) :: inputfile
     type(slice_type) :: slice
   contains
     procedure :: getvar => environ_conditions_getvar
     procedure :: dtime => environ_conditions_dtime
     procedure :: ntimes => environ_conditions_ntimes
     procedure :: update => environ_conditions_update     
  end type environ_conditions
  
contains


  function environ_conditions_create( infilepath, lat, lon, lev ) result(env_cond)

    character(len=*), intent(in) :: infilepath
    real, intent(in) ::  lat, lon, lev
    
    type(environ_conditions), pointer :: env_cond

    allocate(env_cond)
    
    call env_cond%inputfile%open( infilepath )
    env_cond%slice = env_cond%inputfile%set_slice( beglat=lat,endlat=lat, beglon=lon,endlon=lon, beglev=lev,endlev=lev)
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

    real, pointer :: data(:,:,:,:)
    real :: thevalue
    
    data => this%inputfile%extract(var, this%slice )
    thevalue = data(1,1,1,1)

  end function environ_conditions_getvar
  
  function environ_conditions_ntimes(this) result(ntimes)
    class(environ_conditions), intent(in) :: this

    integer :: ntimes
    ntimes = this%inputfile%get_ntimes()
    
  end function environ_conditions_ntimes
  
  function environ_conditions_dtime(this) result(dtime)
    class(environ_conditions), intent(in) :: this

    real :: dtime
    dtime = real(int(86400.0*this%inputfile%get_dtime())) ! convert to seconds
  end function environ_conditions_dtime
  
end module environ_conditions_mod
