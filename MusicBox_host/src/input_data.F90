module input_file

  use netcdf, only: nf90_open, nf90_nowrite, nf90_noerr, nf90_inq_dimid, nf90_inquire_dimension
  use netcdf, only: nf90_inq_varid, nf90_get_var
  
  implicit none

  private

  type, public :: input_file_type

     private
     integer :: ncid

     real, allocatable :: lons(:)
     real, allocatable :: lats(:)
     real, allocatable :: levs(:)
     real, allocatable :: times(:)

     integer :: nlons
     integer :: nlats
     integer :: nlevs
     integer :: ntimes

   contains
     procedure :: open => input_file_open
     procedure :: set_slice => input_file_slice
     procedure :: extract => input_file_extract_slice
     procedure :: get_ntimes => input_file_get_ntimes
  end type input_file_type

contains

  integer function input_file_get_ntimes(this)
    class(input_file_type), intent(in) :: this
    input_file_get_ntimes = this%ntimes
  end function input_file_get_ntimes

  subroutine input_file_open(this, filename )

    class(input_file_type), intent(inout) :: this

    character(len=*), intent(in) :: filename
    
    integer :: status, dimid

    print*,'input_file_open.. filename = >|'//trim(filename)//'|<'
    
    status = nf90_open(filename, nf90_nowrite, this%ncid)
    if(status /= nf90_noerr) call handle_err(status)

    call set_coordinate(this%ncid, 'lat',  this%nlats,  this%lats )
    call set_coordinate(this%ncid, 'lon',  this%nlons,  this%lons )
    call set_coordinate(this%ncid, 'lev',  this%nlevs,  this%levs )
    call set_coordinate(this%ncid, 'time', this%ntimes, this%times )

  contains

    subroutine set_coordinate(fileid, dimname, ndim, coordvar )

      integer, intent(in) :: fileid
      character(len=*), intent(in) :: dimname
      integer, intent(out) :: ndim
      real, allocatable, intent(out) :: coordvar(:)

      integer :: dimid, varid

      status = nf90_inq_dimid(fileid, dimname, dimid)
      if (status /= nf90_noerr) call handle_err(status)
      
      status = nf90_inquire_dimension(fileid, dimid, len = ndim)
      if (status /= nf90_noerr) call handle_err(status)

      allocate(coordvar(ndim))

      status = nf90_inq_varid(fileid, dimname, varid)
      if(status /= nf90_noerr) call handle_err(status)

      status = nf90_get_var(fileid, varid, coordvar)
      if(status /= nf90_noerr) call handle_err(status)
 
    end subroutine set_coordinate
  
  end subroutine input_file_open

  function input_file_slice(this, beglon,endlon, beglat,endlat, beglev,endlev, begtime,endtime) result( slice )
    use input_slice, only : slice_type

    class(input_file_type), intent(in) :: this
    real, optional, intent(in) :: beglon,endlon, beglat,endlat, beglev,endlev
    real, optional, intent(in) :: begtime,endtime
    
    type(slice_type) :: slice

    integer :: i

    if (present(beglon) .and. present(endlon)) then
       call get_slice( this%nlons,this%lons,  beglon,endlon, slice%beglon, slice%nlons )
    else
       slice%beglon = 1
       slice%nlons = this%nlons
    endif

    if (present(beglat) .and. present(endlat)) then
       call get_slice( this%nlats,this%lats,  beglat,endlat, slice%beglat, slice%nlats )
    else
       slice%beglat = 1
       slice%nlats = this%nlats
    endif
   
    if (present(beglev) .and. present(endlev)) then
       call get_slice( this%nlevs,this%levs,  beglev,endlev, slice%beglev, slice%nlevs )
    else
       slice%beglev = 1
       slice%nlevs = this%nlevs
    endif
    
    if (present(begtime) .and. present(endtime)) then
       call get_slice( this%ntimes,this%times,  begtime,endtime, slice%begtime, slice%ntimes )
    else
       slice%begtime = 1
       slice%ntimes = 1
    end if
    
  contains
    
    subroutine get_slice(ncoord,coords, begcoord,endcoord, begslice, nslice )

      integer, intent(in) :: ncoord
      real, intent(in) :: coords(:)
      real, intent(in) :: begcoord,endcoord
      integer, intent(out) :: begslice, nslice

      nslice = 0
      begslice = -1
      findslice: do i = 1,ncoord
         if (coords(i)>= begcoord .and. begslice<1) then
            begslice = i
            nslice = nslice+1
        elseif (coords(i)>= begcoord .and. coords(i)<=endcoord) then
            nslice = nslice+1
         endif
         if (coords(i) >= endcoord) then
            exit findslice
         endif
      end do findslice

      if (begslice<1) then
         if (begcoord<coords(1)) then
            begslice=1
            nslice=1
         elseif (begcoord>coords(ncoord)) then
            begslice=ncoord
            nslice=1
         end if
      end if

    end subroutine get_slice
  
  end function input_file_slice

  function input_file_extract_slice(this, varname, slice ) result(data)
    use input_slice, only : slice_type

    class(input_file_type), intent(inout) :: this
    
    character(len=*), intent(in) :: varname
    type(slice_type), intent(in) :: slice
    
    real, pointer :: data(:,:,:,:)

    integer :: varid, status

    allocate(data(slice%nlons,slice%nlats,slice%nlevs,slice%ntimes))
    data = -1.e36
    
    status = nf90_inq_varid(this%ncid, varname, varid)
    if(status /= nf90_noerr) call handle_err(status)
    
    status = nf90_get_var(this%ncid, varid, data, &
                            start = (/ slice%beglon, slice%beglat, slice%beglev, slice%begtime /),     &
                            count = (/ slice%nlons,  slice%nlats,  slice%nlevs,  slice%ntimes /))
    if(status /= nf90_noerr) call handle_err(status)
    
  end function input_file_extract_slice

  subroutine handle_err(status)

    integer, intent(in) :: status

    write(*,*) 'ERROR input_file; status = ', status
    call exit(status)
    
  end subroutine handle_err
  
end module input_file
