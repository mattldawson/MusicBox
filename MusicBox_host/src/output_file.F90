!----------------------------------------------------------------------------------------------
! for writing time series output to netcdf
!----------------------------------------------------------------------------------------------
module output_file
!  USE ccpp_kinds, ONLY: rk => kind_phys
  USE ccpp_kinds, ONLY: kind_phys

  use netcdf, only: nf90_create, NF90_CLOBBER, nf90_def_dim, NF90_UNLIMITED, nf90_def_var
  use netcdf, only: nf90_close, nf90_put_var, nf90_enddef, NF90_DOUBLE, nf90_put_att
  use netcdf, only: nf90_noerr, nf90_strerror, NF90_MAX_VAR_DIMS, nf90_inq_varid
  use netcdf, only: nf90_inquire_variable, nf90_inq_dimid, nf90_inquire_dimension

  use music_box_assert, only: assert_msg
  use music_box_string, only: to_char

  use const_props_mod, only: const_props_type
  
  implicit none

  private
  
  integer, parameter :: MAXVARS=2000
  
  type, public :: output_file_type
     private
     integer :: ncid
     integer :: rec_dimid
     integer :: rec_num
     integer :: time_varid
     integer :: indx
     integer :: varid(MAXVARS)=0
     character(len=64) :: varname(MAXVARS)=' '
   contains
     procedure :: create => output_file_create
     procedure :: define => output_file_define
     procedure :: add_dimension => output_file_add_dimension
     procedure :: output_file_add_cnsts
     procedure :: output_file_add_var
     generic   :: add => output_file_add_cnsts, output_file_add_var
     procedure :: advance => output_file_adv
     procedure :: set_variable
     procedure :: output_file_out_cnsts
     procedure :: output_file_out_var
     procedure :: output_file_out_grid_var
     generic   :: out => output_file_out_cnsts, output_file_out_var,             &
                         output_file_out_grid_var
     procedure :: close => output_file_close
     procedure :: varindx
  end type output_file_type
  
contains

!--------------------------------------------------------------------------------
! create the netcdf file
!--------------------------------------------------------------------------------
  subroutine output_file_create(this, filename)

    class(output_file_type), intent(inout) :: this

    character(len=*), intent(in) :: filename

    call check( nf90_create(filename, NF90_CLOBBER, this%ncid) )
    call check( nf90_def_dim(this%ncid, 'time', NF90_UNLIMITED, this%rec_dimid) )
    call check( nf90_def_var(this%ncid, 'time', NF90_DOUBLE, (/ this%rec_dimid /), this%time_varid) )
    call check( nf90_put_att(this%ncid, this%time_varid, "units", "sec") )
    this%indx = 0
    this%rec_num = 0

  end subroutine output_file_create

!--------------------------------------------------------------------------------
! add a dimension to the netcdf file
!--------------------------------------------------------------------------------
  subroutine output_file_add_dimension(this, dim_name, dim_long_name, dim_size,  &
      units)

    !> Output file (already created)
    class(output_file_type), intent(inout) :: this
    !> Unique name for the dimension
    character(len=*), intent(in) :: dim_name
    !> Description of the dimension
    character(len=*), intent(in) :: dim_long_name
    !> Number of dimension elements
    integer, intent(in) :: dim_size
    !> Units for the dimension
    character(len=*), intent(in) :: units

    integer :: dimid, varid

    call check( nf90_def_dim( this%ncid, dim_name, dim_size, dimid ) )
    call check( nf90_def_var( this%ncid, dim_name, NF90_DOUBLE, (/ dimid /),  &
                              varid ) )
    call check( nf90_put_att( this%ncid, varid, "units", units ) )
    call check( nf90_put_att( this%ncid, varid, "longname", dim_long_name ) )

  end subroutine output_file_add_dimension

!--------------------------------------------------------------------------------
! set metadat for a collection of constituents
!--------------------------------------------------------------------------------
  subroutine output_file_add_cnsts( this, cnsts_props )
    class(output_file_type), intent(inout) :: this
    class(const_props_type), intent(in) :: cnsts_props(:)

    integer :: ncnst, n
    ncnst = size(cnsts_props)

    do n = 1,ncnst
       call this%output_file_add_var( cnsts_props(n)%get_name(), &
            trim(cnsts_props(n)%get_desc())//' volume mixing ratio', "molec/molec")
    end do
    
  end subroutine output_file_add_cnsts

!--------------------------------------------------------------------------------
! set metadat for a single variable
!--------------------------------------------------------------------------------
  subroutine output_file_add_var( this, varname, longname, varunits, dimnames )
    class(output_file_type), intent(inout) :: this
    character(len=*), intent(in) :: varname, longname, varunits
    !> Dimension names (defaults to dimensions of time)
    !! Dimension names must exist in netcdf file and if 'time' is included, it
    !! must be the last name)
    character(len=*), optional, intent(in) :: dimnames(:)

    integer :: i_dim
    integer, allocatable :: dimids(:)

    this%indx = this%indx + 1
    if (this%indx > MAXVARS) then
       print*,'More variables being output than allowed by MAXVARS in output_file.F90'
       stop
    end if

    if (present(dimnames)) then
      allocate(dimids(size(dimnames)))
      do i_dim = 1, size(dimnames)
        call check( nf90_inq_dimid(this%ncid, dimnames(i_dim), dimids(i_dim)) )
      end do
    else
      allocate(dimids(1))
      dimids(1) = this%rec_dimid
    end if

    call check( nf90_def_var(this%ncid, varname, NF90_DOUBLE, dimids, this%varid(this%indx)) )
    call check( nf90_put_att(this%ncid, this%varid(this%indx), "units", varunits) )
    call check( nf90_put_att(this%ncid, this%varid(this%indx), "longname", longname) )

    this%varname(this%indx) = varname
    deallocate(dimids)

  end subroutine output_file_add_var

!--------------------------------------------------------------------------------
! finalize the metadata in the netcdf file
!--------------------------------------------------------------------------------
  subroutine output_file_define( this )
    class(output_file_type), intent(in) :: this
    ! End define mode. This tells netCDF we are done defining metadata.
    call check( nf90_enddef(this%ncid) )
  end subroutine output_file_define

!--------------------------------------------------------------------------------
! advance in time
!--------------------------------------------------------------------------------
  subroutine output_file_adv(this, time)
    class(output_file_type), intent(inout) :: this
    real(kind_phys), intent(in) :: time

    this%rec_num = this%rec_num+1
    call check( nf90_put_var(this%ncid, this%time_varid, (/ time /), &
                start = (/ this%rec_num /), count = (/ 1 /)) )
    
  end subroutine output_file_adv

!--------------------------------------------------------------------------------
! set every element of a netcdf real variable
! \todo add overloaded subroutines for higher dimension variables
!--------------------------------------------------------------------------------
  subroutine set_variable(this, var_name, values)

    !> output file
    class(output_file_type), intent(inout) :: this
    !> netCDF variable name (must exist in output file)
    character(len=*), intent(in) :: var_name
    !> new values for variable
    !! (array size/shape must match that of netCDF vaiable)
    real(kind_phys), intent(in) :: values(:)

    integer :: var_id, ndims, dim_size
    integer :: dimids(NF90_MAX_VAR_DIMS)

    call check( nf90_inq_varid(this%ncid, var_name, var_id ) )
    call check( nf90_inquire_variable(this%ncid, var_id, ndims = ndims,          &
                                      dimids = dimids ) )
    call assert_msg(708577727, ndims.eq.1,                                       &
                    "NetCDF variable dimension mismatch for '"//var_name//       &
                    "' expected "//trim(to_char(ndims))//" but got 1" )
    call check( nf90_inquire_dimension(this%ncid, dimids(1), len = dim_size) )
    call assert_msg(134717026, dim_size.eq.size(values),                         &
                    "NetCDF variable size mismatch for '"//var_name//            &
                    "' expected "//trim(to_char(dimids(1)))//" but got "//       &
                    trim(to_char(size(values))) )
    call check( nf90_put_var(this%ncid, var_id, values, start = (/ 1 /),         &
                count = (/ size(values) /) ) )

  end subroutine set_variable

!--------------------------------------------------------------------------------
! output a collection of constituents VMR
!--------------------------------------------------------------------------------
  subroutine output_file_out_cnsts( this, cnsts_props, vmr )
    class(output_file_type), intent(inout) :: this
    class(const_props_type), intent(in) :: cnsts_props(:)
    real(kind_phys), intent(in) :: vmr(:)
    
    integer :: ncnst, n
    
    ncnst = size(cnsts_props)

    do n = 1,ncnst
       call this%output_file_out_var( cnsts_props(n)%get_name(), vmr(n) )
    end do
    
  end subroutine output_file_out_cnsts

!--------------------------------------------------------------------------------
! output a single variable
!--------------------------------------------------------------------------------
 subroutine output_file_out_var( this, varname, val )
    class(output_file_type), intent(inout) :: this
    character(len=*), intent(in) :: varname
    real(kind_phys), intent(in) :: val

    call check( nf90_put_var(this%ncid, this%varindx(varname), (/ val /), &
                start = (/ this%rec_num /), count = (/ 1 /)) )

  end subroutine output_file_out_var
  
!--------------------------------------------------------------------------------
! output a gridded variable
!--------------------------------------------------------------------------------
  subroutine output_file_out_grid_var( this, var_name, grid_indices, val )

    !> netcdf ouput file
    class(output_file_type), intent(inout) :: this
    !> netcdf variable name (must exist in output file)
    character(len=*), intent(in) :: var_name
    !> grid indices for the variable to set
    !! (must follow order used in set_var(), and the last index will be set
    !!  to the current time index)
    integer, intent(inout) :: grid_indices(:)
    !> variable value to set for the current time step
    real(kind_phys), intent(in) :: val

    grid_indices(size(grid_indices)) = this%rec_num
    call check( nf90_put_var(this%ncid, this%varindx(var_name), val,             &
                             grid_indices ) )

  end subroutine output_file_out_grid_var

!--------------------------------------------------------------------------------
! finaize the netcdf file
!--------------------------------------------------------------------------------
  subroutine output_file_close( this )
    class(output_file_type), intent(in) :: this
    ! Close the file. This frees up any internal netCDF resources
    ! associated with the file, and flushes any buffers.
    call check( nf90_close(this%ncid) )
  end subroutine output_file_close

!--------------------------------------------------------------------------------
! returns netcdf variable index for a given variable name
!--------------------------------------------------------------------------------
  integer function varindx(this, name)
    class(output_file_type), intent(inout) :: this
    character(len=*),intent(in) :: name
    integer :: i

    varindx = -1

    do i=1,MAXVARS
       if (name == this%varname(i)) then
          varindx = this%varid(i)
          return
       end if
    end do
  end function varindx

!--------------------------------------------------------------------------------
! private method
!--------------------------------------------------------------------------------
  subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then 
       print *, trim(nf90_strerror(status))
       stop "Stopped"
    end if
  end subroutine check

end module output_file
