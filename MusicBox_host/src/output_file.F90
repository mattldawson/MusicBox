!----------------------------------------------------------------------------------------------
! for writing time series output to netcdf
!----------------------------------------------------------------------------------------------
module output_file
!  USE ccpp_kinds, ONLY: rk => kind_phys
  USE ccpp_kinds, ONLY: kind_phys

  use netcdf, only: nf90_create, NF90_CLOBBER, nf90_def_dim, NF90_UNLIMITED, nf90_def_var
  use netcdf, only: nf90_close, nf90_put_var, nf90_enddef, NF90_DOUBLE, nf90_put_att
  use netcdf, only: nf90_noerr, nf90_strerror
  
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
     procedure :: output_file_add_cnsts
     procedure :: output_file_add_var
     generic   :: add => output_file_add_cnsts, output_file_add_var
     procedure :: advance => output_file_adv
     procedure :: output_file_out_cnsts
     procedure :: output_file_out_var
     generic   :: out => output_file_out_cnsts, output_file_out_var
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
! set metadat for a collection of constituents
!--------------------------------------------------------------------------------
  subroutine output_file_add_cnsts( this, cnsts_props )
    class(output_file_type), intent(inout) :: this
    class(const_props_type), intent(in) :: cnsts_props(:)

    integer :: ncnst, n
    ncnst = size(cnsts_props)

    do n = 1,ncnst
       call this%output_file_add_var( cnsts_props(n)%get_name(), &
            trim(cnsts_props(n)%get_desc())//' number density', "molecules/m3")
    end do

  end subroutine output_file_add_cnsts

!--------------------------------------------------------------------------------
! set metadat for a single variable
!--------------------------------------------------------------------------------
  subroutine output_file_add_var( this, varname, longname, varunits )
    class(output_file_type), intent(inout) :: this
    character(len=*), intent(in) :: varname, longname, varunits

    this%indx = this%indx + 1
    if (this%indx > MAXVARS) then
       print*,'More variables being output than allowed by MAXVARS in output_file.F90'
       stop
    end if
    
    call check( nf90_def_var(this%ncid, varname, NF90_DOUBLE, (/ this%rec_dimid /), this%varid(this%indx)) )
    call check( nf90_put_att(this%ncid, this%varid(this%indx), "units", varunits) )
    call check( nf90_put_att(this%ncid, this%varid(this%indx), "longname", longname) )
    
    this%varname(this%indx) = varname

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
! output a collection of constituents number density [#/m3]
!--------------------------------------------------------------------------------
  subroutine output_file_out_cnsts( this, cnsts_props, gas_number_density__num_m3 )
    class(output_file_type), intent(inout) :: this
    class(const_props_type), intent(in) :: cnsts_props(:)
    real(kind_phys), intent(in) :: gas_number_density__num_m3(:)

    integer :: ncnst, n

    ncnst = size(cnsts_props)

    do n = 1,ncnst
       call this%output_file_out_var( cnsts_props(n)%get_name(), &
                                      gas_number_density__num_m3(n) )
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
