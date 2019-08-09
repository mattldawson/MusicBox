module calc_mbar
!  USE ccpp_kinds, ONLY: rk => kind_phys
  USE ccpp_kinds, ONLY: kind_phys
  use const_props_mod, only : const_props_type
  implicit none
  
  integer :: o2_ndx, n2_ndx
  real(kind_phys), allocatable :: molar_mass(:)
  
contains

!> \section arg_table_calc_mbar_init Argument Table
!! \htmlinclude calc_mbar_init.html
!!
  subroutine calc_mbar_init(cnst_info, ncnst, errmsg, errflg)
    type(const_props_type), intent(in)  :: cnst_info(:)
    integer,                intent(in)  :: ncnst
    character(len=512),     intent(out) :: errmsg
    integer,                intent(out) :: errflg

    integer :: i

    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    allocate(molar_mass(ncnst))

    do i = 1,ncnst
       molar_mass(i) = cnst_info(i)%get_wght()
    end do


  end subroutine calc_mbar_init

!> \section arg_table_calc_mbar_run Argument Table
!! \htmlinclude calc_mbar_run.html
!!
  subroutine calc_mbar_run( vmr, mbar, errmsg, errflg )

    real(kind_phys), intent(inout)         :: vmr(:)
    real(kind_phys), intent(out)           :: mbar
    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

     mbar = sum( vmr(:)*molar_mass(:) )
    
  end subroutine calc_mbar_run

!> \section arg_table_calc_mbar_finalize Argument Table
!! \htmlinclude calc_mbar_finalize.html
!!
  subroutine calc_mbar_finalize( errmsg, errflg )

    !--- arguments
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errflg

    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    deallocate(molar_mass)
    
  end subroutine calc_mbar_finalize
  
end module calc_mbar
