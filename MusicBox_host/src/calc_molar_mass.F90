!--------------------------------------------------------------------------------
! utility to compute background air mass quantities
!--------------------------------------------------------------------------------
module calc_molar_mass
!  USE ccpp_kinds, ONLY: rk => kind_phys
  USE ccpp_kinds, ONLY: kind_phys
  use const_props_mod, only : const_props_type
  implicit none
  
  integer :: o2_ndx, n2_ndx
  real(kind_phys), allocatable :: molar_mass(:)
  
contains

!> \section arg_table_calc_molar_mass_init Argument Table
!! \htmlinclude calc_molar_mass_init.html
!!
  subroutine calc_molar_mass_init(cnst_info, ncnst, errmsg, errflg)
    type(const_props_type), intent(in)  :: cnst_info(:)
    integer,                intent(in)  :: ncnst
    character(len=512),     intent(out) :: errmsg
    integer,                intent(out) :: errflg

    integer :: i

    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

!    o2_ndx =-1
!    n2_ndx =-1

    allocate(molar_mass(ncnst))

    do i = 1,ncnst
       molar_mass(i) = cnst_info(i)%get_wght()
!       if ( cnst_info(i)%get_name() == 'O2' ) o2_ndx = i
!       if ( cnst_info(i)%get_name() == 'N2' ) n2_ndx = i
    end do

  end subroutine calc_molar_mass_init

!> \section arg_table_calc_molar_mass_run Argument Table
!! \htmlinclude calc_molar_mass_run.html
!!
  subroutine calc_molar_mass_run( errmsg, errflg )

    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

   
    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    
  end subroutine calc_molar_mass_run

!> \section arg_table_calc_molar_mass_finalize Argument Table
!! \htmlinclude calc_molar_mass_finalize.html
!!
  subroutine calc_molar_mass_finalize( errmsg, errflg )

    !--- arguments
    character(len=512), intent(out) :: errmsg
    integer,            intent(out) :: errflg

    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    deallocate(molar_mass)
    
  end subroutine calc_molar_mass_finalize
  
end module calc_molar_mass
