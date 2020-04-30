module calc_mbar
!  USE ccpp_kinds, ONLY: rk => kind_phys
  USE ccpp_kinds, ONLY: kind_phys
  use const_props_mod, only : const_props_type
  implicit none

  !> Molecular weight [g/mol]
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
  subroutine calc_mbar_run( gas_number_density__num_m3, mean_molecular_mass__g_mol, errmsg, errflg )

    !> Gas species number density [#/m3]
    real(kind_phys), intent(in)     :: gas_number_density__num_m3(:)
    !> Mean molecular mass [g/mol]
    real(kind_phys), intent(out)    :: mean_molecular_mass__g_mol
    !> Error message
    character(len=512), intent(out) :: errmsg
    !> Error flag
    integer, intent(out)            :: errflg

    integer :: total_number_density

    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    total_number_density = sum( gas_number_density__num_m3(:) )
    mean_molecular_mass__g_mol = sum( gas_number_density__num_m3(:)*molar_mass(:) ) / total_number_density

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
