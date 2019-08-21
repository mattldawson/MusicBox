!--------------------------------------------------------------------------------
! utility to calculate the density
!--------------------------------------------------------------------------------
module calc_density
!  USE ccpp_kinds, ONLY: rk => kind_phys
  USE ccpp_kinds, ONLY: kind_phys
  implicit none
  
contains

!> \section arg_table_calc_density_run Argument Table
!! \htmlinclude calc_density_run.html
!!
  subroutine calc_density_run( press, temperature, density, errmsg, errflg )

    real(kind_phys), intent(in)            :: press
    real(kind_phys), intent(in)            :: temperature
    real(kind_phys), intent(out)           :: density
    character(len=512), intent(out) :: errmsg
    integer, intent(out)            :: errflg

    real(kind_phys), parameter :: kboltz= 1.38064852e-16_kind_phys ! boltzmann constant (erg/K)
   
    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    density = 10._kind_phys*press/(kboltz*temperature)
    
  end subroutine calc_density_run
  
end module calc_density
