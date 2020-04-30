!--------------------------------------------------------------------------------
! utility to calculate the density
!--------------------------------------------------------------------------------
module calc_density

  USE ccpp_kinds, ONLY: kind_phys
  implicit none

contains

!> \section arg_table_calc_density_run Argument Table
!! \htmlinclude calc_density_run.html
!!
  subroutine calc_density_run( pressure__Pa, temperature__K, number_density_air__num_m3, errmsg, errflg )

    !> Pressure [Pa]
    real(kind_phys), intent(in)     :: pressure__Pa
    !> Temperature [K]
    real(kind_phys), intent(in)     :: temperature__K
    !> Total number density of air [#/m3]
    real(kind_phys), intent(out)    :: number_density_air__num_m3
    !> Error message
    character(len=512), intent(out) :: errmsg
    !> Error flag
    integer, intent(out)            :: errflg

    real(kind_phys), parameter :: kboltz= 1.38064852e-23_kind_phys ! boltzmann constant (J/K)

    !--- initialize CCPP error handling variables
    errmsg = ''
    errflg = 0

    ! #/m3 = Pa / (J/K * K)
    ! Pa = J/m3
    number_density_air__num_m3 = pressure__Pa / ( kboltz * temperature__K )

  end subroutine calc_density_run

end module calc_density
