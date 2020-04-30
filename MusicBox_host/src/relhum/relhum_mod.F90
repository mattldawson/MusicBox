module relhum_mod
  use ccpp_kinds, only: kind_phys
  use wv_saturation, only: wv_sat_init, wv_sat_final, qsat

contains

!> \section arg_table_relhum_mod_init  Argument Table
!! \htmlinclude arg_table_relhum_mod_init.html
!!
  subroutine relhum_mod_init
    call wv_sat_init()
  end subroutine relhum_mod_init

!> \section arg_table_relhum_mod_run  Argument Table
!! \htmlinclude arg_table_relhum_mod_run.html
!!
  subroutine relhum_mod_run( temperature__K, pressure__Pa, H2O_number_density__num_m3, &
     number_density_air__num_m3,  relative_humidity__pct )

    real(kind_phys), intent(in) :: temperature__K
    real(kind_phys), intent(in) :: pressure__Pa
    real(kind_phys), intent(in) :: H2O_number_density__num_m3
    real(kind_phys), intent(in) :: number_density_air__num_m3
    real(kind_phys), intent(out) :: relative_humidity__pct

    real(kind_phys) :: satv
    real(kind_phys) :: satq

    call qsat(temperature__K, pressure__Pa, satv, satq)

    relative_humidity__pct = .622_kind_phys * H2O_number_density__num_m3 &
                              / number_density_air__num_m3 / satq
    relative_humidity__pct = max( 0._kind_phys,min( 1._kind_phys, relative_humidity__pct ) )
  end subroutine relhum_mod_run

!> \section arg_table_relhum_mod_final  Argument Table
!! \htmlinclude arg_table_relhum_mod_final.html
!!
  subroutine relhum_mod_final
    call wv_sat_final()
  end subroutine relhum_mod_final

end module relhum_mod
