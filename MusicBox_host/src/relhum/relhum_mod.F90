module relhum_mod
  use ccpp_kinds, only: r8 => kind_phys
  use wv_saturation, only: wv_sat_init, wv_sat_final, qsat

contains

  subroutine relhum_mod_init
    call wv_sat_init()
  end subroutine relhum_mod_init

  subroutine relhum_mod_run( temp, press, h2ovmr, relhum )

    real(r8), intent(in) :: temp
    real(r8), intent(in) :: press
    real(r8), intent(in) :: h2ovmr
    real(r8), intent(out) :: relhum

    real(r8) :: satv
    real(r8) :: satq

    call qsat(temp, press, satv, satq)

    relhum = .622_r8 * h2ovmr / satq
    relhum = max( 0._r8,min( 1._r8, relhum ) )
  end subroutine relhum_mod_run

  subroutine relhum_mod_final
    call wv_sat_final()
  end subroutine relhum_mod_final

end module relhum_mod
