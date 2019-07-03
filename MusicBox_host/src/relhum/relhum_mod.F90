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
  subroutine relhum_mod_run( boxtemp, boxpress, boxh2ovmr, boxrelhum )

    real(kind_phys), intent(in) :: boxtemp
    real(kind_phys), intent(in) :: boxpress
    real(kind_phys), intent(in) :: boxh2ovmr
    real(kind_phys), intent(out) :: boxrelhum

    real(kind_phys) :: satv
    real(kind_phys) :: satq

    call qsat(boxtemp, boxpress, satv, satq)

    boxrelhum = .622_kind_phys * boxh2ovmr / satq
    boxrelhum = max( 0._kind_phys,min( 1._kind_phys, boxrelhum ) )
  end subroutine relhum_mod_run

!> \section arg_table_relhum_mod_final  Argument Table
!! \htmlinclude arg_table_relhum_mod_final.html
!!
  subroutine relhum_mod_final
    call wv_sat_final()
  end subroutine relhum_mod_final

end module relhum_mod
