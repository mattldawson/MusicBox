!> \file
!> The music_box_constants module

!> Common physical constants
module music_box_constants

  use ccpp_kinds,                      only : kind_phys

  implicit none
  public

  !> Pi
  real(kind=kind_phys) :: PI = 3.14159265358979323846d0
  !> Avagadro's number
  real(kind=kind_phys) :: AVAGADRO = 6.02214179d23

end module music_box_constants
