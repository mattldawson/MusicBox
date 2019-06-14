module MusicBox_mod

   use ccpp_kinds, only: kind_phys

   implicit none
   public

   integer            :: ntimes_loop
   !> \section arg_table_MusicBox_mod  Argument Table
   !! \htmlinclude arg_table_MusicBox_host.html
   !!
   real(kind_phys)    :: temp_midpoints
   real(kind_phys)    :: temp_interfaces
   real(kind_phys)    :: box_press
   real(kind_phys)    :: box_temp
   real(kind_phys)    :: relhum
   real(kind_phys)    :: box_h2o
   real(kind_phys)    :: zenith
   real(kind_phys)    :: albedo
   real(kind_phys)    :: o3totcol
   real(kind_phys)    :: density, mbar
   real(kind_phys)    :: TimeStart, TimeEnd, dt

   integer            :: photo_lev
   integer            :: nspecies
   integer            :: nlevels
   integer            :: Musicpver, Musicpverp
   integer            :: nbox
   integer            :: nkRxt       ! number gas phase reactions
   integer            :: njRxt       ! number of photochemical reactions
   integer            :: ntimes      ! number of time steps
   integer            :: ntuvRates

   real(kind_phys), allocatable :: alt(:)
   real(kind_phys), allocatable :: temp(:)
   real(kind_phys), allocatable :: o2vmrcol(:)
   real(kind_phys), allocatable :: o3vmrcol(:)
   real(kind_phys), allocatable :: so2vmrcol(:)
   real(kind_phys), allocatable :: no2vmrcol(:)
   real(kind_phys), allocatable :: prates(:,:)
   real(kind_phys), allocatable :: press_mid(:)
   real(kind_phys), allocatable :: press_int(:)
   real(kind_phys), allocatable :: file_times(:)
   real(kind_phys), allocatable :: vmr(:)          ! "working" concentration passed thru CPF


   public :: init_temp
   public :: compare_temp

contains

   subroutine init_temp()

      temp_midpoints = 0.0_kind_phys
      temp_interfaces = 1._kind_phys

   end subroutine init_temp

   logical function compare_temp()

   end function compare_temp

end module MusicBox_mod
