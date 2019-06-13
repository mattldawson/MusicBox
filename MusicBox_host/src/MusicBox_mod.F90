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
