module hello_world_mod

   use ccpp_kinds, only: kind_phys

   implicit none
   public

   integer            :: ntimes_loop
   !> \section arg_table_hello_world_mod  Argument Table
   !! \htmlinclude arg_table_hello_world_host.html
   !!
   real(kind_phys)    :: temp_midpoints
   real(kind_phys)    :: temp_interfaces
   real(kind_phys)    :: box_press

   public :: init_temp
   public :: compare_temp

contains

   subroutine init_temp()

      temp_midpoints = 0.0_kind_phys
      temp_interfaces = 1._kind_phys

   end subroutine init_temp

   logical function compare_temp()

   end function compare_temp

end module hello_world_mod
