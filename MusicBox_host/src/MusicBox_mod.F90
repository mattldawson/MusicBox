module MusicBox_mod

   use ccpp_kinds,        only: kind_phys
   use const_props_mod,   only: const_props_type

   implicit none
   public

   !> \section arg_table_MusicBox_mod  Argument Table
   !! \htmlinclude arg_table_MusicBox_host.html
   !!
   real(kind_phys)    :: box_press
   real(kind_phys)    :: box_temp
   real(kind_phys)    :: relhum
   real(kind_phys)    :: box_h2o
   real(kind_phys)    :: box_o2
   real(kind_phys)    :: zenith
   real(kind_phys)    :: albedo
   real(kind_phys)    :: number_density_air__num_m3
   real(kind_phys)    :: mbar
   real(kind_phys)    :: TimeStart, TimeEnd, dt
   real(kind_phys)    :: press_top

   integer,parameter  :: realkind = kind_phys
   integer            :: photo_lev
   integer            :: nspecies
   integer            :: nlayer
   integer            :: nlevel
   integer            :: nbox
   integer            :: nkRxt       ! number gas phase reactions
   integer            :: njRxt       ! number of photochemical reactions
   integer            :: nRxn        ! number of reactions (total)
   integer            :: ntimes      ! number of time steps
   integer            :: ntuvRates
   integer, parameter :: n_aer_modes = 4 ! assume 4 aerosol modes for now

   real(kind_phys), allocatable :: alt(:)
   real(kind_phys), allocatable :: temp(:)
   real(kind_phys), allocatable :: O2_number_density_column__num_m3(:)
   real(kind_phys), allocatable :: O3_number_density_column__num_m3(:)
   real(kind_phys), allocatable :: SO2_number_density_column__num_m3(:)
   real(kind_phys), allocatable :: NO2_number_density_column__num_m3(:)
   real(kind_phys), allocatable :: cldfrc(:)
   real(kind_phys), allocatable :: cldwat(:)
   real(kind_phys), allocatable :: press_mid(:)
   real(kind_phys), allocatable :: press_int(:)
   real(kind_phys), allocatable :: gas_number_density__num_m3(:) ! "working" concentration passed thru CPF
   real(kind_phys), allocatable :: reaction_rates(:)
   real(kind_phys), allocatable :: reaction_rate_constants(:)
   real(kind_phys), allocatable :: box_aer_sad(:)
   real(kind_phys), allocatable :: box_aer_diam(:)

   type(const_props_type), allocatable :: cnst_info(:)

   character(len=16 ), allocatable :: jnames(:)
   character(len=128), allocatable :: reaction_names(:)
   character(len=*), parameter     :: options_filepath = "../../MICM_chemistry/src/solver_options.nml"

   logical :: print_log_message = .false.

end module MusicBox_mod
