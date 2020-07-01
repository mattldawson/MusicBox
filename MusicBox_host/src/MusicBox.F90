module MusicBox_main

!  use ccpp_kinds, only: r8 => kind_phys
use ccpp_kinds,             only: kind_phys
use read_envConditions,     only: read_envConditions_init, read_envConditions_timestep

use json_loader,            only: json_loader_read
use output_file,            only: output_file_type

! MusicBox host model data
use MusicBox_mod,           only: box_press, box_temp, relhum, box_h2o, photo_lev, nspecies, vmr, box_o2
use MusicBox_mod,           only: box_aer_sad, box_aer_diam, n_aer_modes
use MusicBox_mod,           only: nbox, ntimes
use MusicBox_mod,           only: nkRxt, njRxt, nRxn, TimeStart, TimeEnd
use MusicBox_mod,           only: nlayer, nlevel, zenith, albedo, press_mid, press_int
use MusicBox_mod,           only: alt, temp, o2vmrcol, o3vmrcol, so2vmrcol, no2vmrcol
use MusicBox_mod,           only: dt, density, mbar
use MusicBox_mod,           only: cnst_info
use MusicBox_mod,           only: jnames
use MusicBox_mod,           only: press_top
use MusicBox_mod,           only: cldfrc, cldwat
use MusicBox_mod,           only: reaction_names, reaction_rates, reaction_rate_constants

implicit none

public MusicBox_sub

  ! save a file of photolysis rate constants that can be used as input
  ! to the box model.
  logical, parameter :: OUTPUT_PHOTO_RATES = .false.

contains

  !> \section arg_table_MusicBox_sub  Argument Table
  !! \htmlinclude arg_table_MusicBox_sub.html
  !!
subroutine MusicBox_sub()

!-----------------------------------------------------------
! Main driver routine for MusicBox - The box model of MICM
!-----------------------------------------------------------

  use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_initialize
  use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_timestep_initial
  use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_run
  use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_timestep_final
  use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_finalize
  use MusicBox_ccpp_cap, only: ccpp_physics_suite_list
  use MusicBox_ccpp_cap, only: ccpp_physics_suite_part_list

  use tuv_photolysis,    only: tuv_photolysis_readnl

  implicit none

  integer                         :: col_start, col_end
  integer                         :: index
  character(len=128), allocatable :: part_names(:)
  character(len=128), allocatable :: rxn_names(:)
  character(len=512)              :: errmsg
  integer                         :: errflg = 0

  integer,parameter  :: nbox_param=1    ! Need to read this in from namelist and then allocate arrays
  
  integer            :: i,n,i_rxn
  real(kind=kind_phys), allocatable :: vmrboxes(:,:)   ! vmr for all boxes

  type(output_file_type) :: outfile, out_photo_file

  integer :: ibox
  real(kind_phys) :: sim_beg_time, sim_end_time
  integer :: box_grid_indices(4) = (/ 1, 1, 1, 0 /)  ! NetCDF output indices for grid cell (level, lon, lat, time)

  ! run-time options
  character(len=120) :: init_conds_file = 'NONE'
  character(len=120) :: env_conds_file = 'NONE'
  character(len=120) :: outfile_name = 'test_output.nc'

  ! These need to be plain reals for the reading routine
  real, parameter    :: NOT_SET = -huge(1.0)
  real               :: env_lat(nbox_param) = NOT_SET
  real               :: env_lon(nbox_param) = NOT_SET
  real               :: env_lev(nbox_param) = NOT_SET ! mbar
  real               :: user_begin_time = NOT_SET ! seconds
  real               :: user_end_time = NOT_SET
  real               :: user_dtime = NOT_SET
  
  character(len=*), parameter   :: photo_opts_file = '../Photolysis_options'
  character(len=*), parameter   :: nml_options = '../MusicBox_options'
  character(len=120), parameter :: jsonfile    = '../molec_info.json'

  ! read namelist run-time options
  namelist /options/ outfile_name, init_conds_file, env_conds_file
  namelist /options/ env_lat, env_lon, env_lev
  namelist /options/ user_begin_time, user_end_time, user_dtime

  nbox = nbox_param

  !---------------------------
  ! Read in the MusicBox_options file

  open(unit=10,file=nml_options)
  read(unit=10,nml=options)
  close(10)

  !---------------------------
  ! error checking of namelist settings

  if (any(env_lat(:)<-90.) .or.  any(env_lat(:) >90.)) then
     write(*,*) 'Invalid namelist setting: env_lat = ',env_lat(:)
     write(*,*) 'Must be set between -90 and 90 degrees north'
     stop
  end if
  if (any(env_lon(:)<0.) .or.  any(env_lon(:)>360.)) then
     write(*,*) 'Invalid namelist setting: env_lon = ',env_lon(:)
     write(*,*) 'Must be set between 0 and 360 degrees east'
     stop
  end if
  if (any(env_lev(:)<0)) then
      write(*,*) 'Invalid namelist setting: env_lev = ',env_lev(:)
     write(*,*) 'Must be set to a positive pressure level (hPa)'
    stop
  end if

  !---------------------------
  ! Read in the molecular information

  call json_loader_read( jsonfile, cnst_info, nSpecies, nkRxt, njRxt, jnames )
  nRxn = njRxt + nkRxt ! total number of reactions

  !---------------------------
  ! Initialize the envrionmental conditions

  allocate(vmrboxes(nSpecies,nbox))

  call read_envConditions_init(nbox, nSpecies, init_conds_file, env_conds_file, env_lat, env_lon, env_lev, user_begin_time, &
             user_end_time, user_dtime, cnst_info, vmrboxes, dt, sim_beg_time, sim_end_time, nlayer, photo_lev)

  !---------------------------
  ! Set up the photolysis module
  !! \todo Move photolysis to a CCPP scheme

  call tuv_photolysis_readnl(photo_opts_file, nbox, jnames, env_lat, env_lon, env_lev, errmsg, errflg)
  if (errflg /= 0) then
    write(6, *) trim(errmsg)
    stop
  end if

  !---------------------------
  ! Set up the various dimensions

  nlevel = nlayer+1 ! number if vertical interface levels

  !---------------------------
  ! allocate host model arrays

  allocate(vmr(nSpecies))

  allocate(alt(nlayer))
  allocate(press_mid(nlayer))
  allocate(press_int(nlevel))
  allocate(temp(nlayer))
  allocate(o2vmrcol(nlayer))
  allocate(o3vmrcol(nlayer))
  allocate(so2vmrcol(nlayer))
  allocate(no2vmrcol(nlayer))
  allocate(cldwat(nlayer))
  allocate(cldfrc(nlayer))
  allocate(box_aer_sad(n_aer_modes))
  allocate(box_aer_diam(n_aer_modes))
  allocate(reaction_rates(nRxn))
  allocate(reaction_rate_constants(nRxn))
  allocate(reaction_names(nRxn))

  !---------------------------
  ! Set the times (note this needs to be set prior to call ccpp_initialize)
  ! Once Rosenbrock_init is separated into init and time_step_init, this may go 
  ! down right above time_step_init

  TimeStart = sim_beg_time
  TimeEnd   = TimeStart + dt

  !---------------------------
  ! Use the suite information to setup the run
  call MusicBox_ccpp_physics_initialize('MusicBox_suite', errmsg, errflg)
  if (errflg /= 0) then
    write(6, *) trim(errmsg)
    stop
  end if

  !---------------------------
  ! Create the fields for the output netCDF file

  call outfile%create(outfile_name)
  call outfile%add(cnst_info)
  call outfile%add('Zenith','solar zenith angle','degrees')
  call outfile%add('Density','total number density','molecules/cm3')
  call outfile%add('Mbar','mean molar mass','g/mole')
  call outfile%add('RelHum','relative humidity','')
  do i_rxn = 1, nRxn
    call outfile%add(trim("rate_"//reaction_names(i_rxn)),trim('Rate for reaction '//reaction_names(i_rxn)),'molecules cm-3 s-1')
    call outfile%add(trim("rate_constant_"//reaction_names(i_rxn)),trim('Rate constant for reaction '//reaction_names(i_rxn)),'')
  end do
  call outfile%define() ! cannot add more fields after this call

  if (OUTPUT_PHOTO_RATES) then
    call out_photo_file%create('photolysis_rate_constants_out.nc')
    call out_photo_file%add_dimension('lat', 'latitude', size(env_lat), 'degree')
    call out_photo_file%add_dimension('lon', 'longitude', size(env_lon), 'degree')
    call out_photo_file%add_dimension('lev', 'hybrid level at midpoints', size(env_lev), 'hPa')
    call out_photo_file%add_dimension('ilev', 'hybrid level at interfaces', size(env_lev)+1, 'hPa')
    call out_photo_file%add('hyai', 'hybrid A coefficient at layer interfaces','', (/ 'ilev' /))
    call out_photo_file%add('hyam', 'hybrid A coefficient at layer midpoints','',  (/ 'lev'  /))
    call out_photo_file%add('hybi', 'hybrid B coefficient at layer interfaces','', (/ 'ilev' /))
    call out_photo_file%add('hybm', 'hybrid B coefficient at layer midpoints','',  (/ 'lev'  /))
    do i_rxn = 1, size(jnames)
      call out_photo_file%add("photo_rate_constant_"//trim(jnames(i_rxn)), "Photolysis rate constant for reaction "// &
                              trim(reaction_names(i_rxn)), "s-1", (/ 'lat ', 'lon ', 'lev ', 'time' /))
    end do
    call out_photo_file%define()
    call out_photo_file%set_variable('lat', real(env_lat, kind=kind_phys))
    call out_photo_file%set_variable('lon', real(env_lon, kind=kind_phys))
    call out_photo_file%set_variable('lev', real(env_lev, kind=kind_phys))
  end if

  !-----------------------------------------------------------
  !  loop over time
  !-----------------------------------------------------------

  time_loop:  do while (timestart <= sim_end_time)

     !---------------------------
     ! Initialize the timestep

     call MusicBox_ccpp_physics_timestep_initial('MusicBox_suite', errmsg, errflg)
     if (errflg /= 0) then
        write(6, *) trim(errmsg)
        stop
     end if

     !---------------------------
     ! set the timestep for the output file

     call outfile%advance(TimeStart)
     if (OUTPUT_PHOTO_RATES) call out_photo_file%advance(TimeStart)

     !---------------------------
     ! Loop over the boxes

     Box_loop: do ibox=1,nbox


        !---------------------------
        ! Read in the environmental conditions  at TimeStart
 
        call read_envConditions_timestep(TimeStart,ibox, nlayer, photo_lev, vmrboxes, zenith, albedo, &
             press_mid, press_int, alt,  temp, o2vmrcol, o3vmrcol, so2vmrcol, no2vmrcol, vmr, box_h2o, &
             box_temp, box_press, box_aer_sad, box_aer_diam, box_o2)

        cldwat = 0._kind_phys
        cldfrc = 0._kind_phys
        press_top = press_int(1)
        
        !---------------------------
        ! Call the schemes for the timestep
        col_start=1
        col_end=1

        call MusicBox_ccpp_physics_run('MusicBox_suite', 'physics', col_start, col_end, errmsg, errflg)
        if (errflg /= 0) then
           write(6, *) trim(errmsg)
           call ccpp_physics_suite_part_list('MusicBox_suite', part_names, errmsg, errflg)
           write(6, *) 'Available suite parts are:'
           do index = 1, size(part_names)
             write(6, *) trim(part_names(index))
           end do
           stop
        end if

        !---------------------------
        ! Update the environmental conditions for the timestep
        vmrboxes(:,ibox) = vmr(:)

        !---------------------------
        ! write out the timestep values

        call outfile%out( 'RelHum', relhum )
        call outfile%out( 'Zenith', zenith )

        write(*,'(a, e12.4, f12.2, f8.2)') ' total density, pressure, temperature :', density, box_press, box_temp
        write(*,'(a, 4e12.4)') ' aerosol surface area density (cm2/cm3):', box_aer_sad
        write(*,'(a, 4e12.4)') ' aerosol diameter (cm) :', box_aer_diam
        call outfile%out( 'Density', density )
        call outfile%out( 'Mbar', mbar )

        call outfile%out( cnst_info, vmrboxes(:,ibox) )
        write(*,'(a,1p,g0)') 'Concentration @ hour = ',TimeStart/3600.
        write(*,'(1p,5(1x,g0))') vmrboxes(:,ibox),sum(vmrboxes(:,ibox))

        do i_rxn = 1, nRxn
          call outfile%out( trim("rate_"//reaction_names(i_rxn)), reaction_rates(i_rxn) )
          call outfile%out( trim("rate_constant_"//reaction_names(i_rxn)), reaction_rate_constants(i_rxn) )
        end do

        if (OUTPUT_PHOTO_RATES) then
          !! \todo this only works if the tuv rates stay matched to reaction_rate_constants(1:njRxt)
          do i_rxn = 1, njRxt
            call out_photo_file%out( "photo_rate_constant_"//trim(jnames(i_rxn)), box_grid_indices, &
                                     reaction_rate_constants(i_rxn) )
          end do
        end if

      end do Box_loop

      !---------------------------
      ! Advance the timestep
      TimeStart = TimeEnd
      TimeEnd = TimeStart + dt

      !---------------------------
      ! call the timestep_final scheme routines
      call MusicBox_ccpp_physics_timestep_final('MusicBox_suite', errmsg, errflg)
      if (errflg /= 0) then
         write(6, *) trim(errmsg)
         write(6,'(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
         stop
      end if

  end do time_loop

  !---------------------------
  ! Finalize all of the schemes
  call MusicBox_ccpp_physics_finalize('MusicBox_suite', errmsg, errflg)
  if (errflg /= 0) then
     write(6, *) trim(errmsg)
     write(6,'(a)') 'An error occurred in ccpp_finalize, Exiting...'
     stop
  end if

  call outfile%close()

  if (OUTPUT_PHOTO_RATES) call out_photo_file%close()

  deallocate(vmr)
  deallocate(vmrboxes)
  deallocate(alt)
  deallocate(press_mid)
  deallocate(press_int)
  deallocate(temp)
  deallocate(o2vmrcol)
  deallocate(o3vmrcol)
  deallocate(so2vmrcol)
  deallocate(no2vmrcol)
  deallocate(cldwat)
  deallocate(cldfrc)
  deallocate(box_aer_sad)
  deallocate(box_aer_diam)

end subroutine MusicBox_sub

end module MusicBox_main

!> \brief Main SCM program that calls the main SCM subroutine
!!
!! The Doxygen documentation system cannot handle in-body comments in Fortran main programs, so the "main" program was put in the
!! subroutine \ref MusicBox_main_sub above.
program MusicBox
  use MusicBox_main
  call MusicBox_sub()  
end program MusicBox
