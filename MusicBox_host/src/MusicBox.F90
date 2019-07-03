module MusicBox_main

!  use ccpp_kinds, only: r8 => kind_phys
  use ccpp_kinds, only: kind_phys

use json_loader,            only: json_loader_read
use environ_conditions_mod, only: environ_conditions_create, environ_conditions
use output_file,            only: output_file_type

! MusicBox host model data
use MusicBox_mod,           only: box_press, box_temp, relhum, box_h2o, photo_lev, nspecies, vmr
use MusicBox_mod,           only: Musicpver, Musicpverp, nbox, ntimes, ntuvRates
use MusicBox_mod,           only: nkRxt, njRxt, file_times, TimeStart, TimeEnd
use MusicBox_mod,           only: nlevels, nlevelsMinus1, zenith, albedo, press_mid, press_int
use MusicBox_mod,           only: alt, temp, o2vmrcol, o3vmrcol, so2vmrcol, no2vmrcol
use MusicBox_mod,           only: prates, dt, density, mbar
use MusicBox_mod,           only: cnst_info


implicit none

public MusicBox_sub

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

    implicit none

    integer                         :: col_start, col_end
    integer                         :: index
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg


  integer,parameter  :: nbox_param=1    ! Need to read this in from namelist and then allocate arrays
  
  integer            :: i,n
  real(kind=kind_phys), allocatable :: vmrboxes(:,:)   ! vmr for all boxes
  real(kind=kind_phys), allocatable :: wghts(:)

! declare the types
  type(environ_conditions),allocatable :: theEnvConds(:)
  type(environ_conditions),allocatable :: colEnvConds(:) 

  character(len=16)  :: cnst_name
  character(len=255) :: model_name

  type(output_file_type) :: outfile

  integer :: file_ntimes
  integer :: ibox
  real(kind_phys) :: sim_beg_time, sim_end_time

  ! run-time options
  character(len=120) :: env_conds_file = '../data/env_conditions.nc'
  character(len=120) :: outfile_name = 'test_output.nc'
  real, parameter :: NOT_SET = -huge(1.0)
  real :: env_lat(nbox_param) = NOT_SET
  real :: env_lon(nbox_param) = NOT_SET
  real :: env_lev(nbox_param) = NOT_SET ! mbar
  real :: user_begin_time = NOT_SET ! seconds
  real :: user_end_time = NOT_SET
  real :: user_dtime = NOT_SET
  
  character(len=*), parameter   :: nml_options = '../MusicBox_options'
  character(len=120), parameter :: jsonfile    = '../molec_info.json'

  ! read namelist run-time options
  namelist /options/ outfile_name, env_conds_file
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

  call json_loader_read( jsonfile, cnst_info, nSpecies, nkRxt, njRxt )
    
  !---------------------------
  ! Create the fields for the output netCDF file

  call outfile%create(outfile_name)
  call outfile%add(cnst_info)
  call outfile%add('Zenith','solar zenith angle','degrees')
  call outfile%add('Density','total number density','molecules/cm3')
  call outfile%add('Mbar','mean molar mass','g/mole')
  call outfile%add('RelHum','relative humidity','')
  call outfile%define() ! cannot add more fields after this call
  
  !---------------------------
  ! allocate host model arrays

  allocate(vmrboxes(nSpecies,nbox))
  allocate(vmr(nSpecies))
  allocate(theEnvConds(nbox))
  allocate(colEnvConds(nbox))

  !---------------------------
  ! Read in the first time slice of data.  
  ! "theEnvConds" contains the time slice at the requested lat/lon/level
  ! "colEnvConds" contains the column time slice at the requested lat/lon

  do ibox=1,nbox
     theEnvConds(ibox) = environ_conditions_create( env_conds_file, lat=env_lat(ibox), lon=env_lon(ibox), lev=env_lev(ibox) )
     colEnvConds(ibox)= environ_conditions_create( env_conds_file, lat=env_lat(ibox), lon=env_lon(ibox) )
  end do

  !---------------------------
  ! Assign the times based on the user provided namelist values or use the
  ! values provided in the environmental conditions file

  if (user_dtime>0.) then
     dt = user_dtime
  else
     dt = theEnvConds(1)%dtime()
  end if

  if (user_begin_time == NOT_SET .or. user_end_time == NOT_SET) then
     file_ntimes= theEnvConds(1)%ntimes()
     file_times = theEnvConds(1)%get_times()
  end if
  if (user_begin_time /= NOT_SET) then
     sim_beg_time = user_begin_time
  else
     sim_beg_time = file_times(1)
  end if
  if (user_end_time /= NOT_SET) then
     sim_end_time = user_end_time
  else
     sim_end_time = file_times(file_ntimes)
  end if

  !---------------------------
  ! Set up the various dimensions and allocate arrays accordingly

  nlevels = colEnvConds(1)%nlevels()
  nlevelsMinus1 = nlevels - 1
  Musicpver     = nlevels
  Musicpverp    = nlevels +1
  photo_lev     = theEnvConds(1)%levnum()
  ntuvRates     = 113

  allocate(alt(nlevels))
  allocate(press_mid(Musicpver))
  allocate(press_int(Musicpverp))
  allocate(temp(Musicpver))
  allocate(o2vmrcol(Musicpver))
  allocate(o3vmrcol(Musicpver))
  allocate(so2vmrcol(Musicpver))
  allocate(no2vmrcol(Musicpver))
  allocate(prates(Musicpver,ntuvRates))

  allocate(wghts(nSpecies))
  wghts(:) = 1._kind_phys

  !---------------------------
  ! Retrieve the vmr for all species and boxes
  do n = 1,nSpecies
     do ibox=1,nbox
        call cnst_info(n)%print()
        cnst_name = cnst_info(n)%get_name()
        vmrboxes(n,ibox) = theEnvConds(ibox)%getvar(cnst_name,default_value=0.00_kind_phys)

        write(*,fmt="(' cnst name : ',a20,' init value : ',e13.6)") cnst_name, vmrboxes(n,ibox)
        if (cnst_name == 'CL2') then
           wghts(n) = 2._kind_phys
        end if
     enddo
  enddo

  !---------------------------
  ! Set the times (note this needs to be set prior to call ccpp_initialize)
  ! Once Rosenbrock_init is separated into init and time_step_init, this may go 
  ! down right above time_step_init

  TimeStart = sim_beg_time
  TimeEnd = TimeStart + dt

  !---------------------------
  ! Use the suite information to setup the run
  call MusicBox_ccpp_physics_initialize('MusicBox_suite', errmsg, errflg)
  if (errflg /= 0) then
    write(6, *) trim(errmsg)
    stop
  end if

! For testing short runs   
!   ntimes = 10


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

    !---------------------------
    ! Loop over the boxes

    Box_loop: do ibox=1,nbox

       !---------------------------
       ! read environmental conditions for the next time step

       call colEnvConds(ibox)%update(TimeStart)
       call theEnvConds(ibox)%update(TimeStart)

       !---------------------------
       ! Read in the species information

       zenith              = colEnvConds(ibox)%getsrf('SZA')
       albedo              = colEnvConds(ibox)%getsrf('ASDIR')
       press_mid(:nlevels) = colEnvConds(ibox)%press_mid(nlevels)
       press_int(:nlevels) = colEnvConds(ibox)%press_int(nlevels)
       alt(:nlevels)       = colEnvConds(ibox)%getcol('Z3',nlevels)
       temp(:nlevels)      = colEnvConds(ibox)%getcol('T',nlevels)
       o2vmrcol(:nlevels)  = colEnvConds(ibox)%getcol('O2',nlevels)
       o3vmrcol(:nlevels)  = colEnvConds(ibox)%getcol('O3',nlevels)
       so2vmrcol(:nlevels) = colEnvConds(ibox)%getcol('SO2',nlevels)
       no2vmrcol(:nlevels) = colEnvConds(ibox)%getcol('NO2',nlevels)
       vmr(:)    = vmrboxes(:,ibox)
       box_h2o   = theEnvConds(ibox)%getvar('H2O')
       box_temp  = temp(photo_lev)
       box_press = press_mid(photo_lev)

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
       ! write out the timestep values

       call outfile%out( 'RelHum', relhum )
       call outfile%out( 'Zenith', zenith )

       vmrboxes(:,ibox) = vmr(:)
       write(*,'(a, e12.4, f6.2, f6.2)') ' total density, pressure, temperature :', density, box_press, box_temp
       call outfile%out( 'Density', density )
       call outfile%out( 'Mbar', mbar )
       call outfile%out( cnst_info, vmrboxes(:,ibox) )
       write(*,'(a,1p,g0)') 'Concentration @ hour = ',TimeStart/3600.
       write(*,'(1p,5(1x,g0))') vmrboxes(:,ibox),sum(vmrboxes(:,ibox))

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
  if (allocated(prates)) deallocate(prates)
  if (allocated(wghts)) deallocate(wghts)
  if (allocated(file_times)) deallocate(file_times)


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
