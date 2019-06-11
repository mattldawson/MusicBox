module MusicBox_main

!  use ccpp_kinds, only: r8 => kind_phys
  use ccpp_kinds, only: kind_phys

use const_props_mod,        only: const_props_type
use json_loader,            only: json_loader_read
use environ_conditions_mod, only: environ_conditions_create, environ_conditions
!!!! use prepare_chemistry_mod,  only: prepare_chemistry_init
use output_file,            only: output_file_type
use relhum_mod,             only: relhum_mod_init, relhum_mod_run, relhum_mod_final

implicit none

public MusicBox_sub

contains

  !> \section arg_table_MusicBox_sub  Argument Table
  !! \htmlinclude arg_table_MusicBox_sub.html
  !!
subroutine MusicBox_sub()

    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_initialize
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_timestep_initial
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_run
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_timestep_final
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_finalize
    use MusicBox_ccpp_cap, only: ccpp_physics_suite_list
    use MusicBox_ccpp_cap, only: ccpp_physics_suite_part_list

  use :: iso_c_binding, only: c_loc

  implicit none

    integer                         :: col_start, col_end
    integer                         :: index
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg

!-----------------------------------------------------------
!  these dimension parameters will be set by the cafe/configurator
!-----------------------------------------------------------
  integer :: nSpecies = 0   ! number prognostic constituents
  integer :: nkRxt = 0      ! number gas phase reactions
  integer :: njRxt = 0      ! number of photochemical reactions
  integer :: ntimes = 0     ! number of time steps

  integer ,parameter :: ncols = 1 ! number columns in domain
  integer,parameter  :: nbox=1    ! Need to read this in from namelist and then allocate arrays

  
  integer            :: i,n
  integer            :: ierr
  real(kind=kind_phys), allocatable :: j_rateConst(:)  ! host model provides photolysis rates for now 
  real(kind=kind_phys), allocatable :: k_rateConst(:)  ! host model provides photolysis rates for now
  real(kind=kind_phys), allocatable :: vmr(:)          ! "working" concentration passed thru CPF
  real(kind=kind_phys), allocatable :: vmrboxes(:,:)   ! vmr for all boxes
  real(kind=kind_phys), allocatable :: wghts(:)

  real(kind_phys) :: TimeStart, TimeEnd, Time, dt
  

!  type :: environ_conditions_array_type
!     type(environ_conditions) :: EnvCond
!  end type

! declare the types
  type(environ_conditions),allocatable :: theEnvConds(:)
  type(environ_conditions),allocatable :: colEnvConds(:) 
  type(const_props_type), allocatable :: cnst_info(:)

  character(len=16)  :: cnst_name
  character(len=255) :: model_name

  type(output_file_type) :: outfile

  integer :: photo_lev
  integer :: nlevels
  real(kind_phys) :: zenith
  real(kind_phys) :: albedo
  real(kind_phys) :: o3totcol
  real(kind_phys), allocatable :: alt(:)
  real(kind_phys), allocatable :: press_mid(:)
  real(kind_phys), allocatable :: press_int(:)
  real(kind_phys), allocatable :: temp(:)
  real(kind_phys), allocatable :: o2vmrcol(:)
  real(kind_phys), allocatable :: o3vmrcol(:)
  real(kind_phys), allocatable :: so2vmrcol(:)
  real(kind_phys), allocatable :: no2vmrcol(:)
  real(kind_phys), allocatable :: prates(:,:)
  real(kind_phys), allocatable :: file_times(:)
  real(kind_phys) :: relhum ! relative humidity
  real(kind_phys) :: density, mbar, box_temp, box_press, box_h2o
  integer :: file_ntimes
  integer :: ibox
  real(kind_phys) :: sim_beg_time, sim_end_time

  ! run-time options
  character(len=120) :: env_conds_file = '../data/env_conditions.nc'
  character(len=120) :: outfile_name = 'test_output.nc'
  real, parameter :: NOT_SET = -huge(1.0)
  real :: env_lat(nbox) = NOT_SET
  real :: env_lon(nbox) = NOT_SET
  real :: env_lev(nbox) = NOT_SET ! mbar
  real :: user_begin_time = NOT_SET ! seconds
  real :: user_end_time = NOT_SET
  real :: user_dtime = NOT_SET
  
  character(len=*), parameter :: nml_options = '../MusicBox_options'
  character(len=120) :: jsonfile

  ! read namelist run-time options
  namelist /options/ outfile_name, env_conds_file
  namelist /options/ env_lat, env_lon, env_lev
  namelist /options/ user_begin_time, user_end_time, user_dtime
  
  open(unit=10,file=nml_options)
  read(unit=10,nml=options)
  close(10)

  ! error checking
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


    ! Use the suite information to setup the run
    call MusicBox_ccpp_physics_initialize('MusicBox_suite', ntimes, file_times, box_press, box_temp,       &
        nSpecies, vmr, relhum, box_h2o,photo_lev,TimeStart,TimeEnd,njRxt,errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

! Remove this call when the CPF can allocate arrays 
! NOTE - It is called again in chemistry_driver_init which is where it will
! permamently reside
!!!!!!!  call prepare_chemistry_init(cnst_info, model_name, nSpecies, nkRxt, njRxt)

  model_name = 'Chapman_v3_1547831703456'

  jsonfile = '../../../MICM_chemistry/generated/'//trim(model_name)//'/molec_info.json'
  call json_loader_read( jsonfile, cnst_info, nSpecies, nkRxt, njRxt )


    
  write(*,*) '*******************************************************'
  write(*,*) '************** model = '//trim(model_name)//' ***************'
  write(*,*) '*******************************************************'
  
  call outfile%create(outfile_name)
  call outfile%add(cnst_info)
  call outfile%add('Zenith','solar zenith angle','degrees')
  call outfile%add('O3totcol','integrated ozone column (dobson units)','DU')
  call outfile%add('Density','total number density','molecules/cm3')
  call outfile%add('Mbar','mean molar mass','g/mole')
  if (model_name == 'terminator') then
     call outfile%add('JCL2','Cl2 photolysis rate','sec^-1')
     call outfile%add('CL_TOT','Total Chlorine','molec/molec')
  end if
  if (model_name == '3component') then
     call outfile%add('VMRTOT','sum of all species','molec/molec')
  end if
  call outfile%add('RelHum','relative humidity','')

  call outfile%define() ! cannot add more fields after this call
  
!----------------------------------------
! These allocates will go away once the CPF is able to allocate arrays

  allocate(k_rateConst(nkRxt))
  allocate(j_rateConst(njRxt))
  
  allocate(vmrboxes(nSpecies,nbox))
  allocate(vmr(nSpecies))
  allocate(theEnvConds(nbox))
  allocate(colEnvConds(nbox))
!----------------------------------------

  do ibox=1,nbox
     theEnvConds(ibox) = environ_conditions_create( env_conds_file, lat=env_lat(ibox), lon=env_lon(ibox), lev=env_lev(ibox) )
  end do
  if (user_dtime>0.) then
     dt = user_dtime
  else
     dt = theEnvConds(1)%dtime()
  end if

  if (user_begin_time == NOT_SET .or. user_end_time == NOT_SET) then
     file_ntimes= theEnvConds(1)%ntimes()
!     allocate(file_times(file_ntimes))
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

!  ntimes = 1+int((sim_end_time-sim_beg_time)/dt)
  ntimes = 10

  do ibox=1,nbox
     colEnvConds(ibox)= environ_conditions_create( env_conds_file, lat=env_lat(ibox), lon=env_lon(ibox) )
  end do
  nlevels = colEnvConds(1)%nlevels()
  photo_lev = theEnvConds(1)%levnum()

  allocate(alt(nlevels))
  allocate(press_mid(nlevels))
  allocate(press_int(nlevels))
  allocate(temp(nlevels))
  allocate(o2vmrcol(nlevels))
  allocate(o3vmrcol(nlevels))
  allocate(so2vmrcol(nlevels))
  allocate(no2vmrcol(nlevels))
  ntuvRates=113
  allocate(prates(nlevels,ntuvRates))

  if (model_name == 'terminator') then
     allocate(wghts(nSpecies))
     wghts(:) = 1._kind_phys
  endif

  do n = 1,nSpecies
  do ibox=1,nbox
     call cnst_info(n)%print()
     cnst_name = cnst_info(n)%get_name()
     if (cnst_name == 'N2') then
        vmrboxes(n,ibox) = theEnvConds(ibox)%getvar(cnst_name,default_value=0.79_kind_phys)
     else if (cnst_name == 'O2') then
        vmrboxes(n,ibox) = theEnvConds(ibox)%getvar(cnst_name,default_value=0.21_kind_phys)
     else
        vmrboxes(n,ibox) = theEnvConds(ibox)%getvar(cnst_name,default_value=0.00_kind_phys)
     end if

     write(*,fmt="(' cnst name : ',a20,' init value : ',e13.6)") cnst_name, vmrboxes(n,ibox)
     if (allocated(wghts) .and. cnst_name == 'CL2') then
        wghts(n) = 2._kind_phys
     end if
  enddo
  enddo


    ! Initialize the timestep
    call MusicBox_ccpp_physics_timestep_initial('MusicBox_suite', ntimes, file_times, box_press, box_temp,       &
        nSpecies, vmr, relhum, box_h2o, photo_lev,TimeStart,TimeEnd,njRxt,errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

  TimeStart = 0._kind_phys
  TimeEnd = TimeStart + dt
  
!init_loop: & ! ccpp requires a loop over columns
!  do i = 1, ncols
!      call ccpp_init( '../suites/'//trim(model_name)//'.xml', cdata(i), ierr)
!
!      if (ierr/=0) then
!          write(*,'(a,i0,a)') 'An error occurred in ccpp_init for column ', i, '. Exiting...'
!          stop
!      end if
!
! ! use ccpp_fields.inc to call ccpp_field_add for all variables to be exposed to CCPP (this is
! ! auto-generated from /src/ccpp/scripts/ccpp_prebuild.py - the script parses tables in MusicBox_type_defs.f90)
! ! this requires column index i
!#  include "ccpp_fields.inc"
!
!      !initialize each column's physics
!      call ccpp_physics_init(cdata(i), ierr=ierr)
!      if (ierr/=0) then
!          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_init for column ', i, '. Exiting...'
!          stop
!      end if
!  end do init_loop

  call relhum_mod_init()

!-----------------------------------------------------------
!  loop over time
!-----------------------------------------------------------
time_loop: &
  do ibox=1,nbox
  do n = 1, ntimes
    call outfile%advance(TimeStart)
    call colEnvConds(ibox)%update(TimeStart)
    TimeEnd = TimeStart + dt
    do i = 1, ncols
       zenith = colEnvConds(ibox)%getsrf('SZA')
       albedo = colEnvConds(ibox)%getsrf('ASDIR')
       press_mid(:nlevels) = colEnvConds(ibox)%press_mid(nlevels)
       press_int(:nlevels) = colEnvConds(ibox)%press_int(nlevels)
       alt(:nlevels) = colEnvConds(ibox)%getcol('Z3',nlevels)
       temp(:nlevels) = colEnvConds(ibox)%getcol('T',nlevels)
       o2vmrcol(:nlevels) = colEnvConds(ibox)%getcol('O2',nlevels)
       o3vmrcol(:nlevels) = colEnvConds(ibox)%getcol('O3',nlevels)
       so2vmrcol(:nlevels) = colEnvConds(ibox)%getcol('SO2',nlevels)
       no2vmrcol(:nlevels) = colEnvConds(ibox)%getcol('NO2',nlevels)
       vmr(:)    = vmrboxes(:,ibox)
       box_h2o   = theEnvConds(ibox)%getvar('H2O')
       box_temp  = temp(photo_lev)
       box_press = press_mid(photo_lev)
       call relhum_mod_run( box_temp, box_press, box_h2o, relhum )
       call outfile%out( 'RelHum', relhum )
       call outfile%out( 'Zenith', zenith )
       Time = TimeStart
!       call ccpp_physics_run(cdata(i), ierr=ierr)
     col_start=1
     col_end=1
     call MusicBox_ccpp_physics_run('MusicBox_suite', 'physics', col_start, col_end, ntimes, file_times, box_press, box_temp, &
        nSpecies, vmr, relhum, box_h2o, photo_lev,TimeStart,TimeEnd,njRxt,errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        call ccpp_physics_suite_part_list('MusicBox_suite', part_names, errmsg, errflg)
        write(6, *) 'Available suite parts are:'
        do index = 1, size(part_names)
          write(6, *) trim(part_names(index))
        end do
        stop
      end if

       vmrboxes(:,ibox) = vmr(:)
       write(*,'(2(a,f6.2))') 'solar zenith (degrees): ',zenith,' ...total ozone (DU): ', o3totcol
       write(*,'(a, e12.4, f6.2, f6.2)') ' total density, pressure, temperature :', density, box_press, box_temp
       call outfile%out( 'O3totcol', o3totcol )
       call outfile%out( 'Density', density )
       call outfile%out( 'Mbar', mbar )
       if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_run for column ', i, '. Exiting...'
          stop
       end if
        call outfile%out( cnst_info, vmrboxes(:,ibox) )
    end do
    TimeStart = real(n,kind=kind_phys)*dt
    write(*,'(a,1p,g0)') 'Concentration @ hour = ',TimeStart/3600.
    write(*,'(1p,5(1x,g0))') vmrboxes(:,ibox),sum(vmrboxes(:,ibox))
    if (model_name == 'terminator') then
       call outfile%out('CL_TOT', sum(vmrboxes(:,ibox)*wghts(:) ))
       call outfile%out( 'JCL2', j_rateConst(1) )
    end if
    if (model_name == '3component') then
       call outfile%out('VMRTOT', sum(vmrboxes(:,ibox)))
    end if

    end do
  end do time_loop

    call MusicBox_ccpp_physics_timestep_final('MusicBox_suite',  ntimes, file_times, box_press, box_temp,       &
        nSpecies, vmr, relhum, box_h2o,photo_lev,TimeStart,TimeEnd,njRxt,errmsg, errflg)

    call MusicBox_ccpp_physics_finalize('MusicBox_suite',  ntimes, file_times, box_press, box_temp,       &
        nSpecies, vmr, relhum, box_h2o,photo_lev,TimeStart,TimeEnd,njRxt,errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      write(6,'(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
      stop
    end if

  call outfile%close()

  deallocate(k_rateConst)
  deallocate(j_rateConst)
  deallocate(vmr)
  deallocate(vmrboxes)
!  deallocate(cdata)
  deallocate(alt)
  deallocate(press_mid)
  deallocate(press_int)
  deallocate(temp)
  deallocate(o2vmrcol)
  deallocate(o3vmrcol)
  deallocate(so2vmrcol)
  deallocate(no2vmrcol)
  deallocate(prates)
  if (allocated(wghts)) deallocate(wghts)
  if (allocated(file_times)) deallocate(file_times)

  call relhum_mod_final()

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
