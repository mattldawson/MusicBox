module MusicBox_main

use const_props_mod,        only: const_props_type
use environ_conditions_mod, only: environ_conditions_create, environ_conditions
use prepare_chemistry_mod,  only: prepare_chemistry_init
use output_file,            only: output_file_type
use machine,     only: r8 => kind_phys

implicit none

contains

subroutine MusicBox_main_sub()

  use :: ccpp_api,                           &
         only: ccpp_t,                       &
               ccpp_init,                    &
               ccpp_finalize,                &
               ccpp_physics_init,            &
               ccpp_physics_run,             &
               ccpp_physics_finalize,        &
               ccpp_field_add

  use :: iso_c_binding, only: c_loc

#include "ccpp_modules.inc"

  implicit none
!-----------------------------------------------------------
!  these dimension parameters will be set by the cafe/configurator
!-----------------------------------------------------------
  integer :: nSpecies = 0   ! number prognostic constituents
  integer :: nkRxt = 0      ! number gas phase reactions
  integer :: njRxt = 0      ! number of photochemical reactions
  integer :: ntimes = 0     ! number of time steps

  integer ,parameter :: ncols = 1 ! number columns in domain

  
  integer            :: i,n
  integer            :: ierr
  real(kind=r8), allocatable :: j_rateConst(:)  ! host model provides photolysis rates for now 
  real(kind=r8), allocatable :: k_rateConst(:)  ! host model provides photolysis rates for now
  real(kind=r8), allocatable :: vmr(:)          ! "working" concentration passed thru CPF
  real(kind=r8), allocatable :: wghts(:)

  real(r8) :: TimeStart, TimeEnd, Time, dt
  
  type(ccpp_t), allocatable, target :: cdata(:)

! declare the types
  type(environ_conditions),pointer :: theEnvConds => null()
  type(environ_conditions),pointer :: colEnvConds => null()
  type(const_props_type), pointer :: cnst_info(:) => null()

  character(len=16) :: cnst_name
  character(len=20) :: model_name

  type(output_file_type) :: outfile

  integer :: photo_lev
  integer :: nlevels
  real(r8) :: zenith
  real(r8) :: albedo
  real(r8) :: o3totcol
  real(r8), allocatable :: alt(:)
  real(r8), allocatable :: press_mid(:)
  real(r8), allocatable :: press_int(:)
  real(r8), allocatable :: temp(:)
  real(r8), allocatable :: o2vmrcol(:)
  real(r8), allocatable :: o3vmrcol(:)
  real(r8), allocatable :: so2vmrcol(:)
  real(r8), allocatable :: no2vmrcol(:)
  real(r8), allocatable :: prates(:,:)
  real(r8), allocatable :: file_times(:)
  real(r8) :: density, mbar, box_temp, box_press
  integer :: file_ntimes
  real(r8) :: sim_beg_time, sim_end_time

  ! run-time options
  character(len=120) :: env_conds_file = '../data/env_conditions.nc'
  character(len=120) :: outfile_name = 'test_output.nc'
  real :: env_lat = -999999.
  real :: env_lon = -999999.
  real :: env_lev = -999999. ! mbar
  real :: user_begin_time = -999999. ! seconds
  real :: user_end_time = -999999.
  real :: user_dtime = -999999.
  
  character(len=*), parameter :: nml_options = '../MusicBox_options'
  ! read namelist run-time options
  namelist /options/ outfile_name, env_conds_file
  namelist /options/ env_lat, env_lon, env_lev
  namelist /options/ user_begin_time, user_end_time, user_dtime
  
  open(unit=10,file=nml_options)
  read(unit=10,nml=options)
  close(10)

  ! error checking
  if (env_lat<-90. .or.  env_lat>90.) then
     write(*,*) 'Invalid namelist setting: env_lat = ',env_lat
     write(*,*) 'Must be set between -90 and 90 degrees north'
     stop
  end if
  if (env_lon<0. .or.  env_lon>360.) then
     write(*,*) 'Invalid namelist setting: env_lon = ',env_lon
     write(*,*) 'Must be set between 0 and 360 degrees east'
     stop
  end if
  if (env_lev<0) then
      write(*,*) 'Invalid namelist setting: env_lev = ',env_lev
     write(*,*) 'Must be set to a positive pressure level (hPa)'
    stop
  end if

! Remove this call when the CPF can allocate arrays 
! NOTE - It is called again in chemistry_driver_init which is where it will
! permamently reside

  call prepare_chemistry_init(cnst_info, model_name, nSpecies, nkRxt, njRxt)
    
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

  call outfile%define() ! cannot add more fields after this call
  
!----------------------------------------
! These allocates will go away once the CPF is able to allocate arrays

  allocate(k_rateConst(nkRxt))
  allocate(j_rateConst(njRxt))
  
  allocate(vmr(nSpecies))
  allocate(cdata(ncols)) ! ccpp requires column dimension 
!----------------------------------------

  theEnvConds => environ_conditions_create( env_conds_file, lat=env_lat, lon=env_lon, lev=env_lev )
  if (user_dtime>0.) then
     dt = user_dtime
  else
     dt = theEnvConds%dtime()
  end if

  if (user_begin_time>0. .and. user_end_time>0.) then
     sim_beg_time = user_begin_time
     sim_end_time = user_end_time
  else
     file_ntimes= theEnvConds%ntimes()
     allocate(file_times(file_ntimes))
     file_times = theEnvConds%get_times()
     sim_beg_time = file_times(1)
     sim_end_time = file_times(file_ntimes)
  end if
  ntimes = 1+int((sim_end_time-sim_beg_time)/dt)

  colEnvConds => environ_conditions_create( env_conds_file, lat=env_lat, lon=env_lon )
  nlevels = colEnvConds%nlevels()
  photo_lev = theEnvConds%levnum()

  allocate(alt(nlevels))
  allocate(press_mid(nlevels))
  allocate(press_int(nlevels))
  allocate(temp(nlevels))
  allocate(o2vmrcol(nlevels))
  allocate(o3vmrcol(nlevels))
  allocate(so2vmrcol(nlevels))
  allocate(no2vmrcol(nlevels))
  allocate(prates(nlevels,113))

  if (model_name == 'terminator') then
     allocate(wghts(nSpecies))
     wghts(:) = 1._r8
  endif

  do n = 1,nSpecies
     call cnst_info(n)%print()
     cnst_name = cnst_info(n)%get_name()
     if (cnst_name == 'N2') then
        vmr(n) = theEnvConds%getvar(cnst_name,default_value=0.79_r8)
     else if (cnst_name == 'O2') then
        vmr(n) = theEnvConds%getvar(cnst_name,default_value=0.21_r8)
     else
        vmr(n) = theEnvConds%getvar(cnst_name,default_value=0.00_r8)
     end if

     write(*,fmt="(' cnst name : ',a20,' init value : ',e13.6)") cnst_name, vmr(n)
     if (allocated(wghts) .and. cnst_name == 'CL2') then
        wghts(n) = 2._r8
     end if
  enddo


  TimeStart = 0._r8
  TimeEnd = TimeStart + dt
  
init_loop: & ! ccpp requires a loop over columns
  do i = 1, ncols
      call ccpp_init( '../suites/suite_MusicBox_'//trim(model_name)//'.xml', cdata(i), ierr)

      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_init for column ', i, '. Exiting...'
          stop
      end if

 ! use ccpp_fields.inc to call ccpp_field_add for all variables to be exposed to CCPP (this is
 ! auto-generated from /src/ccpp/scripts/ccpp_prebuild.py - the script parses tables in MusicBox_type_defs.f90)
 ! this requires column index i
#  include "ccpp_fields.inc"

      !initialize each column's physics
      call ccpp_physics_init(cdata(i), ierr=ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_init for column ', i, '. Exiting...'
          stop
      end if
  end do init_loop

!-----------------------------------------------------------
!  loop over time
!-----------------------------------------------------------
time_loop: &
  do n = 1, ntimes
    call outfile%advance(TimeStart)
    call colEnvConds%update(TimeStart)
    TimeEnd = TimeStart + dt
    do i = 1, ncols
       zenith = colEnvConds%getsrf('SZA')
       albedo = colEnvConds%getsrf('ASDIR')
       press_mid(:nlevels) = colEnvConds%press_mid(nlevels)
       press_int(:nlevels) = colEnvConds%press_int(nlevels)
       alt(:nlevels) = colEnvConds%getcol('Z3',nlevels)
       temp(:nlevels) = colEnvConds%getcol('T',nlevels)
       o2vmrcol(:nlevels) = colEnvConds%getcol('O2',nlevels)
       o3vmrcol(:nlevels) = colEnvConds%getcol('O3',nlevels)
       so2vmrcol(:nlevels) = colEnvConds%getcol('SO2',nlevels)
       no2vmrcol(:nlevels) = colEnvConds%getcol('NO2',nlevels)
       box_temp = temp(photo_lev)
       box_press = press_mid(photo_lev)
       call outfile%out( 'Zenith', zenith )
       Time = TimeStart
       call ccpp_physics_run(cdata(i), ierr=ierr)
       write(*,'(2(a,f6.2))') 'solar zenith (degrees): ',zenith,' ...total ozone (DU): ', o3totcol
       write(*,'(a,f6.2,e12.4)') ' mbar, total density :', mbar, density
       call outfile%out( 'O3totcol', o3totcol )
       call outfile%out( 'Density', density )
       call outfile%out( 'Mbar', mbar )
       if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_run for column ', i, '. Exiting...'
          stop
       end if
       call outfile%out( cnst_info, vmr )
    end do
    TimeStart = real(n,kind=r8)*dt
    write(*,'(a,1p,g0)') 'Concentration @ hour = ',TimeStart/3600.
    write(*,'(1p,5(1x,g0))') vmr(:),sum(vmr(:))
    if (model_name == 'terminator') then
       call outfile%out('CL_TOT', sum(vmr(:)*wghts(:) ))
       call outfile%out( 'JCL2', j_rateConst(1) )
    end if
    if (model_name == '3component') then
       call outfile%out('VMRTOT', sum(vmr(:)))
    end if
  end do time_loop

finis_loop: &
  do i=1, ncols
      call ccpp_physics_finalize(cdata(i), ierr=ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_finalize for column ', i, '. Exiting...'
          stop
      end if
      call ccpp_finalize(cdata(i), ierr)
      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_finalize for column ', i, '. Exiting...'
          stop
      end if
  end do finis_loop

  call outfile%close()

  deallocate(k_rateConst)
  deallocate(j_rateConst)
  deallocate(vmr)
  deallocate(cdata)
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
  
end subroutine MusicBox_main_sub

end module MusicBox_main

!> \brief Main SCM program that calls the main SCM subroutine
!!
!! The Doxygen documentation system cannot handle in-body comments in Fortran main programs, so the "main" program was put in the
!! subroutine \ref MusicBox_main_sub above.
program MusicBox
  use MusicBox_main
  call MusicBox_main_sub()  
end program MusicBox
