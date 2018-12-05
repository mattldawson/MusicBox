module MusicBox_main

use const_props_mod,        only: const_props_type
use environ_conditions_mod, only: environ_conditions_create, environ_conditions
use prepare_chemistry_mod,  only: prepare_chemistry_init
use output_file,            only: output_file_type

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
  integer ,parameter :: ncols = 1       ! number columns in domain
  integer ,parameter :: nlevs = 1       ! number vertical levels in each column

  ! Temporary hardwiring of environmental conditions
  real, parameter :: env_lat = -40.
  real, parameter :: env_lon = 180.
  real, parameter :: env_lev = 1. ! mbar
  
  
  integer            :: i, k, n
  integer            :: errflg          ! error index from CPF
  integer            :: ierr
  real(kind=r8), allocatable :: j_rateConst(:)  ! host model provides photolysis rates for now
  real(kind=r8), allocatable :: k_rateConst(:)  ! host model provides photolysis rates for now
  real(kind=r8), allocatable :: vmr(:)          ! "working" concentration passed thru CPF
  real(kind=r8), allocatable :: glb_vmr(:,:,:)  ! "global" concentrations
  character(len=512) :: errmsg

  integer  :: icntrl(20)     ! integer control array for ODE solver
  real(r8) :: rcntrl(20)     ! real control array for ODE solver
  real(r8) :: TimeStart, TimeEnd, Time, dt
  real(r8), allocatable :: absTol(:), relTol(:)
  
  type(ccpp_t), allocatable, target :: cdata(:)

! declare the types
  type(kinetics_type),  pointer  :: theKinetics
  type(environ_conditions),pointer :: theEnvConds => null()
  type(environ_conditions),pointer :: colEnvConds => null()
  type(const_props_type), pointer :: cnst_info(:) => null()

  character(len=16) :: cnst_name

!  character(len=*), parameter :: env_conds_file = '/terminator-data1/fvitt/micm_inputs/waccm_ma_chem.cam.h0.2000-01-01-00000.nc'
  character(len=*), parameter :: env_conds_file = '/terminator-data1/fvitt/micm_inputs/FW2000climo.f09_f09_mg17.cam6_0_030.n01.cam.h2.0001-01-01-00000.nc'

  character(len=*), parameter :: outfile_name = 'test_output.nc'
  type(output_file_type) :: outfile

  ! Model name must be 'terminator' or '3component'
  ! Temporary way to specify which model is being run for input purposes
  character(len=*), parameter :: model = 'terminator'
!  character(len=*), parameter :: model = '3component'
!#include "chemistry_model_name.inc"

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
  real(r8) :: density, mbar, box_temp, box_press

  write(*,*) '*******************************************************'
  write(*,*) '************** model = '//trim(model)//' ***************'
  write(*,*) '*******************************************************'
  
! Remove this call when the CPF can allocate arrays 
! NOTE - It is called again in chemistry_driver_init which is where it will
! permamently reside

  call prepare_chemistry_init(cnst_info, nSpecies, nkRxt, njRxt)
    
  call outfile%create(outfile_name)
  call outfile%add(cnst_info)
  if (model == 'terminator') then
     call outfile%add('Zenith','solar zenith angle','degrees')
     call outfile%add('O3totcol','integrated ozone column (dobson units)','DU')
     call outfile%add('JCL2','Cl2 photolysis rate','sec^-1')
     call outfile%add('Density','total number density','molecules/cm3')
     call outfile%add('Mbar','mean molar mass','g/mole')
  end if

  call outfile%add('VMRTOT','sum of all species','molec/molec')

  call outfile%define() ! cannot add more fields after this call
  
!----------------------------------------
! These allocates will go away once the CPF is able to allocate arrays
  allocate( theKinetics )

  allocate(k_rateConst(nkRxt))
  allocate(j_rateConst(njRxt))
  
  allocate(vmr(nSpecies))
  allocate(cdata(ncols))
  allocate(absTol(nSpecies))
  allocate(relTol(nSpecies))
!----------------------------------------

  theEnvConds => environ_conditions_create( env_conds_file, lat=env_lat, lon=env_lon, lev=env_lev )
  dt = theEnvConds%dtime()
  ntimes = theEnvConds%ntimes()

  if (model=='terminator') then
     colEnvConds => environ_conditions_create( env_conds_file, lat=env_lat, lon=env_lon )
     nlevels = colEnvConds%nlevels()
     photo_lev = theEnvConds%levnum()

     allocate(alt(nlevels))
     allocate( press_mid(nlevels))
     allocate( press_int(nlevels))
     allocate( temp(nlevels))
     allocate( o2vmrcol(nlevels))
     allocate( o3vmrcol(nlevels))
     allocate( so2vmrcol(nlevels))
     allocate( no2vmrcol(nlevels))
     allocate( prates(nlevels,113))
     o3totcol = -9999.
  end if

!-----------------------------------------------------------
!  initialize the "global" concentration array glb_vmr
!-----------------------------------------------------------
  allocate(glb_vmr(ncols,nlevs,nSpecies))

  if (model == 'terminator') then
     do i = 1,nSpecies
        call cnst_info(i)%print()
        cnst_name = cnst_info(i)%get_name()
        print*, ' cnst name : ',cnst_name
        glb_vmr(:,:,i) = theEnvConds%getvar(cnst_name)
        print*, ' init value : ',glb_vmr(:,:,i) 
     enddo
  else if (model == '3component') then
     glb_vmr(:,:,1)   = 1._r8
     glb_vmr(:,:,2:3) = 0._r8
  end if 


  TimeStart = 0._r8
  
init_loop: &
  do i = 1, ncols
      call ccpp_init( '../suites/suite_MusicBox_'//trim(model)//'.xml', cdata(i), ierr)

      if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_init for column ', i, '. Exiting...'
          stop
      end if

    !use ccpp_fields.inc to call ccpp_field_add for all variables to be exposed to CCPP (this is auto-generated from /src/ccpp/scripts/ccpp_prebuild.py - the script parses tables in MusicBox_type_defs.f90)

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
    if (model=='terminator') then
       call colEnvConds%update(n)
    endif
    call theEnvConds%update(n)
    TimeEnd = TimeStart + dt
    do k = 1, nlevs
      do i = 1, ncols
        vmr(:) = glb_vmr(i,k,:)
        if (model == 'terminator') then
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
        end if
        Time = TimeStart
        call ccpp_physics_run(cdata(i), ierr=ierr)
        if (model == 'terminator') then
           write(*,'(2(a,f6.2))') 'solar zenith (degrees): ',zenith,' ...total ozone (DU): ', o3totcol
           write(*,'(a,f6.2,e12.4)') ' mbar, total density :', mbar, density
           call outfile%out( 'O3totcol', o3totcol )
           call outfile%out( 'JCL2', j_rateConst(1) )
           call outfile%out( 'Density', density )
           call outfile%out( 'Mbar', mbar )
        end if
!        call theKinetics%rateConst_print()
        if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_run for column ', i, '. Exiting...'
          stop
        end if
        call outfile%out( cnst_info, vmr )
        glb_vmr(i,k,:) = vmr(:)
      end do
    end do
    TimeStart = real(n,kind=r8)*dt
    write(*,'(a,1p,g0)') 'Concentration @ hour = ',TimeStart/3600.
    write(*,'(1p,5(1x,g0))') vmr(:),sum(vmr(:))
    call outfile%out('VMRTOT', sum(vmr(:)))
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
