module MusicBox_main

use const_props_mod,        only: const_props_type
use environ_conditions_mod, only: environ_conditions_create, environ_conditions
use prepare_chemistry_mod, only: prepare_chemistry_init


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
  use :: half_solver,       only: halfsolver
  use :: Rosenbrock_Solver, only: RosenbrockSolver
  use :: Mozart_Solver,     only: MozartSolver


#include "ccpp_modules.inc"

  implicit none
!-----------------------------------------------------------
!  these dimension parameters will be set by the cafe/configurator
!-----------------------------------------------------------
  integer :: nSpecies = 0   ! number prognostic constituents
  integer :: nkRxt = 0      ! number gas phase reactions
  integer :: njRxt = 0      ! number of photochemical reactions
  integer :: nTotRxt = 0    ! total number of chemical reactions
  integer :: ntimes = 0     ! number of time steps
  integer ,parameter :: ncols = 1       ! number columns in domain
  integer ,parameter :: nlevs = 1       ! number vertical levels in each column

  ! Temporary hardwiring of environmental conditions
  real, parameter :: env_lat = 40.
  real, parameter :: env_lon = 255.
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
  type(Solver_type),    pointer  :: ODE_obj
  type(kinetics_type),  pointer  :: theKinetics
  type(halfsolver),     target   :: theHalfSolver
  type(RosenbrockSolver), target :: theRosenbrockSolver
  type(MozartSolver), target     :: theMozartSolver
  type(environ_conditions),pointer :: theEnvConds => null()
  type(const_props_type), pointer :: cnst_info(:) => null()

  character(len=16) :: cnst_name

  character(len=*), parameter :: jsonfile = '/terminator-data1/home/fvitt/MusicBox/inputs/tagfileoutput.195.json'
  character(len=*), parameter :: env_conds_file = '/terminator-data1/home/fvitt/MusicBox/inputs/waccm_ma_chem.cam.h0.2000-01-01-00000.nc'
  
  ! Model name must be 'terminator' or '3component'
  ! Temporary way to specify which model is being run for input purposes
  character(len=16) :: model = '3component'

! Remove this call when the CPF can allocate arrays (it will be called either by
! the CPF or within chemistry_driver_init)
  call prepare_chemistry_init(cnst_info, nSpecies, nkRxt, njRxt, nTotRxt)
    
!----------------------------------------
! These allocates will go away once the CPF is able to allocate arrays
  allocate( theKinetics )
  allocate( ODE_obj )

  allocate(k_rateConst(nkRxt))
  allocate(j_rateConst(njRxt))
  
  allocate(vmr(nSpecies))
  allocate(cdata(ncols))
  allocate(absTol(nSpecies))
  allocate(relTol(nSpecies))
!----------------------------------------

! ODE_obj%theSolver => theHalfSolver
  ODE_obj%theSolver => theRosenbrockSolver
! ODE_obj%theSolver => theMozartSolver

  theEnvConds => environ_conditions_create( env_conds_file, lat=env_lat, lon=env_lon, lev=env_lev )
  dt = theEnvConds%dtime()
  ntimes = theEnvConds%ntimes()

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
     dt = 21._r8
  end if 

!-----------------------------------------------------------
!  set ode solver "control" variable defaults
!-----------------------------------------------------------
  absTol(:) = 1.e-9_r8
  relTol(:) = 1.e-4_r8
  icntrl(:) = 0
  rcntrl(:) = 0._r8

!-----------------------------------------------------------
!  set ode solver "control" variables
!-----------------------------------------------------------
  select type( baseOdeSolver => ODE_obj%theSolver )
    class is (RosenbrockSolver)
      icntrl(1) = 1                                 ! autonomous, F depends only on Y
      icntrl(3) = 2                                 ! ros3 solver
      rcntrl(2) = dt                                ! Hmax
      rcntrl(3) = .01_r8*dt                         ! Hstart
    class is (mozartSolver)
      icntrl(1) = 1                                 ! autonomous, F depends only on Y
      rcntrl(2) = dt                                ! Hmax
      rcntrl(3) = .01_r8*dt                         ! Hstart
  end select

  write(*,*) ' '
  write(*,*) 'icntrl settings'
  write(*,'(10i6)') icntrl(1:10)
  write(*,*) 'rcntrl settings'
  write(*,'(1p,10(1x,g0))') rcntrl(1:10)
  write(*,*) ' '

  TimeStart = 0._r8
  TimeEnd   = TimeStart + real(ntimes,kind=r8)*dt

init_loop: &
  do i = 1, ncols
      call ccpp_init( '../suites/suite_MusicBox_test_simple1.xml', cdata(i), ierr)
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
    TimeEnd = TimeStart + dt
    do k = 1, nlevs
      do i = 1, ncols
        vmr(:) = glb_vmr(i,k,:)
        call theEnvConds%update(n)
        j_rateConst(:) = theEnvConds%getvar('jcl2')
        call theKinetics%rateConst_print()
        Time = TimeStart
        call ccpp_physics_run(cdata(i), ierr=ierr)
        if (ierr/=0) then
          write(*,'(a,i0,a)') 'An error occurred in ccpp_physics_run for column ', i, '. Exiting...'
          stop
        end if
        glb_vmr(i,k,:) = vmr(:)
      end do
    end do
    TimeStart = real(n,kind=r8)*dt
    write(*,'(a,1p,g0)') 'Concentration @ hour = ',TimeStart/3600.
    write(*,'(1p,5(1x,g0))') vmr(:),sum(vmr(:))
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
