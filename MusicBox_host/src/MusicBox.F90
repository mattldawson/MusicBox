module MusicBox_main

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
  use :: ODE_solver,        only: baseOdeSolver
  use :: half_solver,       only: halfsolver
  use :: Rosenbrock_Solver, only: RosenbrockSolver
  use :: Mozart_Solver,     only: MozartSolver

#include "ccpp_modules.inc"

  implicit none

!-----------------------------------------------------------
!  these dimension parameters will be set by the cafe/configurator
!-----------------------------------------------------------
  integer, parameter :: nSpecies = 3    ! number prognostic constituents
  integer, parameter :: nkRxt = 3       ! number gas phase reactions
  integer ,parameter :: ncols = 1       ! number columns in domain
  integer ,parameter :: nlevs = 8       ! number vertical levels in each column
  integer ,parameter :: ntimes = 3      ! number of time steps

  integer            :: i, k, n
  integer            :: errflg          ! error index from CPF
  integer            :: ierr
  real(kind=r8), allocatable         :: vmr(:)          ! "working" concentration passed thru CPF
  real(kind=r8), allocatable, target :: glb_vmr(:,:,:)  ! "global" concentrations
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

  allocate( theKinetics )
  allocate( ODE_obj )
! ODE_obj%theSolver => theHalfSolver
  ODE_obj%theSolver => theRosenbrockSolver
! ODE_obj%theSolver => theMozartSolver
  
  allocate(glb_vmr(ncols,nlevs,nSpecies))
  allocate(vmr(nSpecies))
  allocate(cdata(ncols))
  allocate(absTol(nSpecies))
  allocate(relTol(nSpecies))

  dt = 21._r8

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

!-----------------------------------------------------------
!  initialize the "global" concentration array glb_vmr
!-----------------------------------------------------------
  glb_vmr(:,:,1)   = 1._r8
  glb_vmr(:,:,2:3) = 0._r8

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
    write(*,'(a,1p,g0)') 'Concentration @ time = ',TimeStart
    write(*,'(1p,5(1x,g0))') vmr(:),sum(vmr(:))
  end do time_loop

finis_loop: &
  do i=1, ncols
      call ccpp_physics_finalize(cdata(i), ierr)
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
