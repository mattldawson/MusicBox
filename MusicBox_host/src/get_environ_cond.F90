!Hello demonstration parameterization
!

MODULE get_environ_cond

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: get_environ_cond_init
  PUBLIC :: get_environ_cond_run
  PUBLIC :: get_environ_cond_finalize

CONTAINS

!> \section arg_table_get_environ_cond_run  Argument Table
!! \htmlinclude arg_table_get_environ_cond_run.html
!!
  subroutine get_environ_cond_run(errmsg, errflg)

    ! This routine currently does nothing -- should update values

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    errmsg = ''
    errflg = 0

  END SUBROUTINE get_environ_cond_run

!> \section arg_table_get_environ_cond_init  Argument Table
!! \htmlinclude arg_table_get_environ_cond_init.html
!!
  subroutine get_environ_cond_init (nbox, nlev, ntimes, O3, temp_constant, file_times, errmsg, errflg)

   use environ_conditions_mod, only: environ_conditions_create, environ_conditions

   integer,                        intent(in)    :: nbox, nlev
   integer,                        intent(out)   :: ntimes
   real(kind_phys),                intent(out)   :: O3(:,:)
   real(kind_phys),                intent(out)   :: temp_constant(:,:)
   real(kind_phys), allocatable,   intent(out)   :: file_times(:)
   character(len=512),             intent(out)   :: errmsg
   integer,                        intent(out)   :: errflg

   character(len=120) :: env_conds_file  = '../data/env_conditions.nc'
   character(len=120) :: outfile_name    = 'test_output.nc'
   character(len=*), parameter :: nml_options = '../MusicBox_options'

   real(kind_phys),allocatable                   :: file_times_local(:)

   ! Input file has straight reals
   real, parameter    :: NOT_SET         = -huge(1.0)
   real               :: env_lat         = NOT_SET
   real               :: env_lon         = NOT_SET
   real               :: env_lev         = NOT_SET ! mbar
   real               :: user_begin_time = NOT_SET ! seconds
   real               :: user_end_time   = NOT_SET
   real               :: user_dtime      = NOT_SET
   real               :: TimeStart, TimeEnd, Time, dt, sim_beg_time, sim_end_time
   integer                       :: file_ntimes

   type(environ_conditions),pointer :: theEnvConds => null()
!   type(environ_conditions),pointer :: colEnvConds => null()

   namelist /options/ outfile_name, env_conds_file, env_lat, env_lon, env_lev, user_begin_time, user_end_time, user_dtime

!----------------------------------------------------------------

  errmsg = ''
  errflg = 0
 
  ! Read in the namelist 
  open(unit=10,file=nml_options)
  read(unit=10,nml=options)
  close(10)

  ! namelist error checking
  !----------------------------------------------------------------
  if (env_lat<-90. .or.  env_lat>90.) then
     write(errmsg,*) 'Invalid namelist setting: env_lat = ',env_lat, 'Must be set between -90 and 90 degrees north'
     errflg = 1
  end if
  if (env_lon<0. .or.  env_lon>360.) then
     write(errmsg,*) 'Invalid namelist setting: env_lon = ',env_lon,'Must be set between 0 and 360 degrees east'
     errflg = 1
  end if
  if (env_lev<0) then
     write(errmsg,*) 'Invalid namelist setting: env_lev = ',env_lev,'Must be set to a positive pressure level (hPa)'
     errflg = 1
  end if

  ! Fill theEnvConds structure (read the information)
  !----------------------------------------------------------------
  theEnvConds => environ_conditions_create( env_conds_file, lat=env_lat, lon=env_lon, lev=env_lev )

  if (user_dtime>0.) then
     dt = user_dtime

!     dt = 5._kind_phys

  else
     dt = theEnvConds%dtime()
  end if

  ! Set the begin and end times   --- TEST PASSING OUT THE file_times array
  !----------------------------------------------------------------
  if (user_begin_time == NOT_SET .or. user_end_time == NOT_SET) then
     file_ntimes= theEnvConds%ntimes()

     allocate(file_times_local(file_ntimes))
     allocate(file_times(file_ntimes))

     file_times_local = theEnvConds%get_times()
     write(6,*) ' file_times_local=', file_times_local
     file_times       = theEnvConds%get_times()
     write(6,*) ' file_times=', file_times
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

!!!!!  if (allocated(file_times)) deallocate(file_times)

  ntimes = 1+int((sim_end_time-sim_beg_time)/dt)


  end subroutine get_environ_cond_init

!> \section arg_table_get_environ_cond_finalize  Argument Table
!! \htmlinclude arg_table_get_environ_cond_finalize.html
!!
  subroutine get_environ_cond_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine get_environ_cond_finalize

END MODULE get_environ_cond
