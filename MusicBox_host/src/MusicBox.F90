module MusicBox_host

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public MusicBox_sub

CONTAINS

  !> \section arg_table_MusicBox_sub  Argument Table
  !! \htmlinclude arg_table_MusicBox_sub.html
  !!
  subroutine MusicBox_sub()

    use hello_world_mod,     only: ncols
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_initialize
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_timestep_initial
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_run
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_timestep_final
    use MusicBox_ccpp_cap, only: MusicBox_ccpp_physics_finalize
    use MusicBox_ccpp_cap, only: ccpp_physics_suite_list
    use MusicBox_ccpp_cap, only: ccpp_physics_suite_part_list
    use hello_world_mod,     only: init_temp, compare_temp


    integer                         :: col_start, col_end
    integer                         :: index
    real(kind_phys), allocatable    :: file_times(:)
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg

    integer                         :: ntimes

    ! Initialize our 'data'
    call init_temp()
  

    ! Use the suite information to setup the run
    call MusicBox_ccpp_physics_initialize('MusicBox_suite', ntimes, file_times, errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    write(6,*) 'file_times=',file_times

    ! Initialize the timestep
    call MusicBox_ccpp_physics_timestep_initial('MusicBox_suite', ntimes, file_times, errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    do col_start = 1, ncols, 5
      col_end = MIN(col_start + 4, ncols)

      call MusicBox_ccpp_physics_run('MusicBox_suite', 'physics', col_start, col_end, ntimes, file_times, errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        call ccpp_physics_suite_part_list('MusicBox_suite', part_names, errmsg, errflg)
        write(6, *) 'Available suite parts are:'
        do index = 1, size(part_names)
          write(6, *) trim(part_names(index))
        end do
        stop
      end if
    end do

    call MusicBox_ccpp_physics_timestep_final('MusicBox_suite',  ntimes, file_times, errmsg, errflg)

    call MusicBox_ccpp_physics_finalize('MusicBox_suite',  ntimes, file_times, errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      write(6,'(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
      stop
    end if

    if (compare_temp()) then
      write(6, *) 'Answers are correct!'
    else
      write(6, *) 'Answers are not correct!'
    end if

  end subroutine MusicBox_sub

end module MusicBox_host

program MusicBox
  use MusicBox_host, only: MusicBox_sub
  call MusicBox_sub()
end program MusicBox
