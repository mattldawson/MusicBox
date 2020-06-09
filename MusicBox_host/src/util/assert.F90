! Portions Copyright (C) 2005-2016 Nicole Riemer and Matthew West
! Licensed under the GNU General Public License version 2 or (at your
! option) any later version. See the file COPYING for details.

!> \file
!> The music_box_assert module.

!> Assertion functions
module music_box_assert

  implicit none

  !> Error output id
  integer, parameter :: ERROR_ID = 0

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assert condition to be true or fail with message
  subroutine assert_msg( code, condition, error_message )

    use music_box_string,              only : to_char

    !> Unique code for the assertion
    integer, intent(in) :: code
    !> Condition to evaluate
    logical, intent(in) :: condition
    !> Message to display on failure
    character(len=*), intent(in) :: error_message

    if( .not. condition ) then
      write(ERROR_ID,*) "ERROR (MusicBox-"//trim( to_char( code ) )//"): "//  &
                        error_message
      stop 3
    end if

  end subroutine assert_msg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assert condition to be true or fail
  subroutine assert( code, condition )

    !> Unique code for the assertion
    integer, intent(in) :: code
    !> Condition to evaluate
    logical, intent(in) :: condition

    call assert_msg( code, condition, 'assertion failed' )

  end subroutine assert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assert condition to be true or print a warning message
  subroutine assert_warn_msg( code, condition, warning_message )

    use music_box_string,              only : to_char

    !> Unique code for the assertion
    integer, intent(in) :: code
    !> Condition to evaluate
    logical, intent(in) :: condition
    !> Message to display on failure
    character(len=*), intent(in) :: warning_message

    if( .not. condition ) then
      write(ERROR_ID,*) "WARNING (MusicBox-"//trim( to_char( code ) )//"): "//&
                        warning_message
    end if

  end subroutine assert_warn_msg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module music_box_assert
