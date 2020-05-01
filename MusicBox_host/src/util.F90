! Portions Copyright (C) 2005-2016 Nicole Riemer and Matthew West
! Licensed under the GNU General Public License version 2 or (at your
! option) any later version. See the file COPYING for details.

!> \file
!> The music_box_util module.

!> Common utility subroutines
module music_box_util

  implicit none

  !> Error output id
  integer, parameter :: ERROR_ID = 0
  !> Length of string for to_string conversions
  integer, parameter :: MB_UTIL_CONVERT_STRING_LEN = 100

  !> Interface to to_string functions
  !! \todo add more type_to_string functions as needed
  interface to_string
    module procedure integer_to_string
  end interface to_string

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assert condition to be true or fail with message
  subroutine assert_msg(code, condition, error_message)

    !> Unique code for the assertion
    integer, intent(in) :: code
    !> Condition to evaluate
    logical, intent(in) :: condition
    !> Message to display on failure
    character(len=*), intent(in) :: error_message

    if(.not.condition) then
      write(ERROR_ID,*) "ERROR (MusicBox-"//trim(to_string(code))//"): "//    &
                        trim(error_message)
      stop 3
    end if

  end subroutine assert_msg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assert condition to be true or fail
  subroutine assert(code, condition)

    !> Unique code for the assertion
    integer, intent(in) :: code
    !> Condition to evaluate
    logical, intent(in) :: condition

    call assert_msg(code, condition, 'assertion failed')

  end subroutine assert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert an integer to a char array
  character(len=MB_UTIL_CONVERT_STRING_LEN) function integer_to_string(val)

    !> Value to convert
    integer, intent(in) :: val

    character(len=MB_UTIL_CONVERT_STRING_LEN) :: ret_val

    ret_val = ""
    write(ret_val, '(i30)') val
    integer_to_string = adjustl(ret_val)

  end function integer_to_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module music_box_util
