!> \file
!> The music_box_string module

!> The string_t type and related functions
module music_box_string

  implicit none
  private

  public :: string_t, assignment(=), operator(//), operator(==),              &
            operator(/=), to_char

  !> Length of character array for to_char conversions
  integer, parameter :: CONVERT_CHAR_LENGTH = 100

  !> Generic tring type
  type :: string_t
  private
    !> the string
    character(len=:), allocatable :: val
  contains
    !> \defgroup StringAssign String assignment
    !! @{
    procedure, private :: string_assign_string
    procedure, private :: string_assign_char
    procedure, private :: string_assign_int
    procedure, private :: string_assign_real
    procedure, private :: string_assign_double
    procedure, private :: string_assign_logical
    generic :: assignment(=) => string_assign_string, string_assign_char,     &
                                string_assign_int, string_assign_real,        &
                                string_assign_double, string_assign_logical
    !> @}
    !> \defgroup StringJoin Join a string
    !! @{
    procedure, private :: string_join_string
    procedure, private :: string_join_char
    procedure, private :: string_join_int
    procedure, private :: string_join_real
    procedure, private :: string_join_double
    procedure, private :: string_join_logical
    generic :: operator(//) => string_join_string, string_join_char,          &
                               string_join_int, string_join_real,             &
                               string_join_double, string_join_logical
    !> @}
    !> \defgroup StringEquality String equality
    !! @{
    procedure, private :: string_equals_string
    procedure, private :: string_equals_char
    procedure, private :: string_equals_int
    procedure, private :: string_equals_real
    procedure, private :: string_equals_double
    procedure, private :: string_equals_logical
    generic :: operator(==) => string_equals_string, string_equals_char,      &
                               string_equals_int, string_equals_real,         &
                               string_equals_double, string_equals_logical
    procedure, private :: string_not_equals_string
    procedure, private :: string_not_equals_char
    procedure, private :: string_not_equals_int
    procedure, private :: string_not_equals_real
    procedure, private :: string_not_equals_double
    procedure, private :: string_not_equals_logical
    generic :: operator(/=) => string_not_equals_string,                      &
                               string_not_equals_char,                        &
                               string_not_equals_int,                         &
                               string_not_equals_real,                        &
                               string_not_equals_double,                      &
                               string_not_equals_logical
    !> @}
    !> Output
    procedure :: write_string_unformatted
    procedure :: write_string_formatted
    generic :: write(unformatted) => write_string_unformatted
    generic :: write(formatted) => write_string_formatted
    !> @}
    !> String length
    procedure :: length
    !> Convert a string to upper case
    procedure :: to_upper
    !> Convert a string to lower case
    procedure :: to_lower
    !> Substring
    procedure :: substring
  end type string_t

  interface assignment(=)
    module procedure char_assign_string
  end interface

  interface operator(//)
    module procedure char_join_string
    module procedure int_join_string
    module procedure real_join_string
    module procedure double_join_string
    module procedure logical_join_string
  end interface

  interface operator(==)
    module procedure char_equals_string
    module procedure int_equals_string
    module procedure real_equals_string
    module procedure double_equals_string
    module procedure logical_equals_string
  end interface

  interface operator(/=)
    module procedure char_not_equals_string
    module procedure int_not_equals_string
    module procedure real_not_equals_string
    module procedure double_not_equals_string
    module procedure logical_not_equals_string
  end interface

  !> Converts values to character arrays
  interface to_char
    module procedure int_to_char
    module procedure real_to_char
    module procedure double_to_char
    module procedure logical_to_char
  end interface to_char

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assign a string from a string
  subroutine string_assign_string( to, from )

    !> String to assign
    class(string_t), intent(out) :: to
    !> New string value
    class(string_t), intent(in) :: from

    to%val = from%val

  end subroutine string_assign_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assign a string from a character array
  subroutine string_assign_char( to, from )

    !> String to assign
    class(string_t), intent(out) :: to
    !> New string value
    character(len=*), intent(in) :: from

    to%val = trim( from )

  end subroutine string_assign_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assign a string from an integer
  subroutine string_assign_int( to, from )

    !> String to assign
    class(string_t), intent(out) :: to
    !> New string value
    integer, intent(in) :: from

    character(len=30) :: new_val

    write( new_val, '(i30)' ) from
    to%val = adjustl( new_val )

  end subroutine string_assign_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assign a string from an real number
  subroutine string_assign_real( to, from )

    !> String to assign
    class(string_t), intent(out) :: to
    !> New string value
    real, intent(in) :: from

    character(len=60) :: new_val

    write( new_val, '(g30.20)' ) from
    to%val = adjustl( new_val )

  end subroutine string_assign_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assign a string from an double precision real number
  subroutine string_assign_double( to, from )

    !> String to assign
    class(string_t), intent(out) :: to
    !> New string value
    double precision, intent(in) :: from

    character(len=60) :: new_val

    write( new_val, '(g30.20)' ) from
    to%val = adjustl( new_val )

  end subroutine string_assign_double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assign a string from a logical
  subroutine string_assign_logical( to, from )

    !> String to assign
    class(string_t), intent(out) :: to
    !> New string value
    logical, intent(in) :: from

    if( from ) then
      to%val = "true"
    else
      to%val = "false"
    end if

  end subroutine string_assign_logical

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a string to a string
  elemental function string_join_string( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> String to join
    class(string_t), intent(in) :: a
    !> String to join
    class(string_t), intent(in) :: b

    c%val = a%val//b%val

  end function string_join_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a string to a character array
  elemental function string_join_char( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> String to join
    class(string_t), intent(in) :: a
    !> Character array to join
    character(len=*), intent(in) :: b

    c%val = a%val//trim( b )

  end function string_join_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a string to an integer
  elemental function string_join_int( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> String to join
    class(string_t), intent(in) :: a
    !> Integer to join
    integer, intent(in) :: b

    character(len=30) :: new_val

    write( new_val, '(i30)' ) b
    c%val = a%val//adjustl( new_val )

  end function string_join_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a string to a real number
  elemental function string_join_real( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> String to join
    class(string_t), intent(in) :: a
    !> Real number to join
    real, intent(in) :: b

    character(len=60) :: new_val

    write( new_val, '(g30.20)' ) b
    c%val = a%val//adjustl( new_val )

  end function string_join_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a string to a double precision real number
  elemental function string_join_double( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> String to join
    class(string_t), intent(in) :: a
    !> Double precision real number to join
    double precision, intent(in) :: b

    character(len=60) :: new_val

    write( new_val, '(g30.20)' ) b
    c%val = a%val//adjustl( new_val )

  end function string_join_double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a string to a logical
  elemental function string_join_logical( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> String to join
    class(string_t), intent(in) :: a
    !> Logical to join
    logical, intent(in) :: b

    if( b ) then
      c%val = a%val//"true"
    else
      c%val = a%val//"false"
    end if

  end function string_join_logical

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a string for equality
  logical elemental function string_equals_string( a, b ) result( equals )

    !> String a
    class(string_t), intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    equals = trim( a%val ) .eq. trim( b%val )

  end function string_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a character array for equality
  logical elemental function string_equals_char( a, b ) result( equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Character array b
    character(len=*), intent(in) :: b

    equals = trim( a%val ) .eq. trim( b )

  end function string_equals_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a integer for equality
  logical elemental function string_equals_int( a, b ) result( equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Integer b
    integer, intent(in) :: b

    character(len=30) :: comp_val

    write( comp_val, '(i30)' ) b
    equals = trim( a%val ) .eq. adjustl( comp_val )

  end function string_equals_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a real number for equality
  logical elemental function string_equals_real( a, b ) result( equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Real number b
    real, intent(in) :: b

    character(len=60) :: comp_val

    write( comp_val, '(g30.20)' ) b
    equals = trim( a%val ) .eq. adjustl( comp_val )

  end function string_equals_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a double-precision real number for equality
  logical elemental function string_equals_double( a, b ) result( equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Double-precition real number b
    double precision, intent(in) :: b

    character(len=60) :: comp_val

    write( comp_val, '(g30.20)' ) b
    equals = trim( a%val ) .eq. adjustl( comp_val )

  end function string_equals_double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a logical for equality
  logical elemental function string_equals_logical( a, b ) result( equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Logical b
    logical, intent(in) :: b

    equals = ( trim( a%val ) .eq. "true"  .and.       b ) .or.                &
             ( trim( a%val ) .eq. "false" .and. .not. b )

  end function string_equals_logical

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a string for equality
  logical elemental function string_not_equals_string( a, b )                 &
      result( not_equals )

    !> String a
    class(string_t), intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    not_equals = .not. a .eq. b

  end function string_not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a character array for equality
  logical elemental function string_not_equals_char( a, b )                   &
      result( not_equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Character array b
    character(len=*), intent(in) :: b

    not_equals = .not. a .eq. b

  end function string_not_equals_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a integer for equality
  logical elemental function string_not_equals_int( a, b )                    &
      result( not_equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Integer b
    integer, intent(in) :: b

    not_equals = .not. a .eq. b

  end function string_not_equals_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a real number for equality
  logical elemental function string_not_equals_real( a, b )                   &
      result( not_equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Real number b
    real, intent(in) :: b

    not_equals = .not. a .eq. b

  end function string_not_equals_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a double-precision real number for equality
  logical elemental function string_not_equals_double( a, b )                 &
      result( not_equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Double-precition real number b
    double precision, intent(in) :: b

    not_equals = .not. a .eq. b

  end function string_not_equals_double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a string to a logical for equality
  logical elemental function string_not_equals_logical( a, b )                &
      result( not_equals )

    !> String a
    class(string_t), intent(in) :: a
    !> Logical b
    logical, intent(in) :: b

    not_equals = .not. a .eq. b

  end function string_not_equals_logical

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Write a string
  subroutine write_string_unformatted( this, unit, iostat, iomsg )

    !> String to read into
    class(string_t), intent(in) :: this
    !> File unit
    integer, intent(in) :: unit
    !> I/O status
    integer, intent(out) :: iostat
    !> I/O error message
    character(len=*), intent(inout) :: iomsg

    write( unit, iostat=iostat, iomsg=iomsg ) this%val

  end subroutine write_string_unformatted

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Write a string
  subroutine write_string_formatted( this, unit, iotype, v_list, iostat,      &
      iomsg )

    !> String to read into
    class(string_t), intent(in) :: this
    !> File unit
    integer, intent(in) :: unit
    !> Format string
    character(len=*), intent(in) :: iotype
    !> V list
    integer, intent(in) :: v_list(:)
    !> I/O status
    integer, intent(out) :: iostat
    !> I/O error message
    character(len=*), intent(inout) :: iomsg

    write( unit, fmt=*, iostat=iostat, iomsg=iomsg ) this%val

  end subroutine write_string_formatted

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get the length of the string
  elemental integer function length( this )

    !> String
    class(string_t), intent(in) :: this

    length = len( this%val )

  end function length

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert a string to upper case
  !!
  !! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
  !! Original author: Clive Page
  function  to_upper( this ) result( cap_string )

    !> Converted string
    type(string_t) :: cap_string
    !> String to convert
    class(string_t), intent(in) :: this

    character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'
    integer :: i_str, i_char

    cap_string%val = this%val
    do i_str = 1, len( cap_string%val )
      i_char = index( low, cap_string%val(i_str:i_str) )
      if( i_char .gt. 0 ) cap_string%val(i_str:i_str) = cap(i_char:i_char)
    end do

  end function to_upper

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert a string to lower case
  !!
  !! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
  !! Original author: Clive Page
  function to_lower( this ) result( low_string )

    !> Converted string
    type(string_t) :: low_string
    !> String to convert
    class(string_t), intent(in) :: this

    character(26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'
    integer :: i_str, i_char

    low_string%val = this%val
    do i_str = 1, len( low_string%val )
      i_char = index( cap, low_string%val(i_str:i_str) )
      if( i_char .gt. 0 ) low_string%val(i_str:i_str) = low(i_char:i_char)
    end do

  end function to_lower

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Return a substring
  function substring( this, start_index, length )

    !> Substring
    type(string_t) :: substring
    !> Full string
    class(string_t), intent(in) :: this
    !> Starting character index
    integer, intent(in) :: start_index
    !> Length of the substring to return
    integer, intent(in) :: length

    integer :: l

    if( start_index + length - 1 .gt. len( this%val ) ) then
      l = len( this%val ) - start_index + 1
    else
      l = length
    end if
    substring%val = this%val(start_index:l+start_index-1)

  end function substring

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assign a character array from a string
  subroutine char_assign_string( to, from )

    !> Character array to assign
    character(len=*), intent(out) :: to
    !> String to assign
    class(string_t), intent(in) :: from

    integer :: len_char, len_str

    len_char = len( to )
    len_str  = len( from%val )

    if( len_char .lt. len_str ) then
      to = from%val(1:len_char)
    else
      to = from%val
    end if

  end subroutine char_assign_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a character array to a string
  elemental function char_join_string( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> Character array to join
    character(len=*), intent(in) :: a
    !> String to join
    class(string_t), intent(in) :: b

    c%val = trim( a )//b%val

  end function char_join_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join an integer to a string
  elemental function int_join_string( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> Integer to join
    integer, intent(in) :: a
    !> String to join
    class(string_t), intent(in) :: b

    character(len=30) :: new_val

    write( new_val, '(i30)' ) a
    c%val = trim( adjustl( new_val ) )//b%val

  end function int_join_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a real number to a string
  elemental function real_join_string( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> Real number to join
    real, intent(in) :: a
    !> String to join
    class(string_t), intent(in) :: b

    character(len=60) :: new_val

    write( new_val, '(g30.20)' ) a
    c%val = trim( adjustl( new_val ) )//b%val

  end function real_join_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a double precision real number to a string
  elemental function double_join_string( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> Double precision real number to join
    double precision, intent(in) :: a
    !> String to join
    class(string_t), intent(in) :: b

    character(len=60) :: new_val

    write( new_val, '(g30.20)' ) a
    c%val = trim( adjustl( new_val ) )//b%val

  end function double_join_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Join a logical to a string
  elemental function logical_join_string( a, b ) result( c )

    !> Joined string
    type(string_t) :: c
    !> Logical to join
    logical, intent(in) :: a
    !> String to join
    class(string_t), intent(in) :: b

    if( a ) then
      c%val = "true"//b%val
    else
      c%val = "false"//b%val
    end if

  end function logical_join_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a character array to a string for equality
  logical elemental function char_equals_string( a, b ) result( equals )

    !> Character array a
    character(len=*), intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    equals = b .eq. a

  end function char_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare an integer to a string for equality
  logical elemental function int_equals_string( a, b ) result( equals )

    !> Integer a
    integer, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    equals = b .eq. a

  end function int_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a real number to a string for equality
  logical elemental function real_equals_string( a, b ) result( equals )

    !> Real number a
    real, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    equals = b .eq. a

  end function real_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a double-precision real number to a string for equality
  logical elemental function double_equals_string( a, b ) result( equals )

    !> Double-precision real number a
    double precision, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    equals = b .eq. a

  end function double_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a logical to a string for equality
  logical elemental function logical_equals_string( a, b ) result( equals )

    !> Logical a
    logical, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    equals = b .eq. a

  end function logical_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a character array to a string for equality
  logical elemental function char_not_equals_string( a, b )                   &
      result( not_equals )

    !> Character array a
    character(len=*), intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    not_equals = .not. b .eq. a

  end function char_not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare an integer to a string for equality
  logical elemental function int_not_equals_string( a, b )                    &
      result( not_equals )

    !> Integer a
    integer, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    not_equals = .not. b .eq. a

  end function int_not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a real number to a string for equality
  logical elemental function real_not_equals_string( a, b )                   &
      result( not_equals )

    !> Real number a
    real, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    not_equals = .not. b .eq. a

  end function real_not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a double-precision real number to a string for equality
  logical elemental function double_not_equals_string( a, b )                 &
      result( not_equals )

    !> Double-precition real number a
    double precision, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    not_equals = .not. b .eq. a

  end function double_not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Compare a logical to a string for equality
  logical elemental function logical_not_equals_string( a, b )                &
      result( not_equals )

    !> Logical a
    logical, intent(in) :: a
    !> String b
    class(string_t), intent(in) :: b

    not_equals = .not. b .eq. a

  end function logical_not_equals_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert an integer to a char array
  character(len=CONVERT_CHAR_LENGTH) function int_to_char( val )              &
      result( ret_val )

    !> Value to convert
    integer, intent(in) :: val

    write( ret_val, '(i30)' ) val
    ret_val = adjustl( ret_val )

  end function int_to_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert a real number to a char array
  character(len=CONVERT_CHAR_LENGTH) function real_to_char( val )             &
      result( ret_val )

    !> Value to convert
    real, intent(in) :: val

    write( ret_val, '(g30.20)' ) val
    ret_val = adjustl( ret_val )

  end function real_to_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert a double-precision real number to a char array
  character(len=CONVERT_CHAR_LENGTH) function double_to_char( val )           &
      result( ret_val )

    !> Value to convert
    double precision, intent(in) :: val

    write( ret_val, '(g30.20)' ) val
    ret_val = adjustl( ret_val )

  end function double_to_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert a logical to a char array
  character(len=CONVERT_CHAR_LENGTH) function logical_to_char( val )          &
      result( ret_val )

    !> Value to convert
    logical, intent(in) :: val

    if( val ) then
      write( ret_val, '(a4)' ) "true"
    else
      write( ret_val, '(a5)' ) "false"
    end if

  end function logical_to_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module music_box_string
