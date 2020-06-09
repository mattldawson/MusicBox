!> \file
!> Tests for the music_box_string module

!> Test module for the music_box_string module
program test_util_string

  use music_box_assert
  use music_box_string

  implicit none

  call test_string_t()

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Test string_t functionality
  subroutine test_string_t( )

    type(string_t) :: a, b, c
    integer :: i
    real :: r
    double precision :: d
    character(len=10) :: ca

    ! string assignment

    a = "test string  "
    call assert( 814138261, a .eq. "test string" )

    b = a
    call assert( 240083225, a .eq. b )

    a = 1469
    call assert( 124406107, a .eq. "1469" )

    a = 13.4563
    call assert( 915898829, a%substring(1,6) .eq. "13.456" )

    a = 14.563d0
    call assert( 381828325, a%substring(1,5) .eq. "14.56" )

    a = .true.
    call assert( 827742932, a .eq. "true" )

    a = .false.
    call assert( 317272220, a .eq. "false" )

    ! string join to's

    a = "foo"
    b = "bar"
    c = a//b
    call assert( 938608038, c .eq. "foobar" )

    c = b//"foo"
    call assert( 817666613, c .eq. "barfoo" )

    c = a//123
    call assert( 984464744, c .eq. "foo123" )

    c = a//52.33
    call assert( 810949067, c%substring(1,7) .eq. "foo52.3" )

    c = a//53.43d0
    call assert( 419966541, c%substring(1,7) .eq. "foo53.4" )

    c = a//.true.
    call assert( 581500365, c .eq. "footrue" )

    c = a//.false.
    call assert( 971029652, c .eq. "foofalse" )

    ! equality

    a = "foo"
    b = "foo"
    c = "bar"
    call assert( 160576005, a .eq. b )
    call assert( 667687944, .not. a .eq. c )

    call assert( 322109829, a .eq. "foo" )
    call assert( 264271270, .not. a .eq. "bar" )

    a = 134
    call assert( 325920897, a .eq. 134 )
    call assert( 315392283, .not. a .eq. 432 )

    a = 52.3
    call assert( 420993082, a .eq. 52.3 )
    call assert( 428162923, .not. a .eq. 762.4 )

    a = 87.45d0
    call assert( 307221498, a .eq. 87.45d0 )
    call assert( 696750785, .not. a .eq. 43.5d9 )

    a = .true.
    b = .false.
    call assert( 240759859, a .eq. .true. )
    call assert( 919934236, .not. a .eq. .false. )
    call assert( 179562527, b .eq. .false. )
    call assert( 969149715, .not. b .eq. .true. )

    ! not-equals

    a = "foo"
    b = "foo"
    c = "bar"
    call assert( 678503681, .not. a .ne. b )
    call assert( 173297276, a .ne. c )

    call assert( 903140371, .not. a .ne. "foo" )
    call assert( 732983467, a .ne. "bar" )

    a = 134
    call assert( 845301812, .not. a .ne. 134 )
    call assert( 957620157, a .ne. 432 )

    a = 52.3
    call assert( 787463253, .not. a .ne. 52.3 )
    call assert( 334831100, a .ne. 762.4 )

    a = 87.45d0
    call assert( 447149445, .not. a .ne. 87.45d0 )
    call assert( 894517291, a .ne. 43.5d9 )

    a = .true.
    b = .false.
    call assert( 389310886, .not. a .ne. .true. )
    call assert( 501629231, a .ne. .false. )
    call assert( 948997077, .not. b .ne. .false. )
    call assert( 778840173, b .ne. .true. )

    ! case convert

    a = "FoObAr 12 %"
    call assert( 500463115, a%to_lower( ) .eq. "foobar 12 %" )
    call assert( 614686994, a%to_upper( ) .eq. "FOOBAR 12 %" )

    ! substring

    call assert( 328852972, a%substring(1,6) .eq. "FoObAr" )
    call assert( 272919947, a%substring(4,5) .eq. "bAr 1" )
    call assert( 604610675, a%substring(7,20) .eq. " 12 %" )

    ! assignment from string

    ca = "XXXXXXXXXX"
    a = "foo"
    ca = a
    call assert( 189690040, trim( ca ) .eq. "foo" )

    ! joins from strings

    ca = "foo"
    a = "bar"
    call assert( 511304449, trim( ca )//a .eq. "foobar" )

    i = 122
    call assert( 678841998, i//a .eq. "122bar" )

    r = 34.63
    b = r//a
    call assert( 165012513, b%substring(1,4) .eq. "34.6" )
    call assert( 610927120, b%substring( b%length( ) - 2, 3 ) .eq. "bar" )

    d = 43.63d0
    b = d//a
    call assert( 625841048, b%substring(1,4) .eq. "43.6" )
    call assert( 848572204, b%substring( b%length( ) - 2, 3 ) .eq. "bar" )

    call assert( 345271333, .true.//a .eq. "truebar" )
    call assert( 164585815, .false.//a .eq. "falsebar" )

    ! equality

    a = "foo"
    b = "foo"
    call assert( 719459994, "foo" .eq. a )
    call assert( 549303090, .not. "bar" .eq. b )

    a = 134
    call assert( 944096684, 134 .eq. a )
    call assert( 773939780, .not. 432 .eq. a )

    a = 52.3
    call assert( 603782876, 52.3 .eq. a )
    call assert( 433625972, .not. 762.4 .eq. a )

    a = 87.45d0
    call assert( 828419566, 87.45d0 .eq. a )
    call assert( 375787413, .not. 43.5d9 .eq. a )

    a = .true.
    b = .false.
    call assert( 153056257, .true. .eq. a )
    call assert( 882899352, .not. .false. .eq. a )
    call assert( 995217697, .false. .eq. b )
    call assert( 542585544, .not. .true. .eq. b )

    ! not-equals

    a = "foo"
    b = "foo"
    call assert( 597065330, .not. "foo" .ne. a )
    call assert( 426908426, "bar" .ne. a )

    a = 134
    call assert( 539226771, .not. 134 .ne. a )
    call assert( 369069867, 432 .ne. a )

    a = 52.3
    call assert( 146338711, .not. 52.3 .ne. a )
    call assert( 876181806, 762.4 .ne. a )

    a = 87.45d0
    call assert( 706024902, .not. 87.45d0 .ne. a )
    call assert( 535867998, 43.5d9 .ne. a )

    a = .true.
    b = .false.
    call assert( 648186343, .not. .true. .ne. a )
    call assert( 760504688, .false. .ne. a )
    call assert( 872823033, .not. .false. .ne. b )
    call assert( 702666129, .true. .ne. b )

    ca = "XXXXXXXXXX"
    ca = to_char( 345 )
    call assert( 278095873, trim( ca ) .eq. "345" )

    ca = "XXXXXXXXXX"
    ca = to_char( 482.53 )
    call assert( 876921224, ca(1:5) .eq. "482.5" )

    ca = "XXXXXXXXXX"
    ca = to_char( 873.453d0 )
    call assert( 989239569, ca(1:6) .eq. "873.45" )

    ca = "XXXXXXXXXX"
    ca = to_char( .true. )
    call assert( 201557915, trim( ca ) .eq. "true" )

    ca = "XXXXXXXXXX"
    ca = to_char( .false. )
    call assert( 931401010, trim( ca ) .eq. "false" )


  end subroutine test_string_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_util_string
