module input_slice

  type, public :: slice_type
     
     integer :: nlons
     integer :: nlats
     integer :: nlevs
     integer :: ntimes
     integer :: beglon
     integer :: beglat
     integer :: beglev
     integer :: begtime
   contains
     procedure :: print
  end type slice_type

contains
  
  subroutine print(this)
    class(slice_type), intent(in) :: this

    write(*,'(a,i6,a,i6)') 'slice lon  limits: ',this%beglon,  ' .... ', this%beglon+this%nlons-1
    write(*,'(a,i6,a,i6)') 'slice lat  limits: ',this%beglat,  ' .... ', this%beglat+this%nlats-1
    write(*,'(a,i6,a,i6)') 'slice lev  limits: ',this%beglev,  ' .... ', this%beglev+this%nlevs-1
    write(*,'(a,i6,a,i6)') 'slice time limits: ',this%begtime, ' .... ', this%begtime+this%ntimes-1
  end subroutine print
  
end module input_slice
