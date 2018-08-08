module json_loader
  use const_props_mod, only: const_props_type
  use json_module, only: json_file, json_value, json_core

  implicit none
  
contains
  function create_cnst_info_array( jsonfile ) result(cnst_info)

    character(len=*), intent(in) :: jsonfile
    type(const_props_type), pointer :: cnst_info(:)

    integer :: ncnst
    type(json_file) :: json       !! the JSON structure read from the file

    type(json_value),pointer :: p !! a pointer for low-level manipulations
    type(json_core) :: core       !! factory for manipulating `json_value` pointers
    type(json_value),pointer :: child 
    type(json_value),pointer :: child2
    type(json_value),pointer :: child3
    character(len=:),allocatable :: name
    
    logical :: found
    integer :: n
    character(len=:),allocatable :: string
    real :: rval

    call json%initialize()

    write(*,*) 'Load the file :'//jsonfile

    call json%load_file(filename = jsonfile)

!    call json%print_file()

    call core%initialize()
    call json%get(p) ! get root

    call core%get_child(p,child)
    call core%get_child(child,child2)
    call core%info(child2,name=name)

    write(*,*)  'Read obj data : '//name
    ncnst = core%count(child2)

    allocate( cnst_info(ncnst) )

    do n = 1,ncnst
       call core%get_child(child2, n, child3, found)
       if (found) then
          call core%get(child3,'moleculename',string)
          call cnst_info(n)%set_name(string)
          deallocate(string)

          call core%get(child3,'formula',string)
          call cnst_info(n)%set_desc(string)
          deallocate(string)

          call core%get(child3,'molecular_weight',string)
          read( string, * ) rval
          deallocate(string)
          call cnst_info(n)%set_wght(rval)
       else
          write(*,*) ' ERROR: Did not find child ',n
          call abort()
       endif
    enddo

    call json%destroy()

  end function create_cnst_info_array
  
end module json_loader
