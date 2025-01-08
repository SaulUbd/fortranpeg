program test_parser
    use parser
    implicit none

    character(len=100) :: filename
    character(len=:), allocatable :: inputstr
    integer :: len
    logical :: exists
    integer :: parse_result

    ! Check if the filename argument is provided
    if (command_argument_count() == 0) then
        print *, "error: no input file"
        stop
    end if

    ! Get the filename from the command line
    call get_command_argument(1, filename)

    ! Check if the file exists and get its size
    inquire(file=filename, exist=exists, size=len)
    if (exists) then
        if (len == 0) then
            print *, "error: input file is empty"
            stop
        end if

        ! Open the file and read its contents
        open(unit=1, file=filename, status='old', action='read', access='stream', form='unformatted')
        allocate(character(len=len) :: inputstr)
        read(1) inputstr
        close(1)

        ! Call the parse function with the input string
        parse_result = parse(inputstr)

        ! Print the result of the parse function
        print *, "Parse result:", parse_result
    else
        print *, "error: file is not present"
        stop
    end if
end program test_parser
