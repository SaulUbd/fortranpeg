program test
	use parser
	implicit none
	character(len=100) :: filename
	character(len=:), allocatable :: inputstr
	integer :: u, len
	logical :: exists
	integer :: output

	if (command_argument_count() == 0) then
		print *, "error: no input file"
		stop
	end if

	call get_command_argument(1, filename)

	inquire(file=filename, exist=exists, size=len)
	if (exists) then
		open (1, file=filename, status='old', action='read', access='stream', form='unformatted')
		allocate (character(len=len) :: inputstr)
        read (1) inputstr
		output = parse(inputstr)
		print *, output
	else
		print *, "error: file is not present"
		stop
	end if

	close(u)

end program test