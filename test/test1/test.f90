program test
    use parser
    implicit none
    character(len=:), allocatable :: in
	integer :: out

	print *, "Type an arithmetic expression or 'exit'"

    ! type looping
    do
        allocate(character(len=100) :: in)
        ! prompt
        write(*, "(A)", advance="no") "> "
        
        read (*,*) in

        ! exit
        if (in == 'exit') then
            print *, "Exiting..."
            exit
        end if

        ! parse expression
        out = parse(in)

        ! print
        print *, out
        deallocate(in)
    end do
    
end program test