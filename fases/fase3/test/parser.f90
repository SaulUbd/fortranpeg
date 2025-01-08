
!auto-generated
module parser
  implicit none
  character(len=:), allocatable, private :: input
  integer, private :: savePoint, lexemeStart, cursor

  interface toStr
      module procedure intToStr
      module procedure strToStr
  end interface
  
  

  contains
  
  

  function parse(str) result(res)
      character(len=:), allocatable :: str
      integer :: res

      input = str
      cursor = 1

      res = peg_s()
  end function parse

  
  recursive function peg_s() result (res)
      integer :: res
      integer :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_e()
              if (.not. acceptEOF()) cycle
              
              res = peg_s_f0(expr_0_0)

              
               


              exit
          
          case default
              call pegError()
          end select
      end do

  end function peg_s

   function peg_s_negative() result (res)
      integer :: res
      integer :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote

      savePoint = cursor
       
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_e()
              if (.not. acceptEOF()) cycle
              
              res = peg_s_f0(expr_0_0)

              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_s_negative

  function peg_s_kleene() result (res)
      integer :: res
      integer :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_e()
              if (.not. acceptEOF()) cycle
              
              res = peg_s_f0(expr_0_0)

              
               


              exit
          
          case default
        res = -99999
          end select
      end do

  end function peg_s_kleene


  recursive function peg_e() result (res)
      integer :: res
      integer :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
integer :: expr_1_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_t()

               lexemeStart = cursor
               if(.not. acceptString('+')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_e()
              
              
              res = peg_e_f0(expr_0_0, expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg_t()
              
              
              res = expr_1_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

  end function peg_e

   function peg_e_negative() result (res)
      integer :: res
      integer :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
integer :: expr_1_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote

      savePoint = cursor
       
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_t()

               lexemeStart = cursor
               if( acceptString('+')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_e()
              
              
              res = peg_e_f0(expr_0_0, expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg_t()
              
              
              res = expr_1_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_e_negative

  function peg_e_kleene() result (res)
      integer :: res
      integer :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
integer :: expr_1_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_t()

               lexemeStart = cursor
               if(.not. acceptString('+')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_e()
              
              
              res = peg_e_f0(expr_0_0, expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg_t()
              
              
              res = expr_1_0
   
              
               


              exit
          
          case default
        res = -99999
          end select
      end do

  end function peg_e_kleene


  recursive function peg_t() result (res)
      integer :: res
      integer :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
integer :: expr_1_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_f()

               lexemeStart = cursor
               if(.not. acceptString('*')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_t()
              
              
              res = peg_t_f0(expr_0_0, expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg_f()
              
              
              res = expr_1_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

  end function peg_t

   function peg_t_negative() result (res)
      integer :: res
      integer :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
integer :: expr_1_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote

      savePoint = cursor
       
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_f()

               lexemeStart = cursor
               if( acceptString('*')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_t()
              
              
              res = peg_t_f0(expr_0_0, expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg_f()
              
              
              res = expr_1_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_t_negative

  function peg_t_kleene() result (res)
      integer :: res
      integer :: expr_0_0
character(len=:), allocatable :: expr_0_1
integer :: expr_0_2
integer :: expr_1_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_f()

               lexemeStart = cursor
               if(.not. acceptString('*')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_t()
              
              
              res = peg_t_f0(expr_0_0, expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg_f()
              
              
              res = expr_1_0
   
              
               


              exit
          
          case default
        res = -99999
          end select
      end do

  end function peg_t_kleene


  recursive function peg_f() result (res)
      integer :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg__()

               lexemeStart = cursor
               if (.not. (acceptRange('0', '9'))) cycle
               do while (.not. cursor > len(input))
                   if (.not. (acceptRange('0', '9'))) exit
               end do
               expr_0_1 = consumeInput()
           
expr_0_2 = peg__()
              
              
              res = peg_f_f0(expr_0_1)

              
               


              exit
          
          case default
              call pegError()
          end select
      end do

  end function peg_f

   function peg_f_negative() result (res)
      integer :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote

      savePoint = cursor
       
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg__()

               lexemeStart = cursor
               if (.not. (acceptRange('0', '9'))) cycle
               do while (.not. cursor > len(input))
                   if (.not. (acceptRange('0', '9'))) exit
               end do
               expr_0_1 = consumeInput()
           
expr_0_2 = peg__()
              
              
              res = peg_f_f0(expr_0_1)

              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_f_negative

  function peg_f_kleene() result (res)
      integer :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg__()

               lexemeStart = cursor
               if (.not. (acceptRange('0', '9'))) cycle
               do while (.not. cursor > len(input))
                   if (.not. (acceptRange('0', '9'))) exit
               end do
               expr_0_1 = consumeInput()
           
expr_0_2 = peg__()
              
              
              res = peg_f_f0(expr_0_1)

              
               


              exit
          
          case default
        res = -99999
          end select
      end do

  end function peg_f_kleene


  recursive function peg__() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              
                lexemeStart = cursor
                do while (.not. cursor > len(input))
                     if (.not. (acceptSet([char(32),char(9),char(10),char(13)]))) exit
                end do
                expr_0_0 = consumeInput()
              
              
              
              res = expr_0_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

  end function peg__

   function peg___negative() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote

      savePoint = cursor
       
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              
                lexemeStart = cursor
                do while (.not. cursor > len(input))
                     if (.not. (acceptSet([char(32),char(9),char(10),char(13)]))) exit
                end do
                expr_0_0 = consumeInput()
              
              
              
              res = expr_0_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg___negative

  function peg___kleene() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote

      savePoint = cursor
      
      do i = 0, 0
          select case(i)
          
          case(0)
              cursor = savePoint
              
              
                lexemeStart = cursor
                do while (.not. cursor > len(input))
                     if (.not. (acceptSet([char(32),char(9),char(10),char(13)]))) exit
                end do
                expr_0_0 = consumeInput()
              
              
              
              res = expr_0_0
   
              
               


              exit
          
          case default
        res = ""
          end select
      end do

  end function peg___kleene


  
  function peg_s_f0(out) result(res)
      integer :: out
      integer :: res
      
    res = out

  end function peg_s_f0
  

  function peg_e_f0(left, right) result(res)
      integer :: left
integer :: right
      integer :: res
      
    res = left + right

  end function peg_e_f0
  

  function peg_t_f0(left, right) result(res)
      integer :: left
integer :: right
      integer :: res
      
    res = left * right

  end function peg_t_f0
  

  function peg_f_f0(num) result(res)
      character(len=:), allocatable :: num
      integer :: res
      
    read(num, *) res

  end function peg_f_f0
  

  function acceptString(str) result(accept)
      character(len=*) :: str
      logical :: accept
      integer :: offset

      offset = len(str) - 1
      if (str /= input(cursor:cursor + offset)) then
          accept = .false.
          return
      end if
      cursor = cursor + len(str)
      accept = .true.
  end function acceptString

  function acceptStringCI(str) result(accept)
       character(len=*) :: str
       logical :: accept
       integer :: offset
       character(len=len(str)) :: lower_input
   
       offset = len(str) - 1
       lower_input = tolower(input(cursor:cursor + offset))
       if (tolower(str) /= lower_input) then
           accept = .false.
           return
       end if
       cursor = cursor + len(str)
       accept = .true.
   end function acceptStringCI


  function acceptRange(bottom, top) result(accept)
      character(len=1) :: bottom, top
      logical :: accept

      if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
          accept = .false.
          return
      end if
      cursor = cursor + 1
      accept = .true.
  end function acceptRange

  function acceptRangeCI(bottom, top) result(accept)
       character(len=1) :: bottom, top
       logical :: accept
       character(len=1) :: lower_input

       lower_input = tolower(input(cursor:cursor))
       if (.not. (lower_input >= tolower(bottom) .and. lower_input <= tolower(top))) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptRangeCI

  function acceptSet(set) result(accept)
      character(len=1), dimension(:) :: set
      logical :: accept

      if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
          accept = .false.
          return
      end if
      cursor = cursor + 1
      accept = .true.
  end function acceptSet

   

   function acceptSetCI(set) result(accept)
       character(len=1), dimension(:) :: set
       logical :: accept
       character(len=1) :: lower_input

       lower_input = tolower(input(cursor:cursor))
       if (.not. (findloc(set, lower_input, 1) > 0)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptSetCI

  function acceptPeriod() result(accept)
      logical :: accept

      if (cursor > len(input)) then
          accept = .false.
          return
      end if
      cursor = cursor + 1
      accept = .true.
  end function acceptPeriod

  function acceptEOF() result(accept)
      logical :: accept

      if(.not. cursor > len(input)) then
          accept = .false.
          return
      end if
      accept = .true.
  end function acceptEOF

  function consumeInput() result(substr)
      character(len=:), allocatable :: substr

      substr = input(lexemeStart:cursor - 1)
  end function consumeInput

  subroutine pegError()
      print '(A,I1,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

      call exit(1)
  end subroutine pegError

  function intToStr(int) result(cast)
      integer :: int
      character(len=31) :: tmp
      character(len=:), allocatable :: cast
      if (int == -99999) then
             cast = ""
             return
        end if
       if (int == -9999) then
               cast = ""
               return
            end if
      write(tmp, '(I0)') int
      cast = trim(adjustl(tmp))
  end function intToStr

  function strToStr(str) result(cast)
      character(len=:), allocatable :: str
      character(len=:), allocatable :: cast

      cast = str
  end function strToStr

  function strToInt(str) result(cast)
        character(len=:), allocatable :: str
           integer :: cast
           if (len(trim(str)) == 0) then
               cast = -9999
               return
           end if
           read(str, *) cast
   end function strToInt

   function acceptStringDelim(str) result(accept)
     character(len=*) :: str
     logical :: accept
     integer :: offset
     character(len=:), allocatable :: beforeDelim, afterDelim, prueba

     offset = len(str) - 1

         ! Verificar si el delimitador coincide con la parte actual de la cadena
             if (str /= input(cursor:cursor + offset)) then
                 accept = .false.
                 return
             end if

             ! Realizar el corte
                 beforeDelim = input(1:cursor-1)       ! Parte antes del lexemeStart

             afterDelim = input(cursor+len(str):)           ! Parte después del delimitador

             input =beforeDelim // afterDelim
             
             ! Mover el cursor después del delimitador
             accept = .true.
       end function acceptStringDelim



  function tolower(str) result(lower_str)
       character(len=*), intent(in) :: str
       character(len=len(str)) :: lower_str
       integer :: i

       lower_str = str 
       do i = 1, len(str)
           if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
               lower_str(i:i) = achar(iachar(str(i:i)) + 32)
           end if
       end do
   end function tolower

end module parser
