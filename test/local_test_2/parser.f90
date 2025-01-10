
!auto-generated
module parser
  implicit none
  character(len=:), allocatable, private :: input
  integer, private :: lexemeStart, cursor

  interface toStr
      module procedure intToStr
      module procedure strToStr
  end interface
  
  
    integer :: id = 0
   

  contains
  
  function temp() result(res)
        character(len=:), allocatable :: res
        character(len=32) :: temp_str
        id = id + 1
        write(temp_str, '(A, I0)') "t", id
        res = trim(adjustl(temp_str)) 
    end function temp


  function parse(str) result(res)
      character(len=:), allocatable :: str
      character(len=:), allocatable :: res

      input = str
      cursor = 1

      res = peg_s()
  end function parse

  
  recursive function peg_s() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint
    print *, "LLAMADA A 'S'"

      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = ""
                        temp = "-"
                        do while (.not. temp == "" .and. len(input) >= cursor)
                            temp = peg_group_0_kleene()
                            expr_0_0 = expr_0_0 // temp
                        end do
                        
              if (.not. acceptEOF()) cycle
              
              res = expr_0_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do
    print *, "FIN S"

  end function peg_s

   function peg_s_negative() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
       
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = ""
                        temp = "-"
                        do while (.not. temp == "" .and. len(input) >= cursor)
                            temp = peg_group_0_kleene()
                            expr_0_0 = expr_0_0 // temp
                        end do
                        
              if (.not. acceptEOF()) cycle
              
              res = expr_0_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_s_negative

  function peg_s_kleene() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = ""
                        temp = "-"
                        do while (.not. temp == "" .and. len(input) >= cursor)
                            temp = peg_group_0_kleene()
                            expr_0_0 = expr_0_0 // temp
                        end do
                        
              if (.not. acceptEOF()) cycle
              
              res = expr_0_0
   
              
               


              exit
          
          case default
        res = ""
          end select
      end do

  end function peg_s_kleene


  recursive function peg_e() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_2_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint

  print *, "LLAMADA A 'E'"
      savePoint = cursor
      
      do i = 0, 3
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

               lexemeStart = cursor
               if(.not. acceptString('-')) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg_e()
              
              
              res = peg_e_f1(expr_1_0, expr_1_2)

              
               


              exit
          
          case(2)
              cursor = savePoint
              
              expr_2_0 = peg_t()
              
              
              res = expr_2_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

    print *, "FIN E"
  end function peg_e

   function peg_e_negative() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_2_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
       
      do i = 0, 3
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

               lexemeStart = cursor
               if( acceptString('-')) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg_e()
              
              
              res = peg_e_f1(expr_1_0, expr_1_2)

              
               


              exit
          
          case(2)
              cursor = savePoint
              
              expr_2_0 = peg_t()
              
              
              res = expr_2_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_e_negative

  function peg_e_kleene() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_2_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
      
      do i = 0, 3
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

               lexemeStart = cursor
               if(.not. acceptString('-')) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg_e()
              
              
              res = peg_e_f1(expr_1_0, expr_1_2)

              
               


              exit
          
          case(2)
              cursor = savePoint
              
              expr_2_0 = peg_t()
              
              
              res = expr_2_0
   
              
               


              exit
          
          case default
        res = ""
          end select
      end do

  end function peg_e_kleene


  recursive function peg_t() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_2_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint
      print *, "LLAMADA A T"

      savePoint = cursor
      
      do i = 0, 3
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

               lexemeStart = cursor
               if(.not. acceptString('/')) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg_t()
              
              
              res = peg_t_f1(expr_1_0, expr_1_2)

              
               


              exit
          
          case(2)
              cursor = savePoint
              
              expr_2_0 = peg_f()
              
              
              res = expr_2_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

    print *, "FIN T"
  end function peg_t

   function peg_t_negative() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_2_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
       
      do i = 0, 3
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

               lexemeStart = cursor
               if( acceptString('/')) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg_t()
              
              
              res = peg_t_f1(expr_1_0, expr_1_2)

              
               


              exit
          
          case(2)
              cursor = savePoint
              
              expr_2_0 = peg_f()
              
              
              res = expr_2_0
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_t_negative

  function peg_t_kleene() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
character(len=:), allocatable :: expr_2_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
      
      do i = 0, 3
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

               lexemeStart = cursor
               if(.not. acceptString('/')) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg_t()
              
              
              res = peg_t_f1(expr_1_0, expr_1_2)

              
               


              exit
          
          case(2)
              cursor = savePoint
              
              expr_2_0 = peg_f()
              
              
              res = expr_2_0
   
              
               


              exit
          
          case default
        res = ""
          end select
      end do

  end function peg_t_kleene


  recursive function peg_f() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint
      print *, "LLAMADA A F"

      savePoint = cursor
      
      do i = 0, 2
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg__()

               lexemeStart = cursor
               if(.not. acceptString('(')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_e()

               lexemeStart = cursor
               if(.not. acceptString(')')) cycle
               expr_0_3 = consumeInput()
       
expr_0_4 = peg__()
              
              
              res = peg_f_f0(expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg__()

               lexemeStart = cursor
               if(.not. (acceptRange('0', '9'))) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg__()
              
              
              res = peg_f_f1(expr_1_1)

              
               


              exit
          
          case default
              call pegError()
          end select
      end do

    print *, "FIN F"
  end function peg_f

   function peg_f_negative() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
       
      do i = 0, 2
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg__()

               lexemeStart = cursor
               if( acceptString('(')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_e()

               lexemeStart = cursor
               if( acceptString(')')) cycle
               expr_0_3 = consumeInput()
       
expr_0_4 = peg__()
              
              
              res = peg_f_f0(expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg__()

               lexemeStart = cursor
               if( (acceptRange('0', '9'))) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg__()
              
              
              res = peg_f_f1(expr_1_1)

              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_f_negative

  function peg_f_kleene() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
character(len=:), allocatable :: expr_0_3
character(len=:), allocatable :: expr_0_4
character(len=:), allocatable :: expr_1_0
character(len=:), allocatable :: expr_1_1
character(len=:), allocatable :: expr_1_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
      
      do i = 0, 2
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg__()

               lexemeStart = cursor
               if(.not. acceptString('(')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg_e()

               lexemeStart = cursor
               if(.not. acceptString(')')) cycle
               expr_0_3 = consumeInput()
       
expr_0_4 = peg__()
              
              
              res = peg_f_f0(expr_0_2)

              
               


              exit
          
          case(1)
              cursor = savePoint
              
              expr_1_0 = peg__()

               lexemeStart = cursor
               if(.not. (acceptRange('0', '9'))) cycle
               expr_1_1 = consumeInput()
       
expr_1_2 = peg__()
              
              
              res = peg_f_f1(expr_1_1)

              
               


              exit
          
          case default
        res = ""
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
      integer :: savePoint
      print *, "LLAMADA A _"

      savePoint = cursor
      
      do i = 0, 1
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

    print *, "FIN _"
  end function peg__

   function peg___negative() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
       
      do i = 0, 1
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
      integer :: savePoint

      savePoint = cursor
      
      do i = 0, 1
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


  
  recursive function peg_group_0() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_e()

               lexemeStart = cursor
               if(.not. acceptString(';')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg__()
              
              
              res = expr_0_0//expr_0_1//expr_0_2
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

  end function peg_group_0

   function peg_group_0_negative() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps
      integer :: i, tempi
      logical :: pivote
      integer :: savePoint

      savePoint = cursor
       
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_e()

               lexemeStart = cursor
               if( acceptString(';')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg__()
              
              
              res = expr_0_0//expr_0_1//expr_0_2
   
              
               


              exit
          
          case default
              call pegError()
          end select
      end do

   end function peg_group_0_negative

  function peg_group_0_kleene() result (res)
      character(len=:), allocatable :: res
      character(len=:), allocatable :: expr_0_0
character(len=:), allocatable :: expr_0_1
character(len=:), allocatable :: expr_0_2
      character(len=:), allocatable :: temp
      integer :: count, min_reps, max_reps, tempi
      integer :: i
      logical :: pivote
      integer :: savePoint

      print *, "LLAMADA A GRUPO (e ';' _)"
      savePoint = cursor
      
      do i = 0, 1
          select case(i)
          
          case(0)
              cursor = savePoint
              
              expr_0_0 = peg_e()

               lexemeStart = cursor
               if(.not. acceptString(';')) cycle
               expr_0_1 = consumeInput()
       
expr_0_2 = peg__()
              
              
              res = expr_0_0//expr_0_1//expr_0_2
   
              
               


              exit
          
          case default
        res = ""
          end select
      end do

    print *, "FIN GRUPO (e ';' _)"
  end function peg_group_0_kleene


  
  function peg_e_f0(left, right) result(res)
      character(len=:), allocatable :: left
character(len=:), allocatable :: right
      character(len=:), allocatable :: res
      
        res = temp()
        print *, res // " = " // left // " + " // right
    
  end function peg_e_f0
  

  function peg_e_f1(left, right) result(res)
      character(len=:), allocatable :: left
character(len=:), allocatable :: right
      character(len=:), allocatable :: res
      
        res = temp()
        print *, res // " = " // left // " - " // right
    
  end function peg_e_f1
  

  function peg_t_f0(left, right) result(res)
      character(len=:), allocatable :: left
character(len=:), allocatable :: right
      character(len=:), allocatable :: res
      
        res = temp()
        print *, res // " = " // left // " * " // right 
    
  end function peg_t_f0
  

  function peg_t_f1(left, right) result(res)
      character(len=:), allocatable :: left
character(len=:), allocatable :: right
      character(len=:), allocatable :: res
      
        res = temp()
        print *, res // " = " // left // " / " // right 
    
  end function peg_t_f1
  

  function peg_f_f0(exp) result(res)
      character(len=:), allocatable :: exp
      character(len=:), allocatable :: res
      
        res = exp
    
  end function peg_f_f0
  

  function peg_f_f1(num) result(res)
      character(len=:), allocatable :: num
      character(len=:), allocatable :: res
      
    res = num

  end function peg_f_f1
  


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
