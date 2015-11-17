program hello

   implicit none
   type cell
      real :: val
      type (cell), pointer :: prev, next
   end type  cell

   type (cell), target :: head
   type (cell), pointer :: curr, temp, t, tprev
   integer :: i, n

   real :: val


   print*, "Num elem"
   read*, n 
   if(n<=0) then
    call EXIT(0)
   end if
      
   read*, val 
   head%val = val
   nullify(head%prev) 
   nullify(head%next) 
   curr=>head
   do i=2,n
      read*, val
      allocate(temp)
      temp%val = val
      !search element where to insert
      t => head      
      do while(associated(t) .AND. t%val<=val)
         t => t % next
      end do
      ! t.prev --  temp -- t
      if(.NOT. associated(t)) then
        tprev => curr
      else
        tprev => t % prev
      end if
      temp % next => t
      temp % prev => tprev
      tprev % next => temp
      tprev => temp
        
       
   end do


    PRINT*, " Backwards..."
    CALL printList (curr,0)

    PRINT*, " Forward..."
    curr => head
    CALL printList (curr,1)


    

contains


   recursive subroutine printList (t, order)

      type (cell), pointer :: t 
      integer, intent (in) :: order 

      if (associated (t)) then
         print *, t % val
          if (order == 0) then
           call printList (t % prev, order)
          else
           call printList (t % next, order)
          end if 
      end if

   end subroutine printList



end program hello
