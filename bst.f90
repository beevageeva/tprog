program tree_sort
!http://www.fortran.com/fortran_storenew/Html/Info/books/gd3_c08.html
! Sorts a file of reals by building a
! tree, sorted in infix order.
! This sort has expected behavior n log n,
! but worst case (input is sorted) n ** 2.

   implicit none
   type tree
      real :: val
      type (tree), pointer :: left, right
   end type tree

   type (tree), pointer :: t  ! A tree
   real :: val

   nullify (t)  ! Start with empty tree

   read*, val 
   do while(val .NE. -1)
      call insert (t, val) ! Put next number in tree
      read*, val 
   end do
   ! Print trees of tree in infix order
   call print_tree (t)
    print*, "another print tree"
    ! in the other function associated test (for root) is inside
   if(associated(t)) then 
    call printTree (t)
   end if
    

contains

   recursive subroutine insert (t, val)

      type (tree), pointer :: t  ! A tree
      real, intent (in) :: val

      ! If (sub)tree is empty, put val at root
      if (.not. associated (t)) then
         allocate (t)
         t % val= val
         nullify (t % left)
         nullify (t % right)
      !do not insert duplicates
      else if(val .NE. t % val) then
      ! Otherwise, insert into correct subtree
	      if (val < t % val) then
	         call insert (t % left, val)
	      else
	         call insert (t % right, val)
	      end if
      end if

   end subroutine insert

   recursive subroutine print_tree (t)
   ! Print tree in infix order

      type (tree), pointer :: t  ! A tree

      if (associated (t)) then
         call print_tree (t % left)
         print *, t % val
         call print_tree (t % right)
      end if

   end subroutine print_tree

! campus virtual functions
  logical function existe_rama_izq(t)
      type (tree), pointer , intent(in) :: t  
      existe_rama_izq = associated(t % left)
  end function existe_rama_izq

  logical function existe_rama_dca(t)
      type (tree), pointer , intent(in) :: t  
      existe_rama_dca = associated(t % right)
  end function existe_rama_dca

 function rama_izq(t) result(l)
      type (tree), pointer, intent(in) :: t  
      type (tree), pointer :: l
      l => t % left
  end function rama_izq

 function rama_dca(t) result(r)
      type (tree), pointer, intent(in) :: t  
      type (tree), pointer :: r
      r => t % right
  end function rama_dca
  
  real function valor_nodo(t)
      type (tree), pointer, intent(in) :: t  
      valor_nodo = t % val
  end function valor_nodo
 
  
   recursive subroutine printTree(t)
      type (tree), pointer, intent(in) :: t  
      if (existe_rama_izq (t)) then
         call printTree (rama_izq(t))
      end if
      print *, valor_nodo(t)
      if (existe_rama_dca (t)) then
         call printTree (rama_dca(t))
      end if

   end subroutine printTree


end program tree_sort
