PROGRAM listas
IMPLICIT NONE

! the bst node type definition
TYPE CELL
INTEGER :: val
TYPE (CELL), POINTER :: left,right
END TYPE CELL

TYPE (CELL), POINTER :: head
INTEGER :: n,k,i

!input data:
!n  
!a1
!a2
!...
!an
!n = number of elements (n >= 1)
!a1, a2,..an integer(see cell type definition) values to insert in the bst


READ*, n

READ*, k
ALLOCATE(head)
head%val = k
NULLIFY(head%left)
NULLIFY(head%right)

DO i=2,n
READ*, k
CALL place_number(head,k)
END DO

CALL Print(head)

Print*, "Count:", countElem(head)
Print*, "Depth:", getDepth(head)

Print*, "head val", (head % val)

call removeNode(100)
call removeNode(200)
call removeNode(1001)
call removeNode(999)
call removeNode(head % val)
call removeNode(head % val)
call removeNode(head % val)
call removeNode(head % val)
call removeNode(head % val)
call removeNode(head % val)
call removeNode(499)
call removeNode(10)
call removeNode(6)

CALL Print(head)
Print*, "Count2:", countElem(head)
Print*, "Depth:", getDepth(head)
Print*, "head2 val", (head % val)

CONTAINS 

! this function creates a tree node with the value from parameter val
!params: val: integer : the value stored in the cell
!returns a pointer to the cell created
function createCell(val) result(res)
     TYPE (CELL), POINTER :: res
     integer, intent (in) :: val
		 allocate (res)
		 res % val= val
		 nullify (res % left)
		 nullify (res % right)

end function createCell

!another recursive function which inserts a value in the subtree with root t. The other one is
!place_number. The code is  shorter(and no call to createCell) and see the declaration of t not intent(in)
!which makes unnecessarily  explicitly pointing parent left or right to the new
!created node
   recursive subroutine insert (t, val)

      type (cell), pointer :: t  
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

!recursive function which inserts a value in the subtree with root node. 
!params: node: pointer to a cell  - the root of the tree where to insert
!         val: integer - the value to insert
RECURSIVE SUBROUTINE place_number(node,val)
      type (cell), pointer, intent(in) :: node
      integer, intent (in) :: val
      ! no duplicates
      if(val .NE. node % val) then
	      if (val < node % val) then
            if (.NOT. associated(node % left)) then
              node % left => createCell(val)
            else
              call place_number(node % left, val)
            end if
        else
        !val > node % val
            if (.NOT. associated(node % right)) then
              node % right => createCell(val)
            else
              call place_number(node % right, val)
            end if
        end if
       end if
END SUBROUTINE place_number




!subroutine which deletes val in the subtree specified by global variable head
!params: val: integer - the value to remove
subroutine removeNode(val)
      integer, intent (in) :: val
      TYPE (CELL), POINTER :: parent
      nullify(parent)
      call findAndDeleteNode(parent, head,val, 0)
end subroutine removeNode


!function which deletes the root of the subtree node
!params: node the root of the subtree
!returns: the tree(a pointer to the new root) with root node removed
!There are 3 cases: the node has no children, the node has one child and the
!node has both children 
function deleteRoot(node) result(res)
      TYPE (CELL), POINTER :: res
      !type (cell), pointer, intent(in) :: node
      type (cell), pointer :: node
      ! when node has no chirden we simply delete it
      if(.NOT. associated(node%left) .AND. .NOT. associated(node%right)) then
        nullify(res)
        deallocate(node)
      ! if node has only right child delete the node and return the child as new root 
      else if(.NOT. associated(node%left) .AND.  associated(node%right))then
        res => node%right
        deallocate(node)
      ! if node has only left child delete the node and return the child as new root 
      else if(associated(node%left) .AND. .NOT. associated(node%right))then
        res => node%left
        deallocate(node)
      else
        ! if node has both children put in the node the value of min of right
        ! subtree (we can put the max of left subtree as well) because we want
        ! to preserve the ordering and delete the min 
        node % val = findAndDeleteMin(node, node % right, 1)
        res => node
      end if
  

end function deleteRoot


!recursive function which find and deletes the minimum value in the subtree (with root) node
! params: parent - node parent
!         node -  the subtree root
!         dir = -1 if node = parent%left and dir = 1 if node = parent%right(this case only happens the first time when I start to look
!         in the right subtree of the node I want to remove. Afterwards we have to descend
!         recursively in the left subtree until there is no left child - this is
!         the minimum, but this last node may have a right subtree!).       
recursive function findAndDeleteMin(parent, node, dir) result(res)
  !type (cell), pointer, intent(in) :: node, parent
  type (cell), pointer, intent(in) ::  parent
  type (cell), pointer :: node, nodeR
  integer, intent (in) :: dir
  integer :: res
  if (.NOT. associated(node % left)) then
    ! this is the minimum
    res = node % val
    ! if the node (with the minimum) has a right subtree, we have to attach it
    ! to the parent
    nodeR => node % right
    deallocate(node)
     if (dir == -1) then
      parent % left => nodeR
    else
      parent % right => nodeR
    end if
  else
    ! always go in the left subtree
    res = findAndDeleteMin(node, node % left, -1)
  end if
end function findAndDeleteMin


!recursive subroutine which searches a value in the subtree node and deletes it
!The subtree with the root containing the value to remove will be modified (as a
!result of the !function deleteRoot) and assigned to parent%left or parent%right 
!(depending on the value of the parameter dir)
!If the value is not in the tree it prints a message
!params: node - root of the subtree
!        parent - node parent 
!        val - the value to search
!        dir = 0 if node = head of the tree (no parent), dir = -1 if node =
!        parent%left, dir = 1 if node = parent%right
!TODO Instead of keeping track of parent and dir (as function parameters) maybe we
!could add field parent in the cell type 
recursive subroutine findAndDeleteNode(parent, node, val, dir) 
      TYPE (CELL), POINTER :: res
      type (cell), pointer, intent(in) :: node, parent
      integer, intent (in) :: dir,val
      ! find node
      if(node%val==val) then
        if (dir == 0) then
          ! head deleted
          head => deleteRoot(node)  
        else if (dir == -1) then
          parent % left => deleteRoot(node)
        else
          parent % right => deleteRoot(node)
        end if
      else
	      if(val < node%val) then
          if (.NOT. associated(node % left)) then
            print*, "not found , less than ", node % val
          else
            call findAndDeleteNode(node, node%left, val,-1)
          end if
	      else
	      !node%val>val
          if (.NOT. associated(node % right)) then
            print*, "not found more than ", node % val
          else
            call findAndDeleteNode(node, node%right, val,1)
          end if
	      end if
      end if



end subroutine findAndDeleteNode






!recursive  subroutine which prints in-order the subtree node
RECURSIVE SUBROUTINE Print (node) 
	 type (cell), pointer, intent(in) :: node
   if (associated(node % left)) then
	  call Print (node % left)
   end if 
	 print *, node % val
   if (associated(node % right)) then
	  call Print (node % right)
   end if
END SUBROUTINE Print

!recursive  function which returns the number of elements of the subtree node
recursive function countElem (node) result (res)
	 type (cell), pointer, intent(in) :: node
   integer :: res 
   if (associated(node)) then
    res = countElem(node % left) + 1 + countElem(node % right) 
   else
    res = 0
   end if 
end function countElem

!recursive  function which returns the depth of the subtree node
recursive function getDepth (node) result (res)
	 type (cell), pointer, intent(in) :: node
   integer :: res 
   if (associated(node)) then
    res = 1 + max(getDepth(node % left),  getDepth(node % right)) 
   else
    res = 0
   end if 
end function getDepth

END PROGRAM listas
