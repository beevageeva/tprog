PROGRAM listas
IMPLICIT NONE

TYPE CELL
INTEGER :: val
TYPE (CELL), POINTER :: left,right
END TYPE CELL

TYPE (CELL), POINTER :: head
INTEGER :: n,k,i

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

function createCell(val) result(res)
     TYPE (CELL), POINTER :: res
     integer, intent (in) :: val
		 allocate (res)
		 res % val= val
		 nullify (res % left)
		 nullify (res % right)

end function createCell



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





subroutine removeNode(val)
      integer, intent (in) :: val
      TYPE (CELL), POINTER :: parent
      nullify(parent)
      call findAndDeleteNode(parent, head,val, 0)
end subroutine removeNode



function deleteRoot(node) result(res)
      TYPE (CELL), POINTER :: res
      !type (cell), pointer, intent(in) :: node
      type (cell), pointer :: node
      if(.NOT. associated(node%left) .AND. .NOT. associated(node%right)) then
        nullify(res)
        deallocate(node)
      else if(.NOT. associated(node%left) .AND.  associated(node%right))then
        res => node%right
        deallocate(node)
      else if(associated(node%left) .AND. .NOT. associated(node%right))then
        res => node%left
        deallocate(node)
      else
        node % val = findAndDeleteMin(node, node % right, 1)
        res => node
      end if
  

end function deleteRoot


recursive function findAndDeleteMin(parent, node, dir) result(res)
  !type (cell), pointer, intent(in) :: node, parent
  type (cell), pointer, intent(in) ::  parent
  type (cell), pointer :: node, nodeR
  integer, intent (in) :: dir
  integer :: res
  if (.NOT. associated(node % left)) then
    res = node % val
    nodeR => node % right
    deallocate(node)
     if (dir == -1) then
      parent % left => nodeR
    else
      parent % right => nodeR
    end if
  else
    res = findAndDeleteMin(node, node % left, -1)
  end if
end function findAndDeleteMin

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

recursive function countElem (node) result (res)
	 type (cell), pointer, intent(in) :: node
   integer :: res 
   if (associated(node)) then
    res = countElem(node % left) + 1 + countElem(node % right) 
   else
    res = 0
   end if 
end function countElem

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
