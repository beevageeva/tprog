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

Print*, "head val", (head % val)


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

recursive function findMin(node) result(res)
  TYPE (CELL), POINTER :: res
  if (.NOT. associated(node % left)) then
    res = node % val
  else
    res = findMin(node % left)
  end if
end function findMin

recursive subroutine removeNode(node, val)
      !http://www.algolist.net/Data_structures/Binary_search_tree/Removal
      type (cell), pointer, intent(in) :: node
      integer, intent (in) :: val
      ! find node


end subroutine removeNode






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

END PROGRAM listas
