PROGRAM practica
  IMPLICIT NONE

  TYPE CELL
     INTEGER :: val
     TYPE (CELL), POINTER :: left,right
  END TYPE CELL

  TYPE (CELL), POINTER :: head
  INTEGER              :: n,k,i

  READ*, n

  ALLOCATE(head)
  head%val = 0
  NULLIFY(head%left)
  NULLIFY(head%right)

  DO i=1,n
     READ*, k
     CALL place_number(head,k)
  END DO

  CALL Print(head)

  CALL borra(head,3)

  CALL Print(head)

CONTAINS
  
  RECURSIVE FUNCTION in_order_successor(node) RESULT (tnode)
    TYPE (CELL), POINTER :: node,tnode
  
!!! ???????????                                                                                                                                                                     

  END FUNCTION in_order_successor

  RECURSIVE FUNCTION get_parent (node,number) RESULT (tnode)
    TYPE (CELL), POINTER :: node,tnode
    INTEGER :: number

!!! ???????????                                                                                                                                                                     
  
  END FUNCTION get_parent
  
  RECURSIVE SUBROUTINE borra(head,number)
    TYPE (CELL), POINTER :: head, parent, node, successor
    INTEGER :: number, exchange

    parent => get_parent(head,number)

    !! Find the node to actually delete                                                                                                                                            
    IF (ASSOCIATED(parent%left)) THEN
       IF (parent%left%val .EQ. number) THEN
          node => parent%left
       ELSE
          node => parent%right
       END IF
    ELSE
       node => parent%right
    END IF

    !! Do the actual delete                                                                                                                                                        
    IF (.NOT. ASSOCIATED(node%left) .AND. .NOT. ASSOCIATED(node%right))
THEN
       !! The easiest. This is a leaf                                                                                                                                              
       IF (ASSOCIATED(parent%left)) THEN
          IF (parent%left%val .EQ. number) THEN
             NULLIFY(parent%left)
          ELSE
             NULLIFY(parent%right)
          END IF
       ELSE
          NULLIFY(parent%right)
       END IF
       DEALLOCATE(node)
    ELSEIF (.NOT. ASSOCIATED(node%left)) THEN
       !! tiene arbol derecho
       !!!! ???????????                                                                                                                                                                                  

    ELSEIF (.NOT. ASSOCIATED(node%right)) THEN
       !! tiene arbol izquierdo
       !!!! ???????????                                                                                                                                                                                  
    ELSE
       !! el caso general
       !!!! ???????????   
    END IF

  END SUBROUTINE borra

  RECURSIVE SUBROUTINE place_number(node,number)
    TYPE (CELL), POINTER :: node, temp
    INTEGER :: number

    IF (number < node%val) THEN
       IF (ASSOCIATED(node%left)) THEN
          CALL place_number(node%left,number)
       ELSE
          ALLOCATE(temp)
          node%left => temp
          temp%val = number
          NULLIFY(temp%left)
          NULLIFY(temp%right)
       END IF
    ELSE IF (number > node%val) THEN
       IF (ASSOCIATED(node%right)) THEN
          CALL place_number(node%right,number)
       ELSE
          ALLOCATE(temp)
          node%right => temp
          temp%val = number
          NULLIFY(temp%left)
          NULLIFY(temp%right)
       END IF
    ELSE
       PRINT*, "No se admiten numeros repetidos, ignorando el numero
",number, " !"
    END IF
  END SUBROUTINE place_number

  RECURSIVE SUBROUTINE Print (node)
    TYPE (CELL), POINTER :: node

    IF (ASSOCIATED(node%left)) CALL Print (node%left)
    PRINT*, node%val
    IF (ASSOCIATED(node%right)) CALL Print (node%right)

  END SUBROUTINE Print

END PROGRAM practica
