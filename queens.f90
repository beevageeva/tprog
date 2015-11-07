PROGRAM queens
  INTEGER, PARAMETER :: dim = 8
  INTEGER, DIMENSION(dim,dim) :: tablero = 0
  INTEGER :: solutions = 0
  CALL place_queen(1)

CONTAINS

  RECURSIVE SUBROUTINE place_queen (row)
    integer, intent(in) :: row
    integer :: col
    
    if(row==dim+1) then
      call Pretty_Print
    else 
	    do col=1,dim
	      if(valid_board(row, col)) then
	        tablero(row, col) = 1
	        call place_queen(row+1)
	        tablero(row, col) = 0
	      end if
	    end do     
    end if
  END SUBROUTINE place_queen

  LOGICAL FUNCTION valid_board (row,col)
    INTEGER :: row,col
    INTEGER :: x,y

    valid_board = .TRUE.

    x = row-1
    y = col-1
    DO WHILE (x > 0 .AND. y > 0)
       IF (tablero(x,y) == 1) THEN
          valid_board = .FALSE.
          RETURN
       END IF
       x = x-1
       y = y-1
    END DO

    x = row-1
    y = col+1
    DO WHILE (x > 0 .AND. y <= dim)
       IF (tablero(x,y) == 1) THEN
          valid_board = .FALSE.
          RETURN
       END IF
       x = x-1
       y = y+1
    END DO

    IF (SUM(tablero(:,col)) == 1) THEN
       valid_board = .FALSE.
    END IF
  END FUNCTION valid_board

  SUBROUTINE Pretty_Print ()
    INTEGER :: i,j
    
    solutions = solutions + 1
    PRINT*, "------------------------"
    PRINT*, "Solution number ", solutions
    DO i = 1,dim
       DO j = 1,dim
          WRITE(*,FMT='(I1)',ADVANCE='NO') tablero(i,j)
       END DO
       WRITE(*,*)
    END DO
  END SUBROUTINE Pretty_Print

END PROGRAM queens
