!  trap.f -- Parallel Trapezoidal Rule, first version
!
! Slightly modified by Angel de Vicente
!
!  Input: None.
!  Output:  Estimate of the integral from a to b of f(x) 
!     using the trapezoidal rule and n trapezoids.
! 
!  Algorithm:
!     1.  Each process calculates "its" interval of 
!         integration.
!     2.  Each process estimates the integral of f(x)
!         over its interval using the trapezoidal rule.
!     3a. Each process != 0 sends its integral to 0.
!     3b. Process 0 sums the calculations received from
!         the individual processes and prints the result.
! 
!  Notes:  
!     1.  f(x), a, b, and n are all hardwired.
!     2.  Assumes number of processes (p) evenly divides
!         number of trapezoids (n = 1024)
!
!  See Chap. 4, pp. 56 & ff. in PPMPI.
!
PROGRAM trapezoidal
  USE MPI
  INTEGER :: n=1024, dest=0, tag=0
  REAL :: a=0.0, b=1.0
  INTEGER :: my_rank, p, local_n, source, status(MPI_STATUS_SIZE), ierr
  REAL :: h, local_a, local_b, integral, total
  
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, p, ierr)

  print* , "num processes: ", p
  
  h = (b-a)/n
  local_n = n/p
  
  local_a = a + my_rank*local_n*h
  local_b = local_a + local_n*h
  integral = Trap(local_a, local_b, local_n, h)
  
  IF (my_rank .EQ. 0) THEN
     total = integral
     DO source = 1, p-1
        CALL MPI_RECV(integral, 1, MPI_REAL, source, tag, MPI_COMM_WORLD, status, ierr)
        total = total + integral
     END DO
  ELSE
     CALL MPI_SEND(integral, 1, MPI_REAL, dest, tag, MPI_COMM_WORLD, ierr)
  END IF
  
  IF (my_rank .EQ. 0) THEN
     PRINT*, "With n = ", n, " trapezoids, our estimate"
     PRINT*, "of the integral from ", a, " to ", b, " = ", total
  END IF

  CALL MPI_FINALIZE(ierr) 

CONTAINS
  REAL FUNCTION f(x)
    REAL :: x

    f = x*x
  END FUNCTION f


  REAL FUNCTION Trap(local_a, local_b, local_n, h)
    REAL ::   local_a, local_b, h, integral, x, i
    INTEGER :: local_n

    integral = (f(local_a) + f(local_b))/2.0 
    x = local_a 
    DO i = 1, local_n-1
       x = x + h 
       integral = integral + f(x) 
    END DO
    Trap = integral*h 
  END FUNCTION Trap



END PROGRAM trapezoidal
