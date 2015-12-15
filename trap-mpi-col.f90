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
  INTEGER :: n
  REAL :: a, b
  INTEGER :: myRank, p, local_n,  status(MPI_STATUS_SIZE), ierr
  REAL :: h, local_a, local_b, integral, total
  
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myRank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, p, ierr)

  
  IF (myRank .EQ. 0) THEN
		print *,"Introduce n"
		read* , n
		print *,"Introduce a"
		read* , a
		print *,"Introduce b"
		read* , b
  end if

  call mpi_bcast(a, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call mpi_bcast(b, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  call mpi_bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

  h = (b-a)/n
  local_n = n/p
  
  local_a = a + myRank*local_n*h
  local_b = local_a + local_n*h


  integral = Trap(local_a, local_b, local_n, h)
  
  !print* , "My Rank =  ", myRank, ", integral = ", integral

  call mpi_reduce(integral, total, 1, MPI_REAL, MPI_SUM, 0,  MPI_COMM_WORLD, ierr)    
  
  IF (myRank .EQ. 0) THEN
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
