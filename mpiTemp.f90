PROGRAM PRACTICA
  USE MPI
  IMPLICIT NONE
  INTEGER :: lado, i, j, t=0, myRank, p, numCols, status(MPI_STATUS_SIZE), ierr
  REAL :: min,max,diff
  REAL, DIMENSION( : , : ), ALLOCATABLE :: datu, localDatu
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myRank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, p, ierr)

  if (myRank .EQ. 0) then
	  READ*, lado
	  ALLOCATE(datu(0:lado+1,lado))
    datu = 0.0
	  DO i=1,lado
	     READ*, datu(i,1:lado)
	  END DO
    
  end if

  
  call mpi_bcast(lado, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  !numCols = ceiling(real(lado) / p)
  numCols = lado/ p
  allocate(localDatu(0:lado+1, 0:numCols+1))
	localDatu = 0.0

  call mpi_scatter(datu, numCols * (lado+2) , MPI_REAL, localDatu(:,1), numCols * (lado+2), MPI_REAL, 0 ,MPI_COMM_WORLD, ierr) 

  call mpi_allreduce(MINVAL(localDatu(1:lado, 1:numCols)), min, 1, MPI_REAL, MPI_MIN,MPI_COMM_WORLD, ierr)
  call mpi_allreduce(MAXVAL(localDatu(1:lado, 1:numCols)), max, 1, MPI_REAL, MPI_MAX,MPI_COMM_WORLD, ierr)
	
!  DO i=0,lado+1
!	     print*, localDatu(i,:)
!	  END DO
 
  diff=max-min
  if(myRank == 0) THEN
    PRINT*, "t = ",t, "diff = ",diff
  end if
  DO WHILE (diff .GE. 1 )
     t=t+1
     ! send boundary values
      if(myRank .NE. 0) then
        ! send back
        call mpi_send(localDatu(:,1), lado+2, MPI_REAL, myRank - 1, 0, MPI_COMM_WORLD, ierr )
      end if    
      if(myRank .NE. p-1) then
        ! send fw
        call mpi_send(localDatu(:,numCols), lado+2, MPI_REAL, myRank + 1, 0, MPI_COMM_WORLD, ierr )
      end if    

      if(myRank .NE. 0) then
        ! recv from back
        call mpi_recv(localDatu(:,0), lado+2, MPI_REAL, myRank - 1, 0, MPI_COMM_WORLD, status, ierr )
      end if    
      if(myRank .NE. p-1) then
        ! recv from fw
        call mpi_recv(localDatu(:,numCols+1), lado+2, MPI_REAL, myRank + 1, 0, MPI_COMM_WORLD, status, ierr )
      end if   
     localDatu(1:lado,1:numCols) = 0.99*localDatu(1:lado,1:numCols) + 0.01*((localDatu(0:lado-1,1:numCols) + & 
 localDatu(2:lado+1,1:numCols) + localDatu(1:lado,0:numCols-1) + localDatu(1:lado,2:numCols+1))/ 4)
    call mpi_allreduce(MINVAL(localDatu(1:lado, 1:numCols)), min, 1, MPI_REAL, MPI_MIN,MPI_COMM_WORLD, ierr)
    call mpi_allreduce(MAXVAL(localDatu(1:lado, 1:numCols)), max, 1, MPI_REAL, MPI_MAX,MPI_COMM_WORLD, ierr)
    diff=max-min
    IF (MOD(t,1000) .EQ. 0 .AND. myRank == 0) PRINT*, "t = ",t, "diff = ",diff
  END DO
  if(myRank == 0) THEN
    PRINT*, "Final t is: ", t
  end if  
  CALL MPI_FINALIZE(ierr) 
END PROGRAM PRACTICA

