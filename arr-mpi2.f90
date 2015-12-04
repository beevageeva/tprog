PROGRAM arraySum
  USE MPI
  INTEGER :: master=0, tag=0
  INTEGER :: n,k,i
  REAL, dimension(:), allocatable :: arr
  REAL, dimension(:), allocatable ::  localarr
  INTEGER :: myRank, numProcs, status(MPI_STATUS_SIZE), ierr
  REAL :: total, valueRecv
  
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myRank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, numProcs, ierr)

  
  IF (myRank .EQ. 0) THEN
		print *,"Introduce n"
		read* , n
    n = (n / numProcs) * numProcs
    k = n / numProcs
		allocate(arr(n), STAT=ierr)
		if (ierr/=0) print*, "allocation failed"
		print *,"Introduce array de ", n, " elementos"
		read*, arr
    call mpi_bcast(k, 1, mpi_integer, master, mpi_comm_world, ierr)
    !we must allocate bot arr and localarr in the sender as well ?? 
		 allocate(localarr(k), STAT=ierr)
    call mpi_scatter(arr, k, mpi_real, localarr, k, mpi_real, master,  mpi_comm_world, ierr)
    !do i = 1, numProcs - 1
    !  call mpi_send(arr(i*k+1), k, mpi_real,i, tag, mpi_comm_world, ierr)
    !end do
    total = sum(arr(1:k) )
    print*, "my total", total
    do i = 1, numProcs - 1
       !order does not mater use any source 
      call mpi_recv(valueRecv, 1, mpi_real, MPI_ANY_SOURCE, tag, mpi_comm_world, status, ierr)
       print*, "value received " , valueRecv 
      total = total + valueRecv
    end do
    PRINT*, "Total = ",  total
  ELSE
      !mpi_bcast both on sender and receiver
     call mpi_bcast(k, 1, mpi_integer, master, mpi_comm_world, ierr)
		 allocate(localarr(k), STAT=ierr)
		 !allocate(arr(k), STAT=ierr)
     call mpi_scatter(arr, k, mpi_real, localarr, k, mpi_real, master,  mpi_comm_world, ierr)
     !call mpi_recv(arr, k, mpi_real,master, tag, mpi_comm_world, status, ierr)
     CALL MPI_SEND(sum(arr), 1, MPI_REAL, master, tag, MPI_COMM_WORLD, ierr)
  END IF
  

  CALL MPI_FINALIZE(ierr) 


END PROGRAM arraySum
