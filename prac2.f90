PROGRAM prac2
  USE MPI
  INTEGER :: master=0
  INTEGER :: n,localStart,localEnd, k, maxValue
  INTEGER, dimension(:), allocatable :: arr
  INTEGER :: myRank, numProcs, status(MPI_STATUS_SIZE), ierr


  
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myRank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, numProcs, ierr)
  if (myRank .eq. master) then
    print*, "Introduce n:"
    read*, n
  end if
  ! send n from process master(0) to all processes
  call mpi_bcast(n, 1, mpi_integer, master, mpi_comm_world, ierr)
	! the problem is equivalent to:
	!for a given n find all prime numbers x
	! n > x > sqrt(n)   
	!and put them in a list(and print the max afterwards)
  !(https://github.com/beevageeva/tprog/blob/master/prac2p.f90)
  ! but here we create an initial list with consecutive numbers(from 2 to n) and eliminate
  ! multiples of k for k = 2, sqrt(n)  
  ! each process creates its own initial array 
  k = int((n-1)/numProcs)
  localStart = 2 + myRank * k
  ! n - 1 might not ne a multiple of numProcs and the last process would have more
  ! elements than the rest
  if (myRank .eq. numProcs - 1) then
    localEnd = n
  else
    localEnd = 2 + myRank * k + k - 1
  end if
  allocate(arr(localEnd - localStart + 1))
  do k = localStart, localEnd
    arr(k - localStart + 1) = k
  end do

  do k = 2, int(sqrt(real(n)))
    ! eliminate multiples of k
    ! TODO memory(new array is smaller than initially allocated) valgrind result: no memory leak
    arr = pack(arr,mod(arr,k)/=0)
  end do

  !print*, "Process ", myRank, ": ", arr

  !PRINT THE MAX
  !we cannot send last element in the array from the last process only
  !because last process might have an empty array(in fact we can't know which is
  !the first process from the last one in descending order which have a non
  !empty array), we could send message from last to first process: numProc - 1 sends max or 0
  !to proc numProc -2 and this one sends to numProc - 3 until process master(0)
  !but mpi_reduce is faster
   if(size(arr) == 0) then
      k = 0
   else
      k = arr(size(arr))
   end if  
  deallocate(arr)
   call mpi_reduce(k, maxValue, 1, MPI_INTEGER, MPI_MAX, master,  MPI_COMM_WORLD, ierr)   
  if(myRank == master) then
    print*, "max = ", maxValue
  end if
  

END PROGRAM prac2
