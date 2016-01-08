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
  call mpi_bcast(n, 1, mpi_integer, master, mpi_comm_world, ierr)
  k = int((n-1)/numProcs)
  localStart = 2 + myRank * k
  ! n might not ne a multiple of numProcs so the last process will have more
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
    ! TODO is there a memory leak here ? valgrind result: no memory leak
    arr = pack(arr,mod(arr,k)/=0)
  end do

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
   call mpi_reduce(k, maxValue, 1, MPI_INTEGER, MPI_MAX, master,  MPI_COMM_WORLD, ierr)   
  deallocate(arr)
  if(myRank == master) then
    print*, "max = ", maxValue
  end if
  

END PROGRAM prac2
