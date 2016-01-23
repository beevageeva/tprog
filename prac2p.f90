PROGRAM prac2
  USE MPI
  INTEGER :: master=0
  INTEGER :: n,localStart,localEnd, k, maxValue, start
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
  ! each process creates its own initial array
  start = ceiling(sqrt(real(n))) 
  k = int((n-start+1)/numProcs)
  localStart = start + myRank * k
  ! n - start + 1 might not be a multiple of numProcs and the last process would have more
  ! elements than the rest
  if (myRank .eq. numProcs - 1) then
    localEnd = n
  else
    localEnd = start + myRank * k + k - 1
  end if
  allocate(arr(localEnd - localStart + 1))
  !reuse n
  n = 1
  do k = localStart, localEnd
    if(isPrime(k)) then
      arr(n) = k
      n=n+1
    end if
  end do

  !print*, "Process ", myRank, ": ", arr

  !we cannot send last element in the array from the last process only
  !because last process might have an empty array(in fact we can't know which is
  !the first process from the last one in descending order which have a non
  !empty array), we could send message from last to first process: numProc - 1 sends max or 0
  !to proc numProc -2 and this one sends to numProc - 3 until process master(0)
  !but mpi_reduce is faster
   if(n > 1) then
      k = arr(n-1) 
   else
      k = 0
   end if 
   deallocate(arr) 
   call mpi_reduce(k, maxValue, 1, MPI_INTEGER, MPI_MAX, master,  MPI_COMM_WORLD, ierr)   
  if(myRank == master) then
    print*, "max = ", maxValue
  end if

  CALL MPI_Finalize(ierr) 


  
CONTAINS
  FUNCTION isPrime(x) result(res)
    logical :: res
    integer, intent (in) :: x
    integer i
    do i=2,int(sqrt(real(x)))
      if(mod(x,i)==0) then
        res = .FALSE.
        return
      end if
    end do 
    res = .TRUE.
  END FUNCTION isPrime

END PROGRAM prac2
