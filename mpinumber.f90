program hello_world
  use mpi
  implicit none

  integer error
  integer, parameter :: master = 0, tag=0
  integer num_procs, world_id, dest, ierr,  status(MPI_STATUS_SIZE)
  real numberMsg

  call MPI_Init ( error )
  call MPI_Comm_size ( MPI_COMM_WORLD, num_procs, error )
  call MPI_Comm_rank ( MPI_COMM_WORLD, world_id, error )

  if ( world_id == master ) then
    read*, numberMsg
  else
    call mpi_recv(numberMsg, 1, MPI_REAL, world_id - 1 , tag, MPI_COMM_WORLD, status, ierr)
  end if

  if ( world_id == num_procs -1 ) then
    dest = master
  else
    dest = world_id + 1
  end if
  call mpi_send(numberMsg * (world_id + 1), 1, mpi_real, dest, tag, mpi_comm_world, ierr)


  if ( world_id == master ) then
    call mpi_recv(numberMsg, 1, MPI_REAL, num_procs - 1  , tag, MPI_COMM_WORLD, status, ierr)
    print*, "RECEIVED NUMBER " , numberMsg
  end if



  call MPI_Finalize ( error )

end program hello_world
