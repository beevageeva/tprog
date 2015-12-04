program hello_world
  use mpi
  implicit none

  integer error
  integer, parameter :: master = 0, tag=0
  integer num_procs, world_id, i, rankReceived, ierr, status(MPI_STATUS_SIZE)

  call MPI_Init ( error )
  call MPI_Comm_size ( MPI_COMM_WORLD, num_procs, error )
  call MPI_Comm_rank ( MPI_COMM_WORLD, world_id, error )

  if ( world_id == master ) then
     do i  = 1, num_procs-1
      CALL MPI_RECV(rankReceived, 1, MPI_INTEGER, i, tag, MPI_COMM_WORLD, status, ierr)
      print*, "greetings from ", rankReceived
     end do
  ELSE
     CALL MPI_SEND(world_id, 1, MPI_INTEGER, master, tag, MPI_COMM_WORLD, ierr)
  END IF


  call MPI_Finalize ( error )

end program hello_world
