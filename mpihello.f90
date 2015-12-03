program hello_world
  use mpi
  implicit none

  integer error
  integer, parameter :: master = 0
  integer num_procs
  integer world_id

  call MPI_Init ( error )
  call MPI_Comm_size ( MPI_COMM_WORLD, num_procs, error )
  call MPI_Comm_rank ( MPI_COMM_WORLD, world_id, error )

  if ( world_id == master ) then
     print*, "I'm the master of ", num_procs, "processes"
  end if

  print*, "Process ", world_id, ' says "Hello, world!"'

  call MPI_Finalize ( error )

end program hello_world
