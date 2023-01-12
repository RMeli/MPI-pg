program attach_large_buffer
   use mpi

   integer :: rank

   integer :: size, real_bytes
   integer(kind=8) :: size_in_bytes

   integer :: ierr

   real, allocatable, dimension(:) :: a

   call mpi_init(ierr)

   call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)

   size = 2000000000 ! largest 32-bits integer: 2'147'483'647
   call mpi_sizeof(real, real_bytes, ierr)

   if (rank == 0) then
      print *, "Number of elements: ", size
      print *, "Size of real in bytes: ", real_bytes
   end if
   size_in_bytes = int(size, kind=8)*int(real_bytes, kind=8)

   if (rank == 0) then
      print *, "Size in bytes", size_in_bytes
      print *, "Size in bytes as int(4) [OVERFLOW]", int(size_in_bytes, kind=4)
   end if

   allocate (a(size))

   ! This fails -- integer overflow
   call mpi_buffer_attach(a, size_in_bytes)

   call mpi_buffer_detach(a, size_in_bytes)

   call mpi_finalize(ierr)

   deallocate (a)

end program attach_large_buffer
