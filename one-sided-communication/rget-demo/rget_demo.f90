! MPI RGET DEMO
! Based on a demo originally written by Marco Bettiol @ CSCS
program rget_demo

    use mpi
    use iso_c_binding, only: c_ptr, c_null_ptr, c_f_pointer
    use iso_fortran_env, only: compiler_version, real64

    implicit none

    integer, parameter :: getter_rank = 1
    integer(kind=MPI_ADDRESS_KIND), parameter :: window_size = 3
    logical, parameter :: verbose = .TRUE.

    integer :: ierr, world_rank, world_size, idx

    integer :: window_object, size_of_mpi_double ! integer(4)
    integer(kind=MPI_ADDRESS_KIND) :: window_size_bytes, ZERO_BYTES ! integer(8)
    integer(kind=MPI_ADDRESS_KIND) :: disp_unit, target_offset, ONE

    real(kind=real64), allocatable, dimension(:) :: buffer
    integer :: buffer_offset
    integer, allocatable, dimension(:) :: requests
    integer :: request_idx

    ! c stuff and pointers
    type(c_ptr) :: c_window_ptr
    real(kind=real64), pointer, dimension(:) :: window_ptr, NULL_F_PTR

    nullify(NULL_F_PTR)
    ZERO_BYTES = 0 ! 0 of kind MPI_ADDRESS_KIND
    ONE = 1 ! 1 of kind MPI_ADDRESS_KINd

    call mpi_init(ierr)

    call mpi_comm_rank(MPI_COMM_WORLD, world_rank, ierr)
    call mpi_comm_size(MPI_COMM_WORLD, world_size, ierr)
    print *, ">>> Rank ", world_rank, " of ", world_size, " reporting for duty."

    ! get size in bytes of MPI_DOUBLE_PRECISION
    call mpi_type_size(MPI_DOUBLE_PRECISION, size_of_mpi_double, ierr)
    call check_mpi_error(ierr, world_rank, "mpi_type_size")

    ! define the displacements in bytes within the window
    ! this is the space occupied by one element
    disp_unit = 8 ! size of MPI_REAL and REAL64

    if(world_rank .eq. getter_rank) then
        print *, "*** Rank ", getter_rank, " is the getter rank."
        print *, "--- Compiler version: ", compiler_version()
        print *, "--- Size of MPI_ADDRESS_KIND: ", MPI_ADDRESS_KIND
        print *, "--- Size of MPI_DOUBLE_PRECISION: ", size_of_mpi_double

        ! getter_rank process does not contribute any local windows to the window object being created
        ! called with a nullified pointer and requesting zero bytes of memory
        ! no memory is allocated by the getter_rank process
        call mpi_win_create(NULL_F_PTR, ZERO_BYTES, ONE, MPI_INFO_NULL, MPI_COMM_WORLD, window_object, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_win_create")
    else ! non-getter processes
        ! compute window size in bytes
        window_size_bytes = window_size * size_of_mpi_double

        ! make sure pointers are null
        call reset_pointers(c_window_ptr, window_ptr)

        ! allocate memory and create window object
        call mpi_alloc_mem(window_size_bytes, MPI_INFO_NULL, c_window_ptr, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_alloc_mem")
        call c_f_pointer(c_window_ptr, window_ptr, [window_size])
        call mpi_win_create(window_ptr, window_size_bytes, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, window_object, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_win_create")

        ! lock window_object to perform meomory update
        call mpi_win_lock(MPI_LOCK_EXCLUSIVE, world_rank, 0, window_object, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_win_lock")

        ! initialise remote memory
        window_ptr(:) = world_rank

        ! unlock window_object to perform memory update
        call mpi_win_unlock(world_rank, window_object, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_win_unlock")
    end if

    if(verbose) then
        print *, "### Window at rank ", world_rank, ": ", window_ptr(:)
    end if

    call mpi_barrier(MPI_COMM_WORLD, ierr)
    call check_mpi_error(ierr, world_rank, "mpi_barrier")

    if(world_rank .eq. getter_rank) then
        ! allocate buffer in getter_rank process
        ! data will be stored here
        allocate(buffer(window_size * (world_size - 1)), stat=ierr)
        if(verbose .and. ierr .ne. 0) then
            print *, "Allocation of 'buffer' on ", world_rank, "FAILED!"
            stop
        end if

        ! allocate requests array in getter_rank
        allocate(requests(world_size - 1), stat=ierr)
        if(verbose .and. ierr .ne. 0) then
            print *, "Allocation of 'requests' on ", world_rank, "FAILED!"
            stop 
        end if

        buffer(:) = -1
        if(verbose) then
            buffer_offset = 1
            do idx = 0, world_size - 1
                if(idx .ne. getter_rank) then
                    print *, "@@@ Buffer ", idx, " at rank ", world_rank, ": ",&
                        buffer(buffer_offset:buffer_offset + window_size - 1)
                    buffer_offset = buffer_offset + window_size
                end if
            end do
        end if

        call mpi_win_lock_all(0, window_object, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_win_lock_all")

        request_idx = 1
        buffer_offset = 1 ! store results of get at buffer(buffer_offset)
        target_offset = 0 ! no target offset, we want to get all the window
        do idx = 0, world_size - 1 ! ranks go from 0 to WORLD_SIZE - 1
           if(idx .ne. getter_rank) then
               target_offset = 0
               call mpi_rget(&
                   buffer(buffer_offset), window_size, MPI_REAL8,&
                   idx, target_offset, window_size, MPI_REAL8,&
                   window_object, requests(request_idx), ierr&
               )
               !call mpi_get(&
               !    buffer(buffer_offset), window_size, MPI_REAL8,&
               !    idx, target_offset, window_size, MPI_REAL8,&
               !    window_object, ierr&
               !)
               call check_mpi_error(ierr, world_rank, "mpi_get")

               ! get the offset for the next batch of data
               buffer_offset = buffer_offset + window_size

               request_idx = request_idx + 1
           end if
        end do

        call mpi_waitall(world_size - 1, requests, MPI_STATUSES_IGNORE, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_waitall")

        call mpi_win_unlock_all(window_object, ierr)
        call check_mpi_error(ierr, world_rank, "mpi_win_unlock_all")

        if(verbose) then
            buffer_offset = 1 
            do idx = 0, world_size - 1
                if(idx .ne. getter_rank) then
                     print *, "%%% Received buffer from rank ", idx, ": ",&
                         buffer(buffer_offset:buffer_offset + window_size - 1)
                     buffer_offset = buffer_offset + window_size
                end if
            end do
        end if

        ! cleanup
        deallocate(buffer)
        deallocate(requests)
    end if

    call mpi_barrier(MPI_COMM_WORLD, ierr)
    call check_mpi_error(ierr, world_rank, "mpi_barrier")

    ! window cleanup
    call mpi_win_free(window_object, ierr)
    call check_mpi_error(ierr, world_rank, "mpi_win_free")
    call mpi_free_mem(window_ptr, ierr)
    call check_mpi_error(ierr, world_rank, "mpi_free_mem")

    call mpi_finalize(ierr)

    contains

    subroutine check_mpi_error(ierror, rank, mpi_function_name)
        integer, intent(in) :: ierror, rank
        character(len=*), intent(in), optional :: mpi_function_name
        if(ierror .ne. MPI_SUCCESS) then
            print *, "MPI Error for rank ", rank, " in function '", mpi_function_name,"': ", ierror
        end if
    end subroutine

    subroutine reset_pointers(cptr, fptr)
        type(c_ptr), intent(inout) :: cptr
        real(kind=real64), pointer, dimension(:), intent(inout) :: fptr(:)

        cptr = c_null_ptr
        nullify(fptr)
    end subroutine

end program rget_demo
