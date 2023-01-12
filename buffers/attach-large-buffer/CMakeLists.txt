cmake_minimum_required(VERSION 3.17)
project(mpi_attach_large_buffer LANGUAGES Fortran)

find_package(MPI REQUIRED)

add_executable(attach_large_buffer attach_large_buffer.f90)
target_link_libraries(attach_large_buffer MPI::MPI_Fortran)
