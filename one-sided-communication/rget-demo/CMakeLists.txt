cmake_minimum_required(VERSION 3.22)
project(mpi_rget_demo LANGUAGES Fortran)

find_package(MPI REQUIRED)

add_executable(rget_demo rget_demo.f90)
target_link_libraries(rget_demo MPI::MPI_Fortran)

