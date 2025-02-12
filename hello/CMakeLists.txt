cmake_minimum_required(VERSION 3.17)
project(hellp-mpi LANGUAGES CXX)

find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)

add_executable(hello-mpi hello-mpi.cpp)
target_link_libraries(hello-mpi MPI::MPI_CXX OpenMP::OpenMP_CXX)
