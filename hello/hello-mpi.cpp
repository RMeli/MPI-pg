#include <mpi.h>
#include <omp.h>

#include <cstddef>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
  int mpi_threading_required = MPI_THREAD_MULTIPLE;
  int mpi_threading_provided;
  MPI_Init_thread(NULL, NULL, mpi_threading_required, &mpi_threading_provided);

  if (mpi_threading_provided != mpi_threading_required) {
    MPI_Abort(MPI_COMM_WORLD, -72);
  }

  int comm_world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &comm_world_size);

  int comm_world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &comm_world_rank);

  char hostname[MPI_MAX_PROCESSOR_NAME];
  int hostname_len;
  MPI_Get_processor_name(hostname, &hostname_len);

  int omp_num_threads = omp_get_max_threads();

  std::stringstream ss;
  ss << "Hello from rank " << comm_world_rank << " of " << comm_world_size
     << " running with " << omp_num_threads << " threads on " << hostname
     << '\n';

  if (comm_world_rank == 0) {
    std::vector<std::string> st(omp_num_threads);
#pragma omp parallel
    {
      int omp_thread_id = omp_get_thread_num();
      std::stringstream sst;
      sst << '\t' << "Hello from thread " << omp_thread_id << " on rank "
          << comm_world_rank << '\n';
      st[omp_thread_id] = sst.str();
    }

    for (const auto &s : st) {
      ss << s;
    }
  }

  std::cout << ss.str();

  MPI_Finalize();
}
