# MPI/OpenMP Hello World

Simple MPI/OpenMP "Hello, world!" program using `MPI_Init_threads`.

### Compilation

```bash
mkdir build & cd build
cmake ..
make
```

### Running

```bash
OMP_NUM_THREADS=4 srun -N 2 -n 8 ./hello-mpi
```
