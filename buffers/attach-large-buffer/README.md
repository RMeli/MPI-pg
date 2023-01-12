# Attach Large Buffer to MPI

`MPI_Bsend` performs a basic send operation with user-provided buffering. The buffer needs to be explicitly attached with `MPI_Buffer_attach` and `MPI_buffer_detach`.

## Issue Reproduced

This code snippet shows that the size of the buffer is limited by the fact that `MPI_Buffer_attach` takes a 32-bit integer as input, and that this input is the size in bytes (not the number of elements).

### MPI Error

```text
++++++++++++++++++++++++++++++++++++++++++++++++++
Number of elements: 2000000000
Size of real in bytes: 4
Size in bytes8000000000
Size in bytes as int(4) [OVERFLOW]-589934592
++++++++++++++++++++++++++++++++++++++++++++++++++
MPICH ERROR [Rank 1] [job id 40623.0] [Thu Jan 12 13:25:28 2023] [nid003049] - Abort(336143116) (rank 1 in comm 0): Fatal error in PMPI_Buffer_attach: Invalid argument, error stack:
PMPI_Buffer_attach(114): MPI_Buffer_attach(buf=0x150c42495020, size=-589934592) failed
PMPI_Buffer_attach(89).: Invalid value for size, must be non-negative but is -589934592

aborting job:
Fatal error in PMPI_Buffer_attach: Invalid argument, error stack:
PMPI_Buffer_attach(114): MPI_Buffer_attach(buf=0x150c42495020, size=-589934592) failed
PMPI_Buffer_attach(89).: Invalid value for size, must be non-negative but is -589934592
MPICH ERROR [Rank 0] [job id 40623.0] [Thu Jan 12 13:25:28 2023] [nid003048] - Abort(805905164) (rank 0 in comm 0): Fatal error in PMPI_Buffer_attach: Invalid argument, error stack:
PMPI_Buffer_attach(114): MPI_Buffer_attach(buf=0x14c691295020, size=-589934592) failed
PMPI_Buffer_attach(89).: Invalid value for size, must be non-negative but is -589934592

aborting job:
Fatal error in PMPI_Buffer_attach: Invalid argument, error stack:
PMPI_Buffer_attach(114): MPI_Buffer_attach(buf=0x14c691295020, size=-589934592) failed
PMPI_Buffer_attach(89).: Invalid value for size, must be non-negative but is -589934592
srun: error: nid003048: task 0: Exited with exit code 255
srun: launch/slurm: _step_signal: Terminating StepId=40623.0
srun: error: nid003049: task 1: Exited with exit code 255
```

## `balfrin`

### Programming Environment

```bash
spack build-env icon @cpu -- bash
```

### Compilation

```bash
mkdir build & cd build

cmake ..
make
```

### Running

```bash
srun -N 2 ./attach_large_buffer
```
