# `MPI_RGet` Demo

## `eiger`

### Programming Environment

```bash
module load PrgEnv-gnu
``` 

### Compilation

```bash
mkdir build & cd build

cmake ..
make
```

### Running

```bash
srun -N 4 -C mc -p normal rget_demo
```

## Credits

The code in this folder is based on code provided by Marco Bettiol.
