# Benchmarks on the Hendrix cluster
## 1. Load required modules
```
module load cuda
module load futhark
module load numpy
module load python/3.11.3
```
## 2. Build and run CUDA and Futhark benchmarks
```
make
```

## 3. Run the Python benchmark
```
python sort.py
```
