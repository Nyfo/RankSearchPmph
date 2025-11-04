# Benchmarks on the Hendrix cluster
## 1. Load required modules
```
module load cuda
module load futhark
module load numpy
module load python/3.11.3
```
## 2. Compile and run CUBs and Futhark benchmarks
To compile both our CUDA and Futhark implementations
```
make compile_all
```
## 3. Run the CUBs benchmark
```
make run_cub
```
## 4. Run the Futhark tests and benchmarks
```
make run_futhark
```
## 5. Run the Python benchmark
```
python sort.py
```
