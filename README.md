# Benchmarks on the Hendrix cluster
## 1. Load required modules
```
module load cuda
module load futhark
module load numpy
module load python/3.11.3
```
## 2. Install futhark-data library with pip
```
pip install futhark-data
```
## 3. Generate the futhark benchmark datasets
```
python gen_futhark-data.py
```
## 4. Compile and run CUBs and Futhark benchmarks
Extract our code .zip and move the extracted folder to the cluster.
Locate the folder on the cluster with the terminal.
To compile and run both our CUDA and Futhark implementations:
```
make
```
## 5. Run the Python benchmark
```
python sort.py
```
