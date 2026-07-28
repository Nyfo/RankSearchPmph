# Batch Rank-Search-k in Futhark

University project implementing a flattened, non-recursive batch rank-search-k
algorithm in Futhark. The implementation runs on GPUs and is benchmarked against
a CUB segmented sorting baseline and a sequential NumPy implementation.

## Report

- [Read the submitted project report](report/Batch_Rank_Search_K_Project.pdf)

## Benchmarks on the Hendrix cluster

Clone or copy the repository to the cluster and enter the project directory.

### 1. Load required modules

```bash
module load cuda
module load futhark
module load numpy
module load python/3.11.3
```

### 2. Install the `futhark-data` library with pip

```bash
pip install futhark-data
```

### 3. Generate the Futhark benchmark datasets

```bash
python gen_futhark_data.py
```

### 4. Compile and run the CUB and Futhark benchmarks

```bash
make
```

### 5. Run the Python benchmark

```bash
python sort.py
```