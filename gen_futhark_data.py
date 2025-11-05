import os
import numpy as np
import futhark_data as fd

# Configuration
SEED = 1337
A_min, A_max = 0, 2**24
OUT_DIR = '128_datasets'
MS_PAIRS = [
    (128,         1_000_000),
    (1_280,         100_000),
    (12_800,         10_000),
    (128_000,          1_000),
    (1_280_000,           100),
    (12_800_000,            10),
]

# Ensure output directory exists
os.makedirs(OUT_DIR, exist_ok=True)
rng = np.random.default_rng(SEED)

def write_pair(m, s):
    n = m * s

    # Data values
    A = rng.uniform(A_min, A_max, size=n).astype(np.float32)

    # Segment sizes and metadata (k in [1,s] and segment owner per element)
    shp = np.full(m, s, dtype=np.int32)
    ks  = rng.integers(1, s+1, size=m, dtype=np.int32)
    II1 = np.repeat(np.arange(m, dtype=np.int32), s)

    # reference rank search solution (using np.sort() here)
    k_elements = np.empty(m, dtype=np.float32)
    for i in range(m):
        seg = A[i*s:(i+1)*s]
        k0 = int(ks[i]) - 1
        # k_elements[i] = np.partition(seg, k0)[k0]
        seg_sorted = np.sort(seg)
        k_elements[i] = seg_sorted[k0]

    # File names with sizes
    base = os.path.join(OUT_DIR, f'm{m}_s{s}')
    in_path  = base + '.in'
    out_path = base + '.out'

    # Write binary .in (ks, shp, II1, A) and .out (k_elements solution)
    with open(in_path, 'wb') as f:
        fd.dump(ks,  f, binary=True)
        fd.dump(shp, f, binary=True)
        fd.dump(II1, f, binary=True)
        fd.dump(A,   f, binary=True)

    with open(out_path, 'wb') as f:
        fd.dump(k_elements, f, binary=True)

    print(f"Wrote {in_path} and {out_path}  (m={m}, s={s}, n={n})")

# Loop over data m and s pairs
for m, s in MS_PAIRS:
    write_pair(m, s)
