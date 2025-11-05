import numpy as np
import time

# Generate random input data
def randomInitNat(num_items: int, H: int = 2**24, seed: int = 12345) -> np.ndarray:
    rng = np.random.default_rng(seed)
    return rng.integers(0, H, size=num_items, dtype=np.uint32)

# Build even offsets for segmenting data
def buildEvenOffsets(N: int, M: int) -> np.ndarray:
    base, rem = divmod(N, M)
    lens = np.full(M, base, dtype=np.int64)
    lens[:rem] += 1
    offsets = np.empty(M + 1, dtype=np.int64)
    offsets[0] = 0
    np.cumsum(lens, out=offsets[1:])
    return offsets

# Segmented sort using NumPy
def segmented_sort_numpy(keys_in: np.ndarray, offsets: np.ndarray) -> np.ndarray:
    keys_out = np.empty_like(keys_in)
    # Loop over segments 
    for s, e in zip(offsets[:-1], offsets[1:]): 
        # np.sort returns a sorted copy for the slice; assign into output
        keys_out[s:e] = np.sort(keys_in[s:e])
    return keys_out

def sort_segmented_numpy(num_items: int, num_segments: int, runs: int = 5, seed: int = 1337) -> None:
    # Generate data
    H = 2 ** 24
    data = randomInitNat(num_items, H=H, seed=seed)
    # Build even offsets
    offsets = buildEvenOffsets(num_items, num_segments)

    # Warm-up
    _ = segmented_sort_numpy(data, offsets)

    # Run time 
    t0 = time.perf_counter()
    for _ in range(runs):
        _ = segmented_sort_numpy(data, offsets)
    t1 = time.perf_counter()

    avg_us = (t1 - t0) * 1e6 / runs

    # Throughput in GB/s
    total_bytes = data.nbytes
    throughput_gbps = total_bytes / (avg_us / 1e6) / 1e9

    print(f" NumPy Segmented Sort - Items: {num_items:10d} Segments: {num_segments:6d} Time: {avg_us:10.3f} µs Avg")
    print(f" NumPy Segmented Sort - Throughput: {throughput_gbps:10.3f} GB/s")

    # Validate result
    sorted = np.all(np.diff(data) >= 0)
    if sorted:
        print(" NumPy Segmented Sort - Result = PASS")
    else:
        print(" NumPy Segmented Sort - Result = FAIL")


sort_segmented_numpy(128000000, 128)
sort_segmented_numpy(128000000, 1280)
sort_segmented_numpy(128000000, 12800)
sort_segmented_numpy(128000000, 128000)
sort_segmented_numpy(128000000, 1280000)
sort_segmented_numpy(128000000, 12800000)
sort_segmented_numpy(128000000, 128000000)