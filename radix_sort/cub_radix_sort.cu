#include "cub/cub.cuh"
#include "helper.cu.h"

// Generate array of random unsigned integers in [0, H)
void randomInitNat(uint32_t* data, const uint32_t size, const uint32_t H) {
    for (int i = 0; i < size; ++i) {
        unsigned long int r = rand();
        data[i] = r % H;
    }
}

// Evenly split N items into M segments and distributing the remainders in the first N % M segments.
void buildEvenOffsets(int* offsets, int N, int M) {
    int base = N / M;
    int rem  = N % M;
    int acc  = 0;
    offsets[0] = 0;
    for (int s = 0; s < M; ++s) {
        int len = base + (s < rem ? 1 : 0);
        acc += len;
        offsets[s + 1] = acc;
    }
}

int sortSegmentedCUB(int num_items, int num_segments)
{
    // Define H as the range of input values
    const uint32_t H = 2u << 24; // range of input values
    srand(1337);

    // Host allocation
    uint32_t* h_keys = (uint32_t*) malloc(num_items * sizeof(uint32_t));
    int* h_offsets = (int*) malloc((num_segments + 1) * sizeof(int));
    randomInitNat(h_keys, num_items, H);
    buildEvenOffsets(h_offsets, num_items, num_segments);

    // Device allocation
    uint32_t* d_keys_in  = nullptr;
    uint32_t* d_keys_out = nullptr;
    int*      d_offsets  = nullptr;
    cudaMalloc(&d_keys_in,  num_items * sizeof(uint32_t));
    cudaMalloc(&d_keys_out, num_items * sizeof(uint32_t));
    cudaMalloc(&d_offsets, (num_segments + 1) * sizeof(int)); 

    // Copy data from host to device
    cudaMemcpy(d_keys_in, h_keys, num_items * sizeof(uint32_t), cudaMemcpyHostToDevice); 
    cudaMemcpy(d_offsets, h_offsets , (num_segments + 1) * sizeof(int), cudaMemcpyHostToDevice); 

    // Computes a batched radix sort using CUB's DeviceSegmentedRadixSort
    void * d_temp_storage = nullptr;
    size_t temp_storage_bytes = 0;
    cub::DeviceSegmentedSort::SortKeys(
        d_temp_storage, temp_storage_bytes,
        d_keys_in, d_keys_out,
        num_items, num_segments, d_offsets, d_offsets + 1);

    // Allocate temporary storage
    cudaMalloc(&d_temp_storage, temp_storage_bytes);
    cudaCheckError();

    cub::DeviceSegmentedSort::SortKeys(
        d_temp_storage, temp_storage_bytes,
        d_keys_in, d_keys_out,
        num_items, num_segments, d_offsets, d_offsets + 1);
    cudaDeviceSynchronize();
    cudaCheckError();

    // Actual sort and captures the average run time
    double elapsed;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL);

    for(int k=0; k< GPU_RUNS; k++) {
        cub::DeviceSegmentedSort::SortKeys(
            d_temp_storage, temp_storage_bytes,
            d_keys_in, d_keys_out,
            num_items, num_segments, d_offsets, d_offsets + 1);
    }
    cudaDeviceSynchronize();
    cudaCheckError();

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6 + t_diff.tv_usec) / ((double)GPU_RUNS);

    // Run time
    printf(" CUB Segmented Radix Sort - Items: %10d Segments: %6d Time: %10.3f µs Avg\n",
          num_items, num_segments, elapsed);

    // Throughput
    double throughput = (num_items * sizeof(uint32_t)) / (elapsed * 1e3);
    printf(" CUB Segmented Radix Sort - Throughput: %10.3f GB/s\n", throughput);


    // //Validate result
    // cudaMemcpy(h_keys, d_keys_out, num_items * sizeof(uint32_t), cudaMemcpyDeviceToHost); 
    // bool sorted = true;
    // for (int s = 0; s < num_segments; ++s) {
    //     int start = h_offsets[s];
    //     int end   = h_offsets[s + 1];
    //     for (int i = start + 1; i < end; ++i) {
    //         if (h_keys[i - 1] > h_keys[i]) {
    //             sorted = false;
    //             break;;
    //         }
    //     }
    // }
    // cleanup

    free(h_keys); 
    free(h_offsets);
    cudaFree(d_temp_storage);
    cudaFree(d_keys_in);
    cudaFree(d_keys_out);
    cudaFree(d_offsets);
    return 0;
}

int main() {
    sortSegmentedCUB(32000000, 32);
    sortSegmentedCUB(32000000, 320);
    sortSegmentedCUB(32000000, 3200);
    sortSegmentedCUB(32000000, 32000);
    sortSegmentedCUB(32000000, 320000);
    sortSegmentedCUB(32000000, 3200000);
    sortSegmentedCUB(32000000, 32000000);
    cudaDeviceSynchronize(); 
    cudaCheckError();
    return 0;
}
