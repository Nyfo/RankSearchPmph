#include "cub/cub.cuh"
#include "helper.cu.h"

// NEW: pick 1-based k per segment
__global__ void pick_k(const uint32_t* sorted, const int* off,
                       const int* ks, uint32_t* out, int m){
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < m) out[i] = sorted[ off[i] + (ks[i]-1) ];
}

double sortSegmentedRadixCUB( uint32_t* data_keys_in,
                              uint32_t* data_keys_out,
                              const uint64_t N,
                              const int M,                // NEW
                              const int* d_offsets)       // NEW
{
    const int beg_bit = 0, end_bit = 32;
    void * tmp_sort_mem = nullptr;
    size_t tmp_sort_len = 0;

    // size query (CHANGED: segmented)
    cub::DeviceSegmentedRadixSort::SortKeys(
        tmp_sort_mem, tmp_sort_len,
        data_keys_in, data_keys_out,
        N, M, d_offsets, d_offsets + 1, beg_bit, end_bit);
    cudaMalloc(&tmp_sort_mem, tmp_sort_len);
    cudaCheckError();

    // dry run (CHANGED: segmented)
    cub::DeviceSegmentedRadixSort::SortKeys(
        tmp_sort_mem, tmp_sort_len,
        data_keys_in, data_keys_out,
        N, M, d_offsets, d_offsets + 1, beg_bit, end_bit);
    cudaDeviceSynchronize();
    cudaCheckError();

    // timing (kept teacher style)
    double elapsed;
    struct timeval t_start, t_end, t_diff;
    gettimeofday(&t_start, NULL);

    for(int k=0; k<GPU_RUNS; k++) {
        cub::DeviceSegmentedRadixSort::SortKeys(
            tmp_sort_mem, tmp_sort_len,
            data_keys_in, data_keys_out,
            N, M, d_offsets, d_offsets + 1, beg_bit, end_bit);
    }
    cudaDeviceSynchronize();
    cudaCheckError();

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_start);
    elapsed = (t_diff.tv_sec*1e6 + t_diff.tv_usec) / ((double)GPU_RUNS);

    cudaFree(tmp_sort_mem);
    return elapsed; // µs average
}

int main (int argc, char * argv[]) {
    // CHANGED: expect <num_segments> <segment_size>
    if (argc != 3) {
        printf("Usage: %s <M num_segments> <S segment_size>\n", argv[0]);
        return 1;
    }
    const int M = atoi(argv[1]);
    const int S = atoi(argv[2]);
    const uint64_t N = (uint64_t)M * (uint64_t)S;

    // host data
    uint32_t* h_keys     = (uint32_t*) malloc(N*sizeof(uint32_t));
    uint32_t* h_keys_res = (uint32_t*) malloc(N*sizeof(uint32_t));
    randomInitNat(h_keys, N, (uint32_t)(N/10));

    // CHANGED: segment offsets [0,S,2S,...,M*S]
    int* h_offsets = (int*) malloc((M+1)*sizeof(int));
    for (int i=0;i<=M;++i) h_offsets[i] = i*S;

    // NEW: k per segment (e.g., median = S/2, 1-based)
    int* h_ks = (int*) malloc(M*sizeof(int));
    for (int i=0;i<M;++i) h_ks[i] = (S+1)/2;  // 1-based

    // device data
    uint32_t *d_keys_in, *d_keys_out;
    cudaSucceeded(cudaMalloc((void**) &d_keys_in,  N * sizeof(uint32_t)));
    cudaSucceeded(cudaMemcpy(d_keys_in, h_keys, N * sizeof(uint32_t), cudaMemcpyHostToDevice));
    cudaSucceeded(cudaMalloc((void**) &d_keys_out, N * sizeof(uint32_t)));

    int *d_offsets, *d_ks;
    cudaSucceeded(cudaMalloc((void**)&d_offsets, (M+1)*sizeof(int)));
    cudaSucceeded(cudaMemcpy(d_offsets, h_offsets, (M+1)*sizeof(int), cudaMemcpyHostToDevice));
    cudaSucceeded(cudaMalloc((void**)&d_ks, M*sizeof(int)));
    cudaSucceeded(cudaMemcpy(d_ks, h_ks, M*sizeof(int), cudaMemcpyHostToDevice));

    // run segmented radix baseline (CHANGED call)
    double elapsed = sortSegmentedRadixCUB(d_keys_in, d_keys_out, N, M, d_offsets);

    // optional: copy back full sorted buffer for a quick per-segment sanity check
    cudaMemcpy(h_keys_res, d_keys_out, N*sizeof(uint32_t), cudaMemcpyDeviceToHost);
    cudaDeviceSynchronize(); cudaCheckError();

    // NEW: compute k-th per segment
    uint32_t* d_ans; cudaSucceeded(cudaMalloc((void**)&d_ans, M*sizeof(uint32_t)));
    pick_k<<<(M+255)/256, 256>>>(d_keys_out, d_offsets, d_ks, d_ans, M);
    cudaDeviceSynchronize(); cudaCheckError();

    // print timing (kept teacher style)
    printf("CUB SegmentedRadixSort  M=%d  S=%d  N=%llu  avg=%.2f us\n",
           M, S, (unsigned long long)N, elapsed);

    // cleanup
    cudaFree(d_ans);
    cudaFree(d_ks); cudaFree(d_offsets);
    cudaFree(d_keys_in); cudaFree(d_keys_out);
    free(h_ks); free(h_offsets);
    free(h_keys); free(h_keys_res);
    return 0;
}
