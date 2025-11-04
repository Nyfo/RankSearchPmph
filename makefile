compile_all: test-cub compile-rankSearch

run: run_futhark run_cub

run_futhark: futhark_test futhark_bench

run_cub: test-cub
	./test-cub

test-cub: cub_radix_sort.cu helper.cu.h
	nvcc -O3 -std=c++17 cub_radix_sort.cu -o test-cub

compile-rankSearch:
	futhark c rankKSearch.fut

futhark_test: rankKSearch.fut
	futhark test --backend=cuda rankKSearch.fut

futhark_bench: rankKSearch.fut
	futhark bench --backend=cuda rankKSearch.fut

clean:
	rm -f test-cub