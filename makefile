compile_all: test-cub compile-rankSearch compile-partition3

run: run_futhark run_cub partition3_test

run_futhark: futhark_test futhark_bench

run_cub: test-cub
	./test-cub

partition3_test: partition3.fut
	futhark test --backend=cuda partition3.fut

test-cub: cub_radix_sort.cu helper.cu.h
	nvcc -O3 -std=c++17 cub_radix_sort.cu -o test-cub

compile-rankSearch:
	futhark c rankSearch.fut
	futhark c rankSearch_gen.fut

compile-partition3:
	futhark c partition3.fut

futhark_test: rankSearch.fut
	futhark test --backend=cuda rankSearch.fut

futhark_bench: rankSearch.fut
	futhark bench --backend=cuda rankSearch.fut

clean:
	rm -f test-cub
	rm -f rankSearch rankSearch.c
	rm -f rankSearch_gen rankSearch_gen.c
	rm -f partition3 partition3.c