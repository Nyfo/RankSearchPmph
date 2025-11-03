all: run

test-cub: cub_radix_sort.cu helper.cu.h
	nvcc -O3 -std=c++17 cub_radix_sort.cu -o test-cub

run: test-cub
	./test-cub

clean:
	rm -f test-cub

