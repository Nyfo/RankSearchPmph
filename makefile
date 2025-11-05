all: compile-all run

# Compile all Futhark programs and the CUDA test program:
compile-all: compile-cub compile-futhark

# Run all:
run: run-futhark run-cub

run-futhark: test-futhark bench-futhark

run-cub: compile-cub
	./test-cub

# Compile programs individually:
compile-cub: cub_radix_sort.cu helper.cu.h
	nvcc -O3 -std=c++17 cub_radix_sort.cu -o test-cub

compile-futhark:
	futhark c rankSearch.fut
	futhark c rankSearch_gen.fut
	futhark c partition3.fut

# Run tests:
test-futhark: rankSearch.fut rankSearch_gen.fut partition3.fut
	futhark test --backend=cuda rankSearch.fut
	futhark test --backend=cuda rankSearch_gen.fut
	futhark test --backend=cuda partition3.fut

# Run human reasoning rank search bench:
bench-futhark: rankSearch.fut
	futhark bench --backend=cuda rankSearch.fut

# Clean up generated files:
clean:
	rm -f test-cub
	rm -f rankSearch rankSearch.c
	rm -f rankSearch_gen rankSearch_gen.c
	rm -f partition3 partition3.c