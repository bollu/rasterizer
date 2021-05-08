.PHONY: run

run: main
	./main

main: main.zig
	zig build-exe main.zig -femit-llvm-ir
