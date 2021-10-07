.PHONY: run 

rasterizer.out: main.cpp
	clang++ -Wall -Werror -std=c++17 main.cpp -o rasterizer.out

