.PHONY: run 

kaboom.out: kaboom.cpp
	clang++ -Wall -g -Werror -std=c++17 kaboom.cpp -o kaboom.out -fsanitize=address -fsanitize=undefined

rasterizer.out: main.cpp
	clang++ -Wall -g -Werror -std=c++17 main.cpp -o rasterizer.out -fsanitize=address -fsanitize=undefined

