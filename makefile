.PHONY: run  kaboom


rasterizer: rasterizer.out
	rm out.ppm
	./rasterizer.out || true # disable leansan causing failure
	rifle out.ppm

kaboom: kaboom.out
	./kaboom.out
	rifle out.ppm


rasterizer.out: main.cpp
	clang++ -Wall -g -Werror -std=c++17 main.cpp -o rasterizer.out -fsanitize=address -fsanitize=undefined -O3

kaboom.out: kaboom.cpp
	clang++ -Wall -g -Werror -std=c++17 kaboom.cpp -o kaboom.out -fsanitize=address -fsanitize=undefined -O3


