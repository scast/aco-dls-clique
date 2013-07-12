CFLAGS=-Wall -g -O3
CC=g++



main: local.o graph.o
	$(CC) -o go graph.o local.o $(CFLAGS)

local.o: local.cpp local.hpp
	$(CC) -o $@ -c local.cpp $(CFLAGS)

graph.o: graph.cpp graph.hpp
	$(CC) -o $@ -c graph.cpp $(CFLAGS)

clean:
	rm -rf local.o graph.o localcpp *.hi *.o
