CFLAGS=-Wall -O3   -lboost_thread-mt
CC=g++



local-go: local.o graph.o local-go.o
	$(CC) -o local-go local-go.o graph.o local.o $(CFLAGS)

aco-go: aco.o local.o graph.o aco-go.o
	$(CC) -o aco-go aco.o local.o graph.o aco-go.o $(CFLAGS)


aco-go.o: aco-go.cpp
	$(CC) -o $@ -c aco-go.cpp $(CFLAGS)

aco.o: aco.cpp
	$(CC) -o $@ -c aco.cpp $(CFLAGS)

local-go.o: local-go.cpp
	$(CC) -o $@ -c local-go.cpp $(CFLAGS)

local.o: local.cpp local.hpp
	$(CC) -o $@ -c local.cpp $(CFLAGS)

graph.o: graph.cpp graph.hpp
	$(CC) -o $@ -c graph.cpp $(CFLAGS)

clean:
	rm -rf local.o graph.o localcpp *.hi *.o
