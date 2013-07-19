CFLAGS=-Wall -g -O3
CC=g++

main:
	ghc --make main.hs -O3 -rtsopts -fforce-recomp -threaded

main-prof:
	ghc --make main.hs -O3 -prof -auto-all -caf-all -rtsopts -fforce-recomp -threaded

clean:
	rm -rf *.o *.hi local main
