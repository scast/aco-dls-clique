#include <boost/thread.hpp>
#include "local.hpp"
#include "graph.hpp"
#define MAX_THREADS 8


int main(int argc, char *argv[]) {
    char *filename = argv[1], *spd=argv[2], *steps=argv[3], *cons=argv[4];
    graph_t *g = parse(filename);
    boost::thread threads[MAX_THREADS];
    DLS *dls = new DLS[MAX_THREADS];
    std::cout << "Iniciando busqueda" << std::endl;
    for (int i=0; i<MAX_THREADS; i++)
	dls[i] = DLS(new state_t(g, atoi(steps), atoi(spd)));
    for (int c=0; c<atoi(cons); c++) {
    	for (int i=0; i<MAX_THREADS; i++) threads[i] = boost::thread(dls[i]);
    	for (int i=0; i<MAX_THREADS; i++) threads[i].join();
    	// for (int i=0; i<MAX_THREADS; i++) dls[i]();
    	// std::cout << "Consolidando" << std::endl;
    	sync(g->n, dls);
    }
    std::cout << "Listo" << std::endl;
    std::cout << "Maximo tamano -> " << maxSize << std::endl;
    std::cout << "Diferentes -> " << bests.size() << std::endl;
    return 0;
}
