#include <cstdlib>
#include <iostream>
#include <ctime>
#include "aco.hpp"
#include "local.hpp"

int main(int argc, char **argv) {
    char *filename = argv[1];
    double tMin = atof(argv[2]), tMax = atof(argv[3]), rho = atof(argv[4]);
    int alpha = atoi(argv[5]), maxSteps = atoi(argv[6]), antNumber = atoi(argv[7]),
	pd = atoi(argv[8]), dlsMS = atoi(argv[9]);
    graph_t *g = parse(filename);
    double start = (double)clock()/CLOCKS_PER_SEC;
    DynamicAntClique dac(tMin, tMax, rho, alpha, maxSteps, antNumber, g, dlsMS, pd);
    std::cout << "Iniciando busqueda" << std::endl;
    dac();
    std::cout << "Listo" << std::endl;
    std::cout << "Maximo tamano -> " << maxSize << std::endl;
    std::cout << "Diferentes -> " << bests.size() << std::endl;
    std::cout << "Primer mejor en " << bestFirst - start << std::endl;
    return 0;
}
