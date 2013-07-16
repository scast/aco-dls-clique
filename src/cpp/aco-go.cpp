#include <cstdlib>
#include <iostream>
#include "aco.hpp"

int main(int argc, char **argv) {
    char *filename = argv[1];
    double tMin = atof(argv[2]), tMax = atof(argv[3]), rho = atof(argv[4]);
    int alpha = atoi(argv[5]), maxSteps = atoi(argv[6]), antNumber = atoi(argv[7]);
    graph_t *g = parse(filename);
    DynamicAntClique dac(tMin, tMax, rho, alpha, maxSteps, antNumber, g);
    std::cout << "Iniciando busqueda" << std::endl;
    dac();
    std::cout << "Listo" << std::endl;
    std::cout << "Maximo tamano -> " << maxSize << std::endl;
    std::cout << "Diferentes -> " << bests.size() << std::endl;
    return 0;
}
