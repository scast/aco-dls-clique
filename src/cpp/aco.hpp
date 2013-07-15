#include <vector>
#include "local.hpp"
#include "graph.hpp"

struct ant_state_t: public state_t {
    std::vector<double> pheromones;
    int alpha;
    ant_state_t(graph_t *_g, std::vector<double> ph, int alph);
    void update();
    void restart();
    int select(std::vector<int>& s);
};

struct DynamicAntClique {
    // settings;
    std::vector<double> pheromones;
    double tauMin, tauMax, rho;
    int alpha, maxSteps, antNumber;
    graph_t *g;
    set_t globalBest;
    int globalBestSize;
    DynamicAntClique(double _tMin, double _tMax, double _rho,
		     int _alpha, int _maxSteps, int _antNumber, graph_t *_g);
    void update();
    void operator()();
};
