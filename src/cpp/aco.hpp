#include <utility>
#include <boost/random.hpp>
#include "local.hpp"
#include "graph.hpp"

struct ant_state_t: public state_t {
    double *pheromones;
    int alpha;
    int *currentImprovementSet, isSz;
    double *p;
    boost::mt19937 rng;

    ant_state_t(graph_t *_g, int maxSteps, int pd,
		double *ph, int alph);
    ~ant_state_t();
    std::pair<int, int> selectImprove(int pos);
};

struct DynamicAntClique {
    // settings;
    double *pheromones;
    double tauMin, tauMax, rho;
    int alpha, maxSteps, antNumber;
    graph_t *g;
    set_t globalBest;
    int globalBestSize;

    int dlsMaxSteps, dlsPd;
    DLS *dls;

    DynamicAntClique(double _tMin, double _tMax, double _rho,
		     int _alpha, int _maxSteps, int _antNumber,
		     graph_t *_g, int _dlsMaxSteps, int _dlsPd);
    ~DynamicAntClique();
    void update();
    void operator()();
};
