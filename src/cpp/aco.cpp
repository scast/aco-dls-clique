#include <cmath>
#include <vector>
#include <algorithm>
#include <iostream>
#include <utility>
#include <boost/dynamic_bitset.hpp>
#include <boost/random.hpp>
#include <boost/thread.hpp>
#include "local.hpp"
#include "graph.hpp"
#include "aco.hpp"

ant_state_t::ant_state_t(graph_t *_g, int maxSteps, int pd,
			 double *ph, int alph)
	: state_t(_g, maxSteps, pd),
	  pheromones(ph), alpha(alph)
{
    currentImprovementSet = new int[g->n];
    isSz = improvementSet(g, currentClique, alreadyUsed, sortedPenalty,
			  currentImprovementSet);
    p = new double[g->n];
}

ant_state_t::~ant_state_t() {
    delete[] currentImprovementSet;
    delete[] p;
}

std::pair<int, int> ant_state_t::selectImprove(int recalc) {
    if (recalc == 0) {
	// recalcular el improvement set desde 0
	isSz = improvementSet(g, currentClique, alreadyUsed, sortedPenalty,
			      currentImprovementSet);
    } else {
	// filtramos el improvementSet.
	int sz = 0;
	for (int i=0; i<isSz; i++)
	    if (g->connected(lastAdded, currentImprovementSet[i]) &&
		!alreadyUsed[currentImprovementSet[i]])
		currentImprovementSet[sz++] = currentImprovementSet[i];
	isSz = sz;
    }
    if (isSz == 0)
	return std::make_pair(-1, g->n);

    int total = 0;
    for (int i=0; i<isSz; i++) {
	p[i] = pow(pheromones[currentImprovementSet[i]], alpha) + total;
	total = p[i];
    }
    boost::uniform_real<double> u(0, total);
    boost::variate_generator<boost::mt19937&, boost::uniform_real<double> > gen(rng, u);
    double r = gen();
    return std::make_pair(currentImprovementSet[std::upper_bound(p, p+isSz, r)-p], 1);
}

DynamicAntClique::DynamicAntClique(double _tMin, double _tMax, double _rho,
				   int _alpha, int _maxSteps, int _antNumber,
				   graph_t *_g, int _dlsMaxSteps, int _dlsPd)
    : tauMin(_tMin), tauMax(_tMax), rho(_rho), alpha(_alpha),
      maxSteps(_maxSteps), antNumber(_antNumber), g(_g), globalBestSize(-1),
      dlsMaxSteps(_dlsMaxSteps), dlsPd(_dlsPd)
{
    pheromones = new double[g->n];
    for (int i=0; i<g->n; i++)
	pheromones[i] = tauMax;
    dls = new DLS[antNumber];
    for (int i=0; i<antNumber; i++)
	dls[i] = DLS(new ant_state_t(g, dlsMaxSteps, dlsPd,
				     pheromones, alpha));
}

DynamicAntClique::~DynamicAntClique() {
    delete[] dls;
}


void DynamicAntClique::update() {
    // Evaporate pheromones.
    for (int i=0; i<g->n; i++)
	pheromones[i] *= rho;

    // Find the biggest clique found in this run
    set_t best=dls[0].st->bestClique;
    int bestSize=best.count();
    for (int i=1; i<antNumber; i++)
    	if (bestSize < dls[i].st->bestClique.count())
    	    best = dls[i].st->bestClique, bestSize = best.count();
    for (int i=0; i<antNumber; i++)
	dls[i].st->bestClique.clear();

    // Update global best clique
    if (globalBestSize < bestSize)
	globalBestSize = bestSize, globalBest = best;

    // Update the pheromone trail
    double upd = 1/double(1+globalBestSize-bestSize);
    for (int i=0; i<g->n; i++) {
	if (best[i])
	    pheromones[i] += upd;
	pheromones[i] = std::max(tauMin, pheromones[i]);
	pheromones[i] = std::min(tauMax, pheromones[i]);
    }

}

void DynamicAntClique::operator()() {
    boost::thread threads[antNumber];
    for (int c=0; c<maxSteps; c++) {
	//	std::cout << c << std::endl;
    	for (int i=0; i<antNumber; i++)
    	    threads[i] = boost::thread(dls[i]);
    	for (int i=0; i<antNumber; i++)
    	    threads[i].join();
	sync(g->n, dls);
    	update();
    }

}
