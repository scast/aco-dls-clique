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

std::vector<set_t> foundCliques;
boost::mutex m_;

ant_state_t::ant_state_t(graph_t *_g, std::vector<double> ph, int alph)
	: state_t(_g, 1, 1),
	  pheromones(ph.begin(), ph.end()), alpha(alph)
{}

void ant_state_t::update() {
    boost::lock_guard<boost::mutex> guard(m_);
    foundCliques.push_back(bestClique);
}

void ant_state_t::restart() {
    // no need to restart
}

int ant_state_t::select(std::vector<int>& s) {
    std::vector<double> p(s.size());
    int total = 0;
    for (int i=0; i<s.size(); i++) {
    	p[i] = pow(pheromones[s[i]], alpha) + total;
    	total = p[i];
    }
    boost::mt19937 rng;
    boost::uniform_real<double> u(0, total);
    boost::variate_generator<boost::mt19937&, boost::uniform_real<double> > gen(rng, u);
    double r = gen();
    return s[upper_bound(p.begin(), p.end(), r)-p.begin()];
    // return s[rand() % s.size()];
}

DynamicAntClique::DynamicAntClique(double _tMin, double _tMax, double _rho,
				   int _alpha, int _maxSteps, int _antNumber, graph_t *_g)
    : tauMin(_tMin), tauMax(_tMax), rho(_rho), alpha(_alpha),
      maxSteps(_maxSteps), antNumber(_antNumber), g(_g), globalBestSize(-1)
{
    pheromones.assign(g->n, tauMax);
}

void DynamicAntClique::update() {
    // Evaporate pheromones.
    for (int i=0; i<pheromones.size(); i++)
	pheromones[i] *= rho;

    // Find the biggest clique found in this run
    set_t best=foundCliques[0];
    int bestSize=foundCliques[0].count();
    for (int i=1; i<foundCliques.size(); i++)
	if (bestSize < foundCliques[i].count())
	    best = foundCliques[i], bestSize = foundCliques[i].count();

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
    foundCliques.clear();
}

void DynamicAntClique::operator()() {
    for (int c=0; c<maxSteps; c++) {
	std::cout << "It -> " << c << std::endl;
    	std::vector<DLS> dls;
    	// inicializar estructuras
    	for (int i=0; i<antNumber; i++) {
    	    ant_state_t *st = new ant_state_t(g, pheromones, alpha);
    	    dls.push_back(DLS(st));
	    // dls[i]();
    	}
	// std::cout << "Begin." << std::endl;
    	// iniciar busqueda
    	// // std::vector<boost::thread> threads; //(antNumber);
    	boost::thread threads[antNumber];
    	for (int i=0; i<antNumber; i++)
    	    threads[i] = boost::thread(dls[i]);
    	for (int i=0; i<antNumber; i++)
    	    threads[i].join();
	// std::cout << "End." << std::endl;
    	update();
    }

}
