#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>
#include <algorithm>
#include <map>
#include <set>
#include <ctime>
#include <boost/dynamic_bitset.hpp>
#include <boost/thread.hpp>
#include "local.hpp"
#include "graph.hpp"
#define MAX_THREADS 8

boost::shared_mutex _access;
std::set<set_t> bests;
int maxSize;

state_t::state_t(graph_t *_g, int _maxSteps, int _penaltyDelay)
    :  numSteps(0), updateCycle(1), g(_g), maxSteps(_maxSteps),
       penaltyDelay(_penaltyDelay), penalty(_g->n, 0) {
    srand (time(NULL));
    int initialVertex = rand() % g->n;
    currentClique = set_t(g->n);
    currentClique[initialVertex] = 1;
    bestClique = currentClique;
    alreadyUsed = set_t(g->n);
    currentImprovementSet.reserve(g->n);
    improvementSet(g, currentClique, alreadyUsed, currentImprovementSet);
    lastAdded = initialVertex;
}

int state_t::select(std::vector<int>& s) {
    int minval = 0x3f3f3f3f, minpos;
    for (unsigned int i=0; i<s.size(); i++)
	if (penalty[s[i]] < minval) minval = penalty[s[i]], minpos = s[i];
    std::vector<int> going;
    going.reserve(g->n);
    for (int i=0; i<s.size(); i++)
	if (penalty[s[i]] == minval)
	    going.push_back(s[i]);
    int r =rand() % going.size();
    return going[r]; // minpos;
}

void state_t::expand() {
    int v;
    while (!currentImprovementSet.empty()) {
	// std::cout << "Aqui 01" << std::endl;
	v = select(currentImprovementSet);
	// std::cout << "Aqui 01" << std::endl;
	currentClique[v] = 1;
	// std::cout << "Aqui 01" << std::endl;
	alreadyUsed[v] = 1;
	// std::cout << "Aqui 01" << std::endl;
	lastAdded = v;
	// std::cout << "Aqui 01" << std::endl;
	numSteps++;
	// std::cout << "Aqui 01" << std::endl;
	// currentImprovementSet = improvementSet(g, currentClique, alreadyUsed);
	// std::cout << "Aqui 02" << std::endl;
	updateImprovementSet(g, currentImprovementSet, v, alreadyUsed);
	// std::cout << "Aqui 03" << std::endl;
    }
    updateBest();
}

void state_t::plateau() {
    set_t currentCopy = currentClique;
    std::vector<int> ls;
    ls.reserve(g->n);
    levelSet(g, currentClique, alreadyUsed, ls);
    int remove, v;
    while ((currentClique & currentCopy).count() != 0 && !ls.empty()){
	v = select(ls);
	remove = g->disconnectedOne(v, currentClique);
	currentClique[v] = 1;
	currentClique[remove] = 0;
	alreadyUsed[v] = 1;
	numSteps++;
	lastAdded = v;
	improvementSet(g, currentClique, alreadyUsed,
		       currentImprovementSet);
	if (!currentImprovementSet.empty()) break;
	levelSet(g, currentClique, alreadyUsed, ls);
    }
}

void state_t::updateBest() {
    boost::upgrade_lock<boost::shared_mutex> lock(_access);
    int cnt = currentClique.count();
    if (bestClique.count() < cnt) {
	bestClique = currentClique;
	boost::upgrade_to_unique_lock<boost::shared_mutex> uniqueLock(lock);
	if (maxSize <= cnt) {
	    if (maxSize < cnt) {
		std::cout << "Consegui un clique de tamano " << cnt << std::endl;
		bool ok=true;
		for (int i=0; i<g->n&&ok; i++)
		    if (currentClique[i])
			for (int j=i+1; j<g->n&&ok; j++)
			    if (currentClique[j])
				ok = g->connected(i, j);
		if (ok) std::cout << "Valido" << std::endl;
		else std::cout << "Invalido" << std::endl;
		maxSize = cnt;
		bests.clear();
	    }
	    bests.insert(bestClique);
	}
    }
}

void state_t::phases() {
    while (!currentImprovementSet.empty()) {
	expand();
	plateau();
    }
}

void state_t::update() {
    int dec = (updateCycle % penaltyDelay == 0) ? -1 : 0;
    for (int i=0; i<g->n; i++)
	penalty[i] = std::max(0, penalty[i] + dec + (currentClique[i] ? 1 : 0));
    updateCycle++;
}

void state_t::restart() {
    if (penaltyDelay > 1) {
	int v = lastAdded;
	currentClique.reset();
	currentClique[v] = 1;
    } else {
	int v = rand() % g->n;
	currentClique[v] = 1;
	for (int i=0; i<g->n; i++)
	    if (i != v && currentClique[i] && !g->connected(i, v))
		currentClique[i] = 0;
    }
    alreadyUsed.reset();
    improvementSet(g, currentClique, alreadyUsed, currentImprovementSet);
}


std::vector<int> sync(int n, std::vector<DLS>& dls) {
    std::vector<int> global_penalty(n, 0);
    std::vector<int> bestSizes(MAX_THREADS);
    int totalSum = 0 ;
    for (int i=0; i<MAX_THREADS; i++)
	bestSizes[i] = dls[i].st->bestClique.count(), totalSum += bestSizes[i];
    for (int i=0; i<n; i++) {
    	for (int j=0; j<MAX_THREADS; j++)
    	    global_penalty[i] += bestSizes[j]*dls[j].st->penalty[i];
	global_penalty[i]  /= totalSum;
    }
    return global_penalty;
}


DLS::DLS(state_t *_st) : st(_st) {}
void DLS::operator()() {
    while (st->numSteps < st->maxSteps) {
	// std::cout << "Expand" << std::endl;
	st->expand();
	// std::cout << "Plateau" << std::endl;
	st->plateau();
	// std::cout << "Phases" << std::endl;
	st->phases();
	// std::cout << "Update" << std::endl;
	st->update();
	// std::cout << "Restart" << std::endl;
	st->restart();
	// std::cout << "Looping" << std::endl;
    }
}
