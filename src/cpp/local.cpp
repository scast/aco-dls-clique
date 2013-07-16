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
double bestFirst;

state_t::state_t(graph_t *_g, int _maxSteps, int _penaltyDelay)
    :  numSteps(0), updateCycle(1), g(_g), maxSteps(_maxSteps),
       penaltyDelay(_penaltyDelay) {

    // Select a random node to start the search.
    srand (time(NULL));
    int initialVertex = rand() % g->n;

    // Store the current clique.
    currentClique = set_t(g->n);
    currentClique[initialVertex] = 1;
    bestClique = currentClique;
    lastAdded = initialVertex;

    // Mark which nodes are already tried on.
    alreadyUsed = set_t(g->n);

    // Initialize penalty, improvementSet, levelSet.
    penalty = new int[g->n];
    for (int i=0; i<g->n; i++)
	penalty[i] = 0;
    sorter.penalties = penalty;
    sortedPenalty = new int[g->n];
    for (int i=0; i<g->n; i++)
	sortedPenalty[i] = i;

    is = selectImprove(0);
    ls = -1;
}

std::pair<int, int> state_t::selectImprove(int pos) {
    return improvementSet(g, currentClique, alreadyUsed, sortedPenalty, pos);
}

int state_t::selectLevel() {
    return levelSet(g, currentClique, alreadyUsed, sortedPenalty);
}

state_t::~state_t() {
    delete[] penalty;
    delete[] sortedPenalty;
}

void state_t::expand() {
    int v;
    while (is.first != -1) {
	v = is.first;
	currentClique[v] = 1;
	alreadyUsed[v] = 1;
	lastAdded = v;
	numSteps++;
	is = selectImprove(is.second);
    }
    updateBest();
}

void state_t::plateau() {
    set_t currentCopy = currentClique;
    int remove;
    ls = selectLevel();
    while (ls != -1 && (currentClique & currentCopy).count() != 0) {
	remove = g->disconnectedOne(ls, currentClique);
	currentClique[ls] = 1;
	currentClique[remove] = 0;
	alreadyUsed[ls] = 1;
	numSteps++;
	lastAdded = ls;
	is = selectImprove(0);
	if (is.first != -1) break;
	ls = selectLevel();
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
		bestFirst = (double)clock()/CLOCKS_PER_SEC;
	    }
	    bests.insert(bestClique);
	}
    }
}

void state_t::phases() {
    while (is.first != -1) {
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
    int v;
    if (penaltyDelay > 1) {
	v = lastAdded;
	currentClique.reset();
	currentClique[v] = 1;
    } else {
	v = rand() % g->n;
	currentClique[v] = 1;
	for (int i=0; i<g->n; i++)
	    if (i != v && currentClique[i] && !g->connected(i, v))
		currentClique[i] = 0;
    }
    alreadyUsed.reset();
    std::sort(sortedPenalty, sortedPenalty+g->n, sorter);
    is = improvementSet(g, currentClique, alreadyUsed,
			sortedPenalty, 0);
}


void sync(int n, DLS *dls) {
    int *globalPenalty = new int[n];
    int *bestSizes = new int[MAX_THREADS];
    int totalSum = 0 ;
    for (int i=0; i<MAX_THREADS; i++)
	bestSizes[i] = dls[i].st->bestClique.count(), totalSum += bestSizes[i];
    for (int i=0; i<n; i++) {
    	for (int j=0; j<MAX_THREADS; j++)
    	    globalPenalty[i] += bestSizes[j]*dls[j].st->penalty[i];
	globalPenalty[i]  /= totalSum;
    }
    for (int i=0; i<MAX_THREADS; i++) {
	dls[i].st->numSteps = 0;
	for (int j=0; j<n; j++)
	    dls[i].st->penalty[j] = globalPenalty[j];
	std::sort(dls[i].st->sortedPenalty,
		  dls[i].st->sortedPenalty+n,
		  dls[i].st->sorter);
    }

    delete[] globalPenalty;
    delete[] bestSizes;
}

DLS::DLS() {}
DLS::DLS(state_t *_st) : st(_st) {}
void DLS::operator()() {
    while (st->numSteps < st->maxSteps) {
	st->expand();
	st->plateau();
	st->phases();
	st->update();
	st->restart();
    }
}
