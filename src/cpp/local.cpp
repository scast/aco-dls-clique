#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>
#include <algorithm>
#include <map>
#include <ctime>
#include <boost/dynamic_bitset.hpp>
#include <boost/thread.hpp>
#include "local.hpp"
#include "graph.hpp"
#define MAX_THREADS 8
std::vector<int> global_penalty;

state_t::state_t(graph_t *_g, int _maxSteps, int _penaltyDelay)
    :  numSteps(0), updateCycle(1), g(_g), maxSteps(_maxSteps),
       penaltyDelay(_penaltyDelay), penalty(_g->n, 0) {
    // boost::lock_guard<boost::mutex> guard(m_);
    srand (time(NULL));
    int initialVertex = rand() % g->n;
    currentClique = boost::dynamic_bitset<>(g->n);
    currentClique[initialVertex] = 1;
    bestClique = currentClique;
    alreadyUsed = boost::dynamic_bitset<>(g->n);
    // penalty.assign(g->n, 0); // global_penalty.begin(), global_penalty.end());
    currentImprovementSet = improvementSet(g, currentClique, alreadyUsed);
    lastAdded = initialVertex;
}

state_t::state_t(graph_t *_g, int _maxSteps, int _penaltyDelay,
		 std::vector<int>& gp)
    :  numSteps(0), updateCycle(1), g(_g), maxSteps(_maxSteps),
       penaltyDelay(_penaltyDelay), penalty(gp.begin(), gp.end()) {
    // boost::lock_guard<boost::mutex> guard(m_);
    srand (time(NULL));
    int initialVertex = rand() % g->n;
    currentClique = boost::dynamic_bitset<>(g->n);
    currentClique[initialVertex] = 1;
    bestClique = currentClique;
    alreadyUsed = boost::dynamic_bitset<>(g->n);
    // penalty.assign(g->n, 0); // global_penalty.begin(), global_penalty.end());
    currentImprovementSet = improvementSet(g, currentClique, alreadyUsed);
    lastAdded = initialVertex;
}


int state_t::select(std::vector<int>& s) {
    int minval = 0x3f3f3f3f, minpos;
    for (unsigned int i=0; i<s.size(); i++) {
	if (penalty[s[i]] < minval) minval = penalty[s[i]], minpos = s[i];
    }
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
	v = select(currentImprovementSet);
	// std::cout << v << std::endl;
	currentClique[v] = 1;
	alreadyUsed[v] = 1;
	lastAdded = v;
	numSteps++;
	currentImprovementSet = improvementSet(g, currentClique, alreadyUsed); // updateImprovementSet(g, currentImprovementSet, v,
						     // alreadyUsed);
    }
    updateBest();
}

void state_t::plateau() {
    boost::dynamic_bitset<> currentCopy = currentClique;
    std::vector<int> ls = levelSet(g, currentClique, alreadyUsed);
    int remove, v;
    while ((currentClique & currentCopy).count() != 0 && !ls.empty()){
	v = select(ls);
	remove = g->disconnectedOne(v, currentClique);
	// std::cout << "Seleccione -> " << v << std::endl;
	// std::cout << "Cambiando por -> " << remove << std::endl;
	currentClique[v] = 1;
	currentClique[remove] = 0;
	alreadyUsed[v] = 1;
	numSteps++;
	lastAdded = v;
	currentImprovementSet = improvementSet(g, currentClique, alreadyUsed);
	if (!currentImprovementSet.empty()) break;
	ls = levelSet(g, currentClique, alreadyUsed);
	// std::cout << "Llegue" << std::endl;
    }
}

void state_t::updateBest() {
    if (bestClique.count() < currentClique.count()) {
	bestClique = currentClique;
	std::cout << "Consegui un clique de tamano " << bestClique.count() << std::endl;
	bool ok=true;
	for (int i=0; i<g->n&&ok; i++)
	    if (currentClique[i])
		for (int j=i+1; j<g->n&&ok; j++)
		    if (currentClique[j])
			ok = g->connected(i, j);
	if (ok) std::cout << "Valido" << std::endl;
	else std::cout << "Invalido" << std::endl;
    }
}

void state_t::phases() {
    while (!currentImprovementSet.empty()) {
	expand();
	plateau();
    }
}

void state_t::update() {
    // boost::lock_guard<boost::mutex> guard(m_);
    int dec = (updateCycle % penaltyDelay == 0) ? -1 : 0;
    for (int i=0; i<g->n; i++)
	penalty[i] = std::max(0, penalty[i] + dec + (currentClique[i] ? 1 : 0));
    // penalty.assign(global_penalty.begin(), global_penalty.end());
    updateCycle++;
}

void state_t::restart() {
    int v = lastAdded; // rand() % g->n;
    currentClique.reset();
    currentClique[v] = 1;
    alreadyUsed.reset();
    currentImprovementSet = improvementSet(g, currentClique, alreadyUsed);
    //    std::cout << numSteps << std::endl; // "Reiniciando en " << v << std::endl;
}

struct DLS {
    state_t *st;
    DLS(state_t *_st) : st(_st) {}
    // DLS(graph_t *g, int steps, int pd) : st(g, steps, pd) {
    // }

    // DLS(graph_t *g, int steps, int pd, std::vector<int>& gp) : st(g, steps, pd, gp) {
    // }
    void operator()() {
	// std::cout << "Entre aqui con " << st->numSteps << std::endl;
	// std::cout << st->numSteps << std::endl;
	// std::cout << st->lastAdded << std::endl;
	// std::cout << st->g->n << std::endl;
	// std::cout << st->g->m << std::endl;
    	while (st->numSteps < st->maxSteps) {
	    st->expand();
	    st->plateau();
	    st->phases();
	    st->update();
	    st->restart();
    	}
	// std::cout << "Sali aqui con " << st->numSteps << std::endl;
	for (int i=0; i<st->g->n; i++) std::cout << st->penalty[i] << " ";
	std::cout << std::endl;
	std::cout << "------" << std::endl;
    }
};


// sincroniza las tablas de todos los threads, cuando termine,
// se reinicia la busqueda.
std::vector<int> sync(int n, std::vector<DLS>& dls) {
    std::vector<int> global_penalty(n, 0);
    // consolidamos todos los penalties en una sola tabla
    for (int i=0; i<n; i++)
    	for (int j=0; j<dls.size(); j++)
    	    global_penalty[i] += dls[j].st->penalty[i];
    return global_penalty;
}

int main(int argc, char *argv[]) {
    char *filename = argv[1], *spd=argv[2], *steps=argv[3], *cons=argv[4];
    std::fstream fin(filename);
    std::string line;
    graph_t *g;
    while(std::getline(fin, line))
	{
	    //the following line trims white space from the beginning of the string
	    line.erase(line.begin(), std::find_if(line.begin(), line.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
	    std::istringstream iss(line);
	    std::string g1, g2;
	    if(line[0] == 'c') continue;
	    if (line[0] == 'p') {
		int n, m;
		iss >> g1 >> g2 >> n >> m;
		g = new graph_t(n, m);
		global_penalty.assign(g->n, 0);
		continue;
	    }
	    int a, b;
	    iss >> g1 >> a >> b;
	    g->add_edge(a-1, b-1);
	}
    // // state_t st(g, atoi(steps), atoi(spd));
    // // std::vector<boost::thread> threads(MAX_THREADS);
    boost::thread threads[MAX_THREADS];
    std::vector<DLS> dls;
    for (int i=0; i<MAX_THREADS; i++) dls.push_back(DLS(new state_t(g, atoi(steps), atoi(spd))));
    // for (int c=0; c<atoi(cons); c++) {
    for (int i=0; i<MAX_THREADS; i++) threads[i] = boost::thread(dls[i]);
    for (int i=0; i<MAX_THREADS; i++) threads[i].join();
    	// std::cout << "Consolidando" << std::endl;
    // 	std::vector<int> global_penalties = sync(g->n, dls);
    // 	for (int i=0; i<g->n; i++) std::cout << global_penalties[i] << " ";
    // 	std::cout << std::endl;
    // 	for (int i=0; i<MAX_THREADS; i++) dls[i] = DLS(new state_t(g, atoi(steps), atoi(spd),
    // 								   global_penalties));
    // }
    // DLS x(g, atoi(steps), atoi(spd));
    // boost::thread thread(x);
    // dls(st);
    return 0;
}
