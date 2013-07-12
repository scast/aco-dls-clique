#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>
#include <algorithm>
#include <ctime>
#include <boost/dynamic_bitset.hpp>
#include <boost/thread.hpp>
#include "local.hpp"
#include "graph.hpp"

std::vector<int> global_penalty;
boost::mutex m_;

state_t::state_t(graph_t *_g, int _maxSteps, int _penaltyDelay)
    :  numSteps(0), updateCycle(1), g(_g), maxSteps(_maxSteps),
       penaltyDelay(_penaltyDelay) {
    boost::lock_guard<boost::mutex> guard(m_);
    srand (time(NULL));
    int initialVertex = rand() % g->n;
    currentClique = boost::dynamic_bitset<>(g->n);
    currentClique[initialVertex] = 1;
    bestClique = currentClique;
    alreadyUsed = boost::dynamic_bitset<>(g->n);
    penalty.assign(global_penalty.begin(), global_penalty.end());
    currentImprovementSet = improvementSet(g, currentClique, alreadyUsed);
    lastAdded = initialVertex;
}

int state_t::select(std::vector<int>& s) {
    int minval = 0x3f3f3f3f, minpos;
    for (unsigned int i=0; i<s.size(); i++) {
	if (penalty[s[i]] < minval) minval = penalty[s[i]], minpos = s[i];
    }
    return minpos;
}

void state_t::expand() {
    int v;
    while (!currentImprovementSet.empty()) {
	v = select(currentImprovementSet);
	currentClique[v] = 1;
	alreadyUsed[v] = 1;
	lastAdded = v;
	numSteps++;
	currentImprovementSet = updateImprovementSet(g, currentImprovementSet, v,
						     alreadyUsed);
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
    }
}

void state_t::updateBest() {
    if (bestClique.count() < currentClique.count()) {
	bestClique = currentClique;
	std::cout << "Consegui un clique de tamano " << bestClique.count() << std::endl;
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
	penalty[i] = std::min(0, penalty[i] + dec + (currentClique[i] ? 1 : 0));
}

void state_t::restart() {
    int v = rand() % g->n;
    currentClique.reset();
    currentClique[v] = 1;
    alreadyUsed.reset();
    currentImprovementSet = improvementSet(g, currentClique, alreadyUsed);
    //    std::cout << numSteps << std::endl; // "Reiniciando en " << v << std::endl;
}

// struct DLS {
//     state_t st;
//     DLS(graph *g, int steps, int pd) : st(g, steps, pd) {}
//     void operator()() {
// 	// while (st->num)
//     }
// };

int dls(state_t& st) {
    while (st.numSteps < st.maxSteps) {
	// std::cout << "Paso -> " << st.numSteps << std::endl;
	st.expand();
	st.plateau();
	st.phases();
	st.update();
	st.restart();
    }
    return st.bestClique.count();
}

int main(int argc, char *argv[]) {
    char *filename = argv[1], *spd=argv[2], *steps=argv[3];
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
    state_t st(g, atoi(steps), atoi(spd));
    dls(st);
    return 0;
}
