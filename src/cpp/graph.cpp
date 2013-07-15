#include <fstream>
#include <sstream>
#include <vector>
#include <utility>
#include <map>
#include <boost/dynamic_bitset.hpp>
#include <boost/thread.hpp>
#include "graph.hpp"
#include <iostream>

graph_t::graph_t(int _n, int _m) : n(_n), m(_m) {
    matrix.resize(n);
    degree.resize(n);
    for (int i=0; i<n; i++)
	matrix[i] = set_t(n);
}

void graph_t::add_edge(int a, int b) {
    matrix[a][b] = 1;
    matrix[b][a] = 1;
    degree[a]++;
    degree[b]++;
}

bool graph_t::connected(int a, int b) {
    return matrix[a][b];
}

bool graph_t::connectedAll(int x, set_t& cc) {
    return (matrix[x] & cc) == cc;
}

// int graph_t::swapWith(set_t bm, int lo, int hi) {
//     int mid = (lo+hi)/2;
//     // std::cout << "mid=" << mid << std::endl;
//     if (bm[mid]) return mid;
//     else if (! (bm & masks[mid]).count())
//     	return swapWith(bm, mid+1, hi);
//     else
//     	return swapWith(bm, lo, mid-1);
// }

int graph_t::swapWith(set_t bm) {
    return bm.find_first();
}

int graph_t::disconnectedOne(int x, set_t& cc) {
    return swapWith(cc ^ (matrix[x] & cc));
}

bool graph_t::isDisconnectedFromOne(int x, set_t& cc, unsigned int bc) {
    return bc - 1 == (matrix[x] & cc).count();
}
void filterLevelSet(std::vector<int> levelSet, set_t alreadyUsed,
		    std::vector<int>& ans) {
    for (unsigned int i=0; i<levelSet.size(); i++)
	if (!alreadyUsed[levelSet[i]])
	    ans.push_back(levelSet[i]);
}

void improvementSet(graph_t *g, set_t& cc,
		    set_t& alreadyUsed,
		    std::vector<int>& ans) {
    ans.clear();
    for (int i=0; i<g->n; i++) {
	if (!cc[i] && !alreadyUsed[i] && g->connectedAll(i, cc)) {
	    ans.push_back(i);
	}
    }
}

void levelSet(graph_t *g, set_t& cc, set_t& alreadyUsed,
	      std::vector<int>& ans) {
    ans.clear();
    int bc = cc.count();
    for (int i=0; i<g->n; i++) {
	if (!cc[i] && !alreadyUsed[i] && g->isDisconnectedFromOne(i, cc, bc))
	    ans.push_back(i);
    }
}

void updateImprovementSet(graph_t *g, std::vector<int>& i0, int v,
			  set_t& alreadyUsed) {
    int p=0;
    for (unsigned int i=0; i<i0.size(); i++) {
	if (g->connected(v, i0[i]) && !alreadyUsed[i0[i]])
	    i0[p++] = i0[i];
    }
    i0.resize(p);
}

graph_t *parse(char *filename) {
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
		continue;
	    }
	    int a, b;
	    iss >> g1 >> a >> b;
	    g->add_edge(a-1, b-1);
	}
    return g;
}
