#include <boost/dynamic_bitset.hpp>
#include <vector>
#include "graph.hpp"

graph_t::graph_t(int _n, int _m) : n(_n), m(_m) {
    matrix.resize(n);
    for (int i=0; i<n; i++)
	matrix[i] = boost::dynamic_bitset<>(n);
}

void graph_t::add_edge(int a, int b) {
    matrix[a][b] = 1;
    matrix[b][a] = 1;
}

bool graph_t::connected(int a, int b) {
    return matrix[a][b];
}

bool graph_t::connectedAll(int x, boost::dynamic_bitset<>& cc) {
    return (matrix[x] & cc) == cc;
}

int graph_t::swapWith(boost::dynamic_bitset<> bm) {
    return bm.find_first();
}

int graph_t::disconnectedOne(int x, boost::dynamic_bitset<>& cc) {
    return swapWith(cc ^ (matrix[x] & cc));
}

bool graph_t::isDisconnectedFromOne(int x, boost::dynamic_bitset<>& cc, unsigned int bc) {
    return bc - 1 == (matrix[x] & cc).count();
}

std::vector<int> improvementSet(graph_t *g, boost::dynamic_bitset<>& cc,
				boost::dynamic_bitset<> alreadyUsed) {
    std::vector<int> ans;
    ans.reserve(g->n);
    for (int i=0; i<g->n; i++) {
	if (!alreadyUsed[i] && !cc[i] && g->connectedAll(i, cc)) {
	    ans.push_back(i);
	}
    }
    return ans;
}

std::vector<int> levelSet(graph_t *g, boost::dynamic_bitset<>& cc,
			  boost::dynamic_bitset<> alreadyUsed) {
    std::vector<int> ans;
    ans.reserve(g->n);
    int bc = cc.count();
    for (int i=0; i<g->n; i++) {
	if (!cc[i] && !alreadyUsed[i] && g->isDisconnectedFromOne(i, cc, bc))
	    ans.push_back(i);
    }
    return ans;
}

std::vector<int> updateImprovementSet(graph_t *g, std::vector<int> i0, int v,
				      boost::dynamic_bitset<> alreadyUsed) {
    std::vector<int> ans;
    ans.reserve(i0.size());
    for (unsigned int i=0; i<i0.size(); i++) {
	if (g->connected(v, i) && !alreadyUsed[i])
	    ans.push_back(i);
    }
    return ans;
}
