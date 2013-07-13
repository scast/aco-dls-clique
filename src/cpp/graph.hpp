#ifndef GRAPH_H
#define GRAPH_H
#include <vector>
#include <boost/dynamic_bitset.hpp>

struct graph_t {
    std::vector<boost::dynamic_bitset<> > matrix;
    std::vector<int> degree;
    int n, m, maxDegree;

    graph_t(int _n, int _m);
    void add_edge(int a, int b);
    bool connected(int a, int b);
    bool connectedAll(int x, boost::dynamic_bitset<>& cc);
    int swapWith(boost::dynamic_bitset<> bm);
    bool isDisconnectedFromOne(int x, boost::dynamic_bitset<>& cc, unsigned int bc);
    int disconnectedOne(int x, boost::dynamic_bitset<>& cc);
};

std::vector<int> improvementSet(graph_t *g, boost::dynamic_bitset<>& cc,
				boost::dynamic_bitset<> alreadyUsed);
std::vector<int> levelSet(graph_t *g, boost::dynamic_bitset<> cc,
			  boost::dynamic_bitset<> alreadyUsed);

std::vector<int> updateImprovementSet(graph_t *g, std::vector<int> i0, int v,
				      boost::dynamic_bitset<> alreadyUsed);
#endif
