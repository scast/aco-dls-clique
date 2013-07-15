#ifndef GRAPH_H
#define GRAPH_H
#include <vector>
#include <boost/dynamic_bitset.hpp>

typedef boost::dynamic_bitset<> set_t;

struct graph_t {
    std::vector<set_t > matrix;
    std::vector<int> degree;
    int n, m, maxDegree;

    graph_t(int _n, int _m);
    void add_edge(int a, int b);
    bool connected(int a, int b);
    bool connectedAll(int x, set_t& cc);
    int swapWith(set_t bm);
    bool isDisconnectedFromOne(int x, set_t& cc, unsigned int bc);
    int disconnectedOne(int x, set_t& cc);
};

void improvementSet(graph_t *g, set_t& cc, set_t& alreadyUsed,
		    std::vector<int>& ans);
void levelSet(graph_t *g, set_t& cc, set_t& alreadyUsed, std::vector<int>& ans);

void updateImprovementSet(graph_t *g, std::vector<int>& i0, int v,
			  set_t& alreadyUsed);
graph_t *parse(char *filename);
#endif
