#ifndef GRAPH_H
#define GRAPH_H
// #include <vector>
#include <boost/dynamic_bitset.hpp>
#include <utility>

typedef boost::dynamic_bitset<> set_t;

struct graph_t {
    set_t *matrix;
    int *degree;
    int n, m, maxDegree;

    graph_t(int _n, int _m);
    ~graph_t();
    void add_edge(int a, int b);
    bool connected(int a, int b);
    bool connectedAll(int x, set_t& cc);
    int swapWith(set_t bm);
    bool isDisconnectedFromOne(int x, set_t& cc, unsigned int bc);
    int disconnectedOne(int x, set_t& cc);
};


std::pair<int, int> improvementSet(graph_t *g, set_t& cc, set_t& alreadyUsed,
				   int *sortedPenalty, int pos);
// int improvementSet(graph_t *g, set_t& cc, set_t& alreadyUsed, int *ans);
int levelSet(graph_t *g, set_t& cc, set_t& alreadyUsed, int *sortedPenalty);
// int updateImprovementSet(graph_t *g, int *i0, int i0sz, int v,
// 			 set_t& alreadyUsed);
graph_t *parse(char *filename);
#endif
