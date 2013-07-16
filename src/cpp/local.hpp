#ifndef LOCAL_H
#define LOCAL_H
#include "graph.hpp"
#include <utility>
#include <boost/dynamic_bitset.hpp>
// #include <vector>
#include <set>

extern std::set<set_t> bests;
extern int maxSize;


struct penalty_sorter {
    int *penalties;

    penalty_sorter() {};
    bool operator() (int i, int j) {
	return (penalties[i]<penalties[j]);
    }
};

struct state_t {
    // real state
    set_t currentClique, bestClique, alreadyUsed;
    int *penalty; // , *currentLevelSet;
    int numSteps, lastAdded, updateCycle;
    std::pair<int, int> is;
    int ls;
    int *sortedPenalty;
    penalty_sorter sorter;

    // settings
    graph_t *g;
    int maxSteps, penaltyDelay;


    // state_t(state_t *st);
    state_t(graph_t *g, int maxSteps, int penaltyDelay);
    ~state_t();
    void expand();
    void plateau();
    void updateBest();
    void phases();
    virtual std::pair<int, int> selectImprove(int pos);
    virtual int selectLevel();
    void update();
    void restart();
};

struct DLS {
    state_t *st;
    DLS();
    DLS(state_t *_st);
    void operator()();
};

void sync(int n, DLS *dls);

#endif
