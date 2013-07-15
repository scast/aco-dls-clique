#ifndef LOCAL_H
#define LOCAL_H
#include "graph.hpp"
#include <boost/dynamic_bitset.hpp>
#include <vector>
#include <set>

extern std::set<set_t> bests;
extern int maxSize;

struct state_t {
    // real state
    set_t currentClique, bestClique, alreadyUsed;
    std::vector<int> penalty, currentImprovementSet;
    int numSteps, lastAdded, updateCycle;

    // settings
    graph_t *g;
    int maxSteps, penaltyDelay;


    // state_t(state_t *st);
    state_t(graph_t *g, int maxSteps, int penaltyDelay);
    state_t(graph_t *_g, int _maxSteps, int _penaltyDelay, std::vector<int>& gp);
    void expand();
    void plateau();
    void updateBest();
    void phases();
    virtual int select(std::vector<int>& s);
    virtual void update();
    virtual void restart();
};

struct DLS {
    state_t *st;
    DLS(state_t *_st);
    void operator()();
};

std::vector<int> sync(int n, std::vector<DLS>& dls);

#endif
