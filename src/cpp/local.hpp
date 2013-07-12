#ifndef LOCAL_H
#define LOCAL_H
#include "graph.hpp"
#include <boost/dynamic_bitset.hpp>
#include <vector>

struct state_t {
    // real state
    boost::dynamic_bitset<> currentClique, bestClique, alreadyUsed;
    std::vector<int> penalty, currentImprovementSet;
    int numSteps, lastAdded, updateCycle;

    // settings
    graph_t *g;
    int maxSteps, penaltyDelay;

    state_t(graph_t *g, int maxSteps, int penaltyDelay);

    void expand();
    void plateau();
    void updateBest();
    int select(std::vector<int>& s);
    void phases();
    void update();
    void restart();
};

int dls(state_t& initial);
#endif
