#include <vector>
#include <map>
#include <boost/dynamic_bitset.hpp>
#include <boost/thread.hpp>
#include "graph.hpp"
#include <iostream>

std::map<boost::dynamic_bitset<>, std::vector<int> > memoLevel;
std::map<boost::dynamic_bitset<>, std::vector<int> > memoImpr;
boost::shared_mutex ml_, mi_;

graph_t::graph_t(int _n, int _m) : n(_n), m(_m) {
    matrix.resize(n);
    degree.resize(n);
    for (int i=0; i<n; i++)
	matrix[i] = boost::dynamic_bitset<>(n);
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

bool graph_t::connectedAll(int x, boost::dynamic_bitset<>& cc) {
    return (matrix[x] & cc) == cc;
}

// int graph_t::swapWith(boost::dynamic_bitset<> bm, int lo, int hi) {
//     int mid = (lo+hi)/2;
//     // std::cout << "mid=" << mid << std::endl;
//     if (bm[mid]) return mid;
//     else if (! (bm & masks[mid]).count())
//     	return swapWith(bm, mid+1, hi);
//     else
//     	return swapWith(bm, lo, mid-1);
// }

int graph_t::swapWith(boost::dynamic_bitset<> bm) {
    return bm.find_first();
}

int graph_t::disconnectedOne(int x, boost::dynamic_bitset<>& cc) {
    return swapWith(cc ^ (matrix[x] & cc));
}

bool graph_t::isDisconnectedFromOne(int x, boost::dynamic_bitset<>& cc, unsigned int bc) {
    return bc - 1 == (matrix[x] & cc).count();
}
void filterLevelSet(std::vector<int>& levelSet, boost::dynamic_bitset<> alreadyUsed,
		    std::vector<int>& ans) {
    for (unsigned int i=0; i<levelSet.size(); i++)
	if (!alreadyUsed[levelSet[i]])
	    ans.push_back(levelSet[i]);
}

std::vector<int> improvementSet(graph_t *g, boost::dynamic_bitset<>& cc,
				boost::dynamic_bitset<> alreadyUsed) {
    // boost::upgrade_lock<boost::shared_mutex> lock(mi_);
    std::vector<int> ans;
    std::vector<int> m;
    ans.reserve(g->n);
    if (!memoImpr.count(cc)) {
	m.reserve(g->n);
	for (int i=0; i<g->n; i++) {
	    if (!cc[i] && g->connectedAll(i, cc)) {
		ans.push_back(i);
	    }
	}
	// boost::upgrade_to_unique_lock<boost::shared_mutex> uniqueLock(lock);
	memoImpr[cc] = m;
    } else {
	m = memoImpr[cc];
    }
    filterLevelSet(m, alreadyUsed, ans);
    return ans;
}
// std::vector<int> levelSet(graph_t *g, boost::dynamic_bitset<>& cc,
// 			  boost::dynamic_bitset<> alreadyUsed) {
//     std::vector<int> ans;
//     ans.reserve(g->n);
//     if (memoLevel.count(cc)) {
// 	std::vector<int> m = memoLevel[cc];
// 	filterLevelSet(m, alreadyUsed, ans);
//     } else {
// 	std::vector<int> m;
// 	int bc = cc.count();
// 	m.reserve(g->n);
// 	for (int i=0; i<g->n; i++)
// 	    if (!cc[i] && g->isDisconnectedFromOne(i, cc, bc))
// 		m.push_back(i);
// 	memoLevel[cc] = m;
// 	filterLevelSet(m, alreadyUsed, ans);
//     }
//     return ans;
// }

std::vector<int> levelSet(graph_t *g, boost::dynamic_bitset<> cc,
			  boost::dynamic_bitset<> alreadyUsed) {
    // boost::upgrade_lock<boost::shared_mutex> lock(ml_);
    std::vector<int> ans;
    std::vector<int> m;
    ans.reserve(g->n);
    int bc = cc.count();
    if (!memoLevel.count(cc)) {
	m.reserve(g->n);
	for (int i=0; i<g->n; i++) {
	    if (!cc[i] && g->isDisconnectedFromOne(i, cc, bc))
		m.push_back(i);
	}
	// boost::upgrade_to_unique_lock<boost::shared_mutex> uniqueLock(lock);
	memoLevel[cc] = m;
    } else
	m = memoLevel[cc];
    filterLevelSet(m, alreadyUsed, ans);
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
