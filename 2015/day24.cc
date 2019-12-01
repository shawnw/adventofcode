/* Day 24. Partition a set of numbers into N groups with equal sums, then
*  from the sets of N partitions, find the partitions with the fewest members,
*  and return the product of that partition.
*
* Original version (partition) is dumb brute force that only finds partitions of 
* the given goal sum, and assumes that the remaining numbers can be partitioned
* equally too. This is the case for the problem's data set, but not universal.
* This caused some debate on the discussion threads about this problem.
*
* So... the second version (partition_smart) is more strict about ensuring that
* N valid partitions actually exist before returning true. It's slower because
* of that. The naive smart approach, though, is much faster than the naive
* partition because it generates a lot less possible combinations to consider,
* however briefly.
*/

#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>
#include <cstdint>
#include <string>
#include <functional>
#include <chrono>

// Depends on the SSE4.2 popcnt instruction
#include <nmmintrin.h>
#include "combos.hpp"

using ivec = std::vector<int>;
using std::uint64_t;

int weight(const ivec &presents) {
	return std::accumulate(presents.begin(), presents.end(), 0);
}

uint64_t qe(const ivec &presents) {
	return std::accumulate(presents.begin(), presents.end(),
			uint64_t(1), std::multiplies<uint64_t>());
}

template<class T, class binary_op>
T foldit(ivec &v, unsigned c, T total, binary_op op) {
	for (unsigned n = 0; n < v.size(); n += 1)
		if (c & (1 << n))
			total = op(total, v[n]);
	return total;
}

int weight(ivec &presents, unsigned c) {
	return foldit<int>(presents, c, 0, std::plus<int>());
}

uint64_t qe(ivec &presents, unsigned c) {
	return foldit<uint64_t>(presents, c, 1, std::multiplies<uint64_t>());
}

int size(unsigned c) {
	return _mm_popcnt_u32(c);
}

bool partition(ivec &presents, uint64_t &qe_val, int ways) {
	int minbag = INT_MAX;
	int goal = weight(presents) / ways;
	int maxbag = presents.size() / ways;
	
	for (unsigned c = 1; c < (1U << presents.size()); c += 1) {
		int s = size(c);
		if (s > maxbag)
			continue;
		if (weight(presents, c) != goal)
			continue;
		if (s < minbag) {
			minbag = s;
			qe_val = qe(presents, c);
		} else if (s == minbag) 
			qe_val = std::min(qe_val, qe(presents, c));
	}
	return minbag < INT_MAX;
}

bool partition_smart(ivec &presents, uint64_t &qe_val, int ways) {
	int goal = weight(presents) / ways;
	int maxbag = presents.size() / ways;
	bool found = false;

	if (ways == 1) {
		qe_val = qe(presents);
		return true;
	}
	
	combinations<int> generator(presents);
	for (int i = 1; i <= maxbag; i += 1) {
		auto combos = generator.combos(i);
		for (auto &c : combos) {
			int w = weight(c);
			if (w == goal) {
				ivec leftover;
				uint64_t qe2 = 0;
				std::sort(c.begin(), c.end());
				std::set_difference(presents.begin(), presents.end(),
						c.begin(), c.end(), std::back_inserter(leftover));	
				if (ways == 2) {
					if (weight(leftover) == goal) {
						if (!found) {
								found = true;
								qe_val = std::min(qe(c), qe(leftover));
						} else {
								qe2 = std::min(qe(c), qe(leftover));
								qe_val = std::min(qe_val, qe2);
						}
					}
				} else if (partition_smart(leftover, qe2, ways - 1)) {
					if (!found) {
						found = true;
						qe_val = std::min(qe(c), qe2);
					} else {
 						qe2 = std::min(qe2, qe(c));
						qe_val = std::min(qe_val, qe2);
					}
				}
			}
		}
		if (found)
			return true;
	}
	return false;
}

int main(int argc, char **argv) {
	ivec presents;
	int n;
	uint64_t qe1 = 0, qe2 = 0;
	if (argc != 2) {
		std::cerr << "Usage: " << argv[0] << " NPARTITIONS\n";
		return 1;
	}
	while (std::cin >> n)
		presents.push_back(n);
	
	int ways = std::stoi(argv[1]);
	
	using namespace std::chrono;
	steady_clock::time_point t1 = steady_clock::now();
	partition(presents, qe1, ways);
	steady_clock::time_point t2 = steady_clock::now();
	std::cout << "partition1: " << duration_cast<milliseconds>(t2 - t1).count() << '\n';
	
	t1 = steady_clock::now();
	if (!partition_smart(presents, qe2, ways))
		std::cout << "partition2 failed!\n";
	t2 = steady_clock::now();
	std::cout << "partition2: " << duration_cast<milliseconds>(t2 - t1).count() << '\n';
	
	std::cout << "QE1 " << qe1 << '\n';
	std::cout << "QE2 " << qe2 << '\n';
	
	return 0;
}
			
	
	
	
	
	
	
	