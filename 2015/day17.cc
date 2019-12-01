#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>

#include "combos.hpp"

int main(int argc, char **argv) {
	int n;
	std::vector<int> jugs;
	bool part2 = argc == 2;
			
	while (std::cin >> n) {
		jugs.push_back(n);
	}
	
	int combos = 0;
	combinations<int> generator(jugs);
	
	for (n = 2; n < jugs.size(); n++) {
		bool found = false;
		auto c = generator.combos(n);
		for (auto &i : c) 
			if (std::accumulate(i.begin(), i.end(), 0) == 150) {
				combos += 1;
				found = true;
			}
		if (part2 && combos > 0)
			break;
		if (combos > 0 && !found)
			break;
	}
	
	std::cout << "Total: " << combos << '\n';
	return 0;
}