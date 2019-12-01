#include <iostream>
#include <string>
#include <regex>
#include <vector>
#include <utility>
#include <map>
#include <algorithm>
#include <climits>
#include <iterator>

using conn = std::map<std::pair<std::string, std::string>, int>;

struct no_such_route {};

int route_length(const conn &connections, const std::vector<std::string> &cities) {
	int distance = 0;
	auto current = cities.front();
	for (auto city = cities.begin() + 1; city != cities.end(); ++city) {
		const auto route = std::make_pair(current, *city);
		const auto r = connections.find(route);
		if (r == connections.end()) 
			throw no_such_route();
		distance += r->second;
		current = *city;
	}
	return distance;
}

int main(void) {
	std::string line;
	std::regex edge_re { "(\\w+) to (\\w+) = (\\d+)" };
	std::vector<std::string> cities;
	conn connections;
	int min_distance = INT_MAX;
	int max_distance = 0;
	
	while (std::getline(std::cin, line)) {
		std::smatch fields;
		if (std::regex_match(line, fields, edge_re)) {
		  cities.push_back(fields[1]);
		  cities.push_back(fields[2]);
			int d = stoi(fields[3]);
			connections.emplace(std::make_pair(fields[1], fields[2]), d);
			connections.emplace(std::make_pair(fields[2], fields[1]), d);
		} else {
			std::cerr << "Unknown line '" << line << "'\n";
		}	
	}

	std::sort(cities.begin(), cities.end());
	cities.erase(std::unique(cities.begin(), cities.end()), cities.end());

	do {
		try {
			int d = route_length(connections, cities);
			min_distance = std::min(min_distance, d);
			max_distance = std::max(max_distance, d);
			std::copy(cities.begin(), cities.end(), std::ostream_iterator<std::string>(std::cout, " -> "));
			std::cout << " = " << d << '\n';
		} catch (no_such_route e) {
		}
	} while (std::next_permutation(cities.begin(), cities.end()));			
	
	std::cout << "Minimum distance: " << min_distance << '\n';
	std::cout << "Maximum distance: " << max_distance << '\n';
	
	return 0;
}
