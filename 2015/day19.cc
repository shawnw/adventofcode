/* For part 2, this one returns the first deriviation it finds for reducing the
* test string to e. Since it tries to reduce longest matches first, I think this
* will always be correct. An exhaustive search to confirm it takes more time
* than I have patience.
* Anyways, the answer it gave was right.
*/

// g++ -O -std=c++14 -o day19 day19.cc
#include <iostream>
#include <sstream>
#include <string>
#include <regex>
#include <map>
#include <numeric>
#include <functional>
#include <set>
#include <iterator>
#include <cctype>
#include <climits>

using smap = std::map<std::string, std::string>;
using svec = std::vector<std::string>;

std::string atoms_to_str(const svec &atoms) {
		std::ostringstream s;
		std::copy(atoms.begin(), atoms.end(), std::ostream_iterator<std::string>(s));
		return s.str();
}

int reduce(const std::string &molecule, smap &trans, const svec &tokens, int steps = 0) {
  static int min_steps = INT_MAX;

	auto i = trans.find(molecule);
	if (i != trans.end() && i->second == "e") {
		return steps + 1;
	}

	if (steps >= min_steps) {
		return -1;
	}
	
	for (auto &t : tokens) {
		size_t p, from = 0;
		while ((p = molecule.find(t, from)) != std::string::npos) {
				std::string nm = molecule;
				nm.replace(p, t.length(), trans[t]);
				int s = reduce(nm, trans, tokens, steps + 1);
				if (s > 0) {
					return s;
					min_steps = std::min(min_steps, s);
				}
				from = p + 1;
			}
	}
	if (min_steps == INT_MAX)
		return -1;
	else
		return min_steps;
}

int main(void) {
	std::multimap<std::string, std::string> transformations;
	std::string line, molecule;
	std::regex trans_re{ R"((\w+) => (\w+))" };
	std::regex atom { R"([A-Z][a-z]?)" };
	
	
	while (std::getline(std::cin, line)) {
		std::smatch fields;
		if (std::regex_match(line, fields, trans_re)) {
				transformations.insert(std::make_pair(fields[1], fields[2]));
		} else if (!line.empty()) {
			molecule = std::move(line);
			break;
		}
	}
	
	std::regex_iterator<std::string::iterator> ri(molecule.begin(),
		molecule.end(), atom), rend;
	
	svec atoms;
	while (ri != rend) 
		atoms.push_back((ri++)->str());
	
	std::set<std::string> molecules;
	
	for (auto &a : atoms) {
			std::string orig = a;
			auto r = transformations.equal_range(a);
			for (auto ti = r.first; ti != r.second; ++ti) {
				a = ti->second;
				molecules.insert(atoms_to_str(atoms));
			}
			a = orig;
	}
	
	std::cout << molecules.size() << " distinct molecules.\n";
	
	smap rtrans;
	svec rhs;
	for (auto &t : transformations) {
		if (t.first != "e")
			rhs.push_back(t.second);
		rtrans.insert(std::make_pair(t.second, t.first));
	}
	std::sort(rhs.begin(), rhs.end(),
			[](auto &a, auto &b) { return a.length() > b.length(); });	
	
	int steps = reduce(molecule, rtrans, rhs);
	
	std::cout << steps << " steps to derive the molecule.\n";
	
	return 0;
}