#include <iostream>
#include <string>
#include <regex>
#include <vector>
#include <utility>
#include <unordered_map>
#include <algorithm>
#include <functional>

using happyscore = std::pair<const std::string, int>;
using happylist = std::vector<happyscore>;
using happymap = std::unordered_map<std::string, happylist>;

bool cmp_happy(const happyscore &h, const std::string &s) {
	return h.first == s;
}

int happiness_for(happymap &h, const std::string &person1, const std::string &person2) {
	const happylist &h1 = h[person1];
	const happylist &h2 = h[person2];
	using namespace std::placeholders;
	auto m1 = std::find_if(h1.begin(), h1.end(), std::bind(cmp_happy, _1, person2));
	auto m2 = std::find_if(h2.begin(), h2.end(), std::bind(cmp_happy, _1, person1));
	return m1->second + m2->second;
}

int main(int argc, char **argv) {
	std::string line;
	std::regex person_re{R"((\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.)" };
	std::vector<std::string> people;
	happymap h;
	
	bool yourself = argc == 2;
	
	while (std::getline(std::cin, line)) {
		std::smatch fields;
		if (std::regex_match(line, fields, person_re)) {
			people.push_back(fields[1]);
			people.push_back(fields[4]);
			int happiness = std::stoi(fields[3]);
			if (fields[2] == "lose")
				happiness = -happiness;
			h[fields[1]].emplace_back(fields[4], happiness);
		} else {
			std::cerr << "Unknown line: '" << line << "'\n";
		}	
	}

	std::sort(people.begin(), people.end());
	people.erase(std::unique(people.begin(), people.end()), people.end());

	// For part 2.
	if (yourself) {
		for (const auto &person : people) {
			h["yourself"].emplace_back(person, 0);
			h[person].emplace_back("yourself", 0);
		}
		auto where = std::upper_bound(people.begin(), people.end(), "yourself");
		people.insert(where, "yourself");
	}

	int max_happiness = 0;
	do {
		int happiness = happiness_for(h, people.front(), people.back());
		for (size_t i = 0; i < people.size() - 1; i++)
			happiness += happiness_for(h, people[i], people[i+1]);
		max_happiness = std::max(max_happiness, happiness);
	} while (std::next_permutation(people.begin(), people.end()));
	std::cout << "Maximum happiness: " << max_happiness << '\n';
	return 0;
}