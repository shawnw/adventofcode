#include <iostream>
#include <string>
#include <unordered_map>
#include <regex>

using things = std::unordered_map<std::string, int>;

bool agrees(bool part2, const things &sue, const std::pair<std::string, int> &item) {
	auto si = sue.find(item.first);
	if (si == sue.end())
		return true;
	else if (part2 && (item.first == "cats" || item.first == "trees"))
		return si->second > item.second;
	else if (part2 && (item.first == "pomeranians" || item.first == "goldfish"))
		return si->second < item.second;
	else 
		return si->second == item.second;
}

bool possible_sue(bool part2, const things &traces, const things &sue) {
	for (const auto &item: traces) {
		if (!agrees(part2, sue, item))
			return false;
	}
	return true;
}

int main(int argc, char **argv) {
	things traces{{"children", 3}, {"cats", 7}, {"samoyeds", 2},
		{"pomeranians", 3}, {"akitas", 0}, {"vizslas", 0},
		{"goldfish", 5}, {"trees", 3}, {"cars", 2}, {"perfumes", 1}};
	std::string line;
	std::regex pair_re{ R"((\w+): (\d+))" };
	int nsue = 1;
	bool part2 = argc == 2;
	
	while (std::getline(std::cin, line)) {
		int fields[] = {1,2};
		std::regex_token_iterator<std::string::iterator> ri{line.begin(), line.end(), pair_re, fields}, re{};
		things sue;
		while (ri != re) {
			std::string thing = *ri++;
			int count = std::stoi(*ri++);
			sue.emplace(thing, count);
		}
		if (possible_sue(part2, traces, sue))
			std::cout << "Sue " << nsue << " is a match.\n";
		nsue += 1;
	}	
	return 0;
}