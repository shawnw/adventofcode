// Compile with g++ -O -std=c++14 -o day15 day15.cc
#include <iostream>
#include <string>
#include <regex>
#include <vector>
#include <valarray>
#include <numeric>	
	
using ingquals = std::vector<std::valarray<int>>;

int mmax(int a, int b) { return std::max(a,b); }
	
int score(ingquals ivec, const std::vector<int> &qvec) {
	std::valarray<int> sum{0,0,0,0};	
	std::transform(ivec.begin(), ivec.end(), qvec.begin(), ivec.begin(),
		[](auto &i, int a){ return i * a; });
	sum = std::accumulate(ivec.begin(), ivec.end(), sum);
	sum = sum.apply([](int i){ return std::max(i, 0); });
	return std::accumulate(std::begin(sum), std::end(sum), 1, std::multiplies<int>());
}
	
int tottsps(const std::vector<int> &q) {
	return std::accumulate(q.begin(), q.end(), 0);
}

void populate_quantities(std::vector<std::vector<int>> &q, int count, std::vector<int> scratch = {}) {
	if (count == 1) {
		int s = 100 - tottsps(scratch);
		scratch.push_back(s);
		q.emplace_back(scratch);
	} else {
		scratch.push_back(0);
		for (int n = 0; n <= 100; n++) {
			scratch.back() = n;
			if (tottsps(scratch) > 100)
				return;
			populate_quantities(q, count - 1, scratch);
		}
	}
}
	
int main(int argc, char **argv) {
	std::string line;
	std::regex ingred_re{ R"(\w+: capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d), calories (-?\d+))" };
	ingquals ingreds;
	std::vector<int> calories;
	int calorie_goal = -1;
	
	if (argc == 2) {
		std::cout << "Goal of " << argv[1] << " calories.\n";
		calorie_goal = std::stoi(argv[1]);
	}
	
	while (std::getline(std::cin, line)) {
		std::smatch fields;
		if (std::regex_match(line, fields, ingred_re)) {
			std::valarray<int> q{std::stoi(fields[1]), std::stoi(fields[2]), std::stoi(fields[3]), std::stoi(fields[4])};
			ingreds.push_back(std::move(q));
			calories.push_back(std::stoi(fields[5]));
		} else {
			std::cout << "Unknown line '" << line << "'\n";
		}
	}

	std::vector<std::vector<int>> quantities;
	populate_quantities(quantities, ingreds.size());
	
	int max_score = 0;
	for (auto &q : quantities) {
		if (calorie_goal != -1 &&
			std::inner_product(calories.begin(), calories.end(), q.begin(), 0) != calorie_goal)
			continue;
		max_score = std::max(score(ingreds, q), max_score);
	}
	
	std::cout << "Score: " << max_score << '\n';
	
	return 0;
}