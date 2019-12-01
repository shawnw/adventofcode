#include <iostream>
#include <string>
#include <tuple>
#include "json.hpp"

using nlohmann::json;

std::tuple<int, int> sum_json(json j) {
	if (j.is_number()) {
	  int n = j.get<int>();
		return std::make_tuple(n, n);
	} else if (j.is_string()) {
		return std::make_tuple(0, 0);
	} else if (j.is_array()) {
		int sum1 = 0, sum2 = 0;
		for (auto element : j) {
		  auto t = sum_json(element);
		  sum1 += std::get<0>(t);
		  sum2 += std::get<1>(t);
		}
		return std::make_tuple(sum1, sum2);
	} else if (j.is_object()) {
		int sum1 = 0, sum2 = 0;
		bool red = false;
		for (auto it = j.begin(); it != j.end(); ++it) {
		  if (it.value() == "red") {
		    red = true;
		    sum2 = 0;
		  }
		  auto t = sum_json(it.value());
		  sum1 += std::get<0>(t);
		  if (!red)
		    sum2 += std::get<1>(t);
		}
		return std::make_tuple(sum1, sum2);
	} else {
		// Unhandled type
		return std::make_tuple(0, 0);
	}
}

int main(void) {
	std::string line;
	int sum1 = 0, sum2 = 0;	
	json j;
	
	std::cin >> j;
	for (auto element : j) {
		auto t = sum_json(element);
		sum1 += std::get<0>(t);
		sum2 += std::get<1>(t);
	}
	std::cout << "Total sum, day 1: " << sum1 << '\n';
	std::cout << "Total sum, day 2: " << sum2 << '\n';
	
	return 0;
}
