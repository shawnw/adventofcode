// g++ -O2 -std=c++14 -o day25 day25.cc
#include <iostream>
#include <cstdint>

using std::uint64_t;

constexpr uint64_t next_code(uint64_t seed){
		return (seed * 252533) % 33554393;
}

constexpr uint64_t calcrc(int row, int col) {
	uint64_t val = 1;
	int r = 1;
	for (; r < row; r += 1)
		val += r;
	for (int c = 1; c < col; c += 1)
		val += ++r;
	return val;
}

int main(void) {
	int row = 2981;
	int col = 3075;
	uint64_t seed = 20151125;
	
	auto p = calcrc(row, col);
	std::cout << '(' << row << ',' << col << ") is the " << p << "th number.\n";
	
	for (uint64_t n = 1; n < p; n += 1) 
		seed = next_code(seed);
	
	std::cout << "Code is " << seed << '\n';
	
	return 0;
}

