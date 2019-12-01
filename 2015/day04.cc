// Takes one argument, the length of the leading 0s needed in the md5 hash.

// For best results, compile like so:
// g++ -o day04 -std=c++11 -O3 -march=native -flto day04.cc md5.cc

// Also supports OpenMP, but it actually runs faster without. Too much time
// spent in synchronization?

#include <iostream>
#include <string>
#include <cstdlib>
#include "md5.h"

class compare_hash {
	private:
		std::string prefix;
	public:
		explicit compare_hash(int n) : prefix(n, '0') {}
		bool matches(const std::string &hash) const {
			return hash.compare(0, prefix.length(), prefix) == 0;
		}
		bool matches(const char *hash) const {
			return prefix.compare(0, prefix.length(), hash, prefix.length()) == 0;
		}
};

std::string make_secret_key(const std::string &seed, int n) {
	return seed + std::to_string(n);
}

int main(int argc, char **argv) {

	if (argc != 2) {
		std::cerr << "Missing prefix length argument.\n";
		return 1;
	}
	
	compare_hash test{std::stoi(argv[1])};
	
	//std::getline(std::cin, seed);
	std::string seed{"iwrupvqb"};
	
	#pragma omp parallel for ordered
	for (int magic = 1; magic < INT_MAX; magic += 1) {
		const auto key = make_secret_key(seed, magic);
		const auto hashed_key = md5_cstr(key);
		#pragma omp ordered
		if (test.matches(hashed_key)) {
			std::cout << magic << '\n';
			//break;
			std::exit(0);
		} 
	}

	return 0;
}
	