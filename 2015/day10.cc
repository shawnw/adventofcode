#include <iostream>
#include <string>
#include <regex>
#include <cmath>

constexpr double conway = 1.303577260034;

constexpr size_t grow(size_t len) {
	return static_cast<size_t>(std::ceil(conway * len));
}

std::string looksay(const std::string &s) {
	std::string r;	
	r.reserve(grow(s.length()));
	for (size_t i = 0; i < s.length(); ) {
		char c = s[i];
		size_t n = s.find_first_not_of(c, i);
		size_t len = 0;
		if (n == std::string::npos)
			len = s.length() - i;
		else 
			len = n - i;
		r.push_back('0' + len);
		r.push_back(c);
		i += len;
	}
	return r;
}

std::string looksay_re(const std::string &s) {
	static std::regex re{ "(\\d)\\1*" };
	std::string r;
	r.reserve(grow(s.length()));
	std::regex_iterator<decltype(s.begin())> rit(s.begin(), s.end(), re), rend;
	for (;rit != rend; ++rit) {
		r.push_back('0' + rit->length());
		r.push_back(rit->str()[0]);
	}
	return r;
}

int main(int argc, char **argv) {
	if (argc != 3) {
		std::cerr << "Usage: " << argv[0] << " seed repetitions\n";
		return 1;
	}
	
	std::string input{argv[1]};
	int reps = std::stoi(argv[2]);
	
	std::cout << "Starting with: " << input << '\n';
	for (int i = 0; i < reps; i++)
		input = looksay(input);
	std::cout << "After " << reps << " repetitions, string is this long: " << input.length() << '\n';
        for (int i = reps; i < reps + 10; i++)
          input = looksay(input);
        std::cout << "After " << (reps + 10) << " repetitions, string is this long: " << input.length() << '\n';
	return 0;
}
