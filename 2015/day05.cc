#include <iostream>
#include <string>
#include <regex>
#include <algorithm>

bool isnice_part1(const std::string &word) {
	static std::regex duplicate_letter { "(.)\\1" };
	static std::regex three_vowels { "[aeiou].*[aeiou].*[aeiou]" };
	static std::regex naughty_sequences {"ab|cd|pq|xy"};
	
	if (!std::regex_search(word, duplicate_letter))
		return false;
	if (!std::regex_search(word, three_vowels))
		return false;
	return !std::regex_search(word, naughty_sequences);
}

bool isnice_part1_nore(const std::string &word) {
	static const std::string vowels{"aeiou"};
	std::string::size_type n = 0;
	
	for (int i = 0; i < 3; i += 1) {
		n = word.find_first_of(vowels, n);
		if (n == std::string::npos)
			return false;
		n += 1;
	}
	
	bool found = false;
	for (n = 0; n < word.size() - 1; n += 1) {
		if (word[n] == word[n + 1]) {
			found = true;
			break;
		}
	}
	if (!found)
		return false;
	
	static const std::string naughty[] = { "ab", "cd", "pq", "xy" };
	return std::all_of(std::begin(naughty), std::end(naughty),
		[&word](const auto &bad) { return word.find(bad) == std::string::npos; });
}

bool isnice_part2(const std::string &word) {
	static std::regex repeating_pair { "(..).*\\1" };
	static std::regex duplicate_single { "(.).\\1" };
	
	if (!std::regex_search(word, repeating_pair)) 
		return false;
	return std::regex_search(word, duplicate_single);
}

bool isnice_part2_nore(const std::string &word) {
	char pair[3] = { 0, 0, 0 };
	std::string::size_type n;
	
	bool found = false;
	for (n = 0; n < word.size() - 3; n += 1) {
			pair[0] = word[n];
			pair[1] = word[n + 1];
			if (word.find(pair, n + 2, 2) != std::string::npos) {
				found = true;
				break;
			}
	}
	if (!found)
		return false;
	
	for (n = 0; n < word.size() - 2; n += 1) {
		if (word[n] == word[n + 2])
			return true;
	}
	
	return false;
}

int main(void) {
	std::string word;
	int nice_part1 = 0;
	int nice_part2 = 0;
	
	while (std::getline(std::cin, word)) {
		if (isnice_part1(word))
			nice_part1 += 1;
		if (isnice_part2(word)) 
			nice_part2 += 1;
	}
	std::cout << "Part 1 nice count: " << nice_part1 << '\n';
	std::cout << "Part 2 nice count: " << nice_part2 << '\n';
	return 0;
}