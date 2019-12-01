#include <iostream>
#include <string>

int logical_length(const std::string &s) {
	int len = 0;
	
	// Start at 1 to skip leading double quote. Same for ending position.
	for (std::string::size_type i = 1; i < s.length() - 1; ) {
		if (s[i] == '\\') {
			i += 1;
			if (s[i] == 'x') {
				i += 3;
				len += 1;
			} else if (s[i] == '\\' || s[i] == '"') {
				i += 1;
				len += 1;
			}
		} else {
			i += 1;
			len += 1;
		}
	}
	return len;
}

int encoded_length(const std::string &s) {
	int len = 2; // For the double quotes
	
	for (auto c : s) {
		if (c == '"' || c == '\\') 
			len += 2;
		else
			len += 1;
	}
	return len;
}


int main(void) {
	std::string line;
	int total_chars = 0;
	int logical_chars = 0;
	int encoded_chars = 0;
	
	while (std::getline(std::cin, line)) {
		total_chars += line.length();
		logical_chars += logical_length(line);
		encoded_chars += encoded_length(line);
	}
	std::cout << "Part 1 length: " << (total_chars - logical_chars) << '\n';
	std::cout << "Part 2 length: " << (encoded_chars - total_chars) << '\n';
	return 0;
}