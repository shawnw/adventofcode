#include <iostream>
#include <map>
#include <utility>

using point = std::pair<int, int>;

int main(void) {
	point santa1 = {0,0}, santa2 = {0,0}, robot = {0,0};
	std::map<point, int> year1, year2;
	char dir;
	bool santas_move = true;
	
	year1[santa1] = 1;
	year2[santa2] = 2;
	while (std::cin >> dir) {
	  point &coord = santas_move ? santa2 : robot;
		switch (dir) {
			case '^':
				santa1.first += 1;
				coord.first += 1;
				break;
			case '>':
				santa1.second += 1;
				coord.second += 1;
				break;
			case '<':
				santa1.second -= 1;
				coord.second -= 1;
				break;
			case 'V':
			case 'v':
				santa1.first -= 1;
				coord.first -= 1;
				break;
			default:
				std::cerr << "Invalid character '" << dir << "'\n";
				continue;
		}
		year1[santa1] += 1;
		year2[coord] += 1;
		santas_move = !santas_move;
	}
	
	std::cout << "Number of houses:\nYear 1: " << year1.size()
		<< "\nYear 2: " << year2.size() << '\n';
	return 0;
}
