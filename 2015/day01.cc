#include <iostream>

int main(void) {
	int floor = 0;
	char p;
	int position = 0;
	bool basement = false;
	
	while (std::cin >> p) {
		position += 1;
		if (p == '(')
			floor += 1;
		else if (p == ')')
			floor -= 1;
		if (!basement && floor == -1) {
			basement = true;
			std::cout << "In the basement at position " << position << '\n';
		}
	}
	std::cout << "Floor " << floor << '\n';
	return 0;
}