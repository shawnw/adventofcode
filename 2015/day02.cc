#include <iostream>
#include <algorithm>
#include <array>

// 3 element sorting network
void sort3(std::array<int, 3> &a) {
  if (a[0] > a[1])
    std::swap(a[0], a[1]);
  if (a[0] > a[2])
    std::swap(a[0], a[2]);
  if (a[1] > a[2])
    std::swap(a[1], a[2]);
}

int paper(int l, int w, int h) {
	int surface_area = 2*l*w + 2*w*h + 2*h*l;
	
	std::array<int,3> sides = {l, w, h};
	sort3(sides);
	
	return surface_area + sides[0]*sides[1];
}

int ribbon(int l, int w, int h) {
	std::array<int, 3> sides = {l, w, h};
	sort3(sides);
	
	int perimeter = sides[0] + sides[0] + sides[1] + sides[1];
	
	return perimeter + l*w*h;
}

int main(void) {
	int l, w, h;
	char x1, x2;
	int total_paper = 0;
	int total_ribbon = 0;
	
	while (std::cin >> l >> x1 >> w >> x2 >> h) {		
		total_paper += paper(l, w, h);
		total_ribbon += ribbon(l, w, h);
	}
	
	std::cout << "Paper: " << total_paper
   			  << "\nRibbon: " << total_ribbon << '\n';
	
	return 0;
}
