#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using world = std::vector<std::vector<bool>>;

int count_neighbors(const world &grid, int x, int y) {
		int n = 0;
		if (x > 0) {
			n += grid[x - 1][y];
			if (y > 0)
				n += grid[x - 1][y - 1];
			if (y < grid[x-1].size() - 1)
				n += grid[x - 1][y + 1];
		}
		if (x < grid.size() - 1) {
			n += grid[x + 1][y];
			if (y > 0)
				n += grid[x + 1][y - 1];
			if (y < grid[x + 1].size() - 1)
				n += grid[x + 1][y + 1];
		}
		if (y > 0)
			n += grid[x][y - 1];
		if (y < grid[x].size() - 1)
			n += grid[x][y + 1];
		
		return n;
}

world life(const world &grid, bool fixed_corners) {
	world newgrid = grid;
	for (int x = 0; x < grid.size(); x += 1) {
		for (int y = 0; y < grid[x].size(); y += 1) {
			if (fixed_corners) {
					if (x == 0 && (y == 0 || y == grid[0].size() - 1))
						continue;
					if (x == grid.size() - 1 && (y == 0 || y == grid[x].size() - 1))
						continue;
			}
			
			int neighbors = count_neighbors(grid, x, y);
			if (grid[x][y] && !(neighbors == 2 || neighbors == 3))
				newgrid[x][y] = false;
			if (!grid[x][y] && neighbors == 3)
				newgrid[x][y] = true;
		}
	}
	return newgrid;
}

int main(int argc, char **argv) {
	if (argc == 1) {
		std::cerr << "Usage: " << argv[0] << " steps [corners] < world.data\n";
		return 1;
	}

	int steps = std::stoi(argv[1]);
	world grid;
	std::string line;
	bool part2 = argc == 3;
	
	while (std::getline(std::cin, line)) {
			std::vector<bool> row;
			for (auto c : line) 
				row.push_back(c == '#');
			grid.push_back(std::move(row));
	}
	
	for (int n = 0; n < steps; n += 1) 
		grid = life(grid, part2);
	
	int lights = 0;
	for (auto &row : grid) 
		lights += std::count(row.begin(), row.end(), true);
	std::cout << lights << '\n';
	
	return 0;
}