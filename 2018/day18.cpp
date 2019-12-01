#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

using grid = std::vector<std::string>;
using cache = std::unordered_map<std::string, unsigned>;

std::string grid_to_string(const grid &map) {
  std::string out;
  out.reserve(map.size() * map[0].size());
  for (const auto &row : map) {
    out.append(row);
  }
  return out;
}

int count_adjacent(const grid &map, unsigned y, unsigned x, char what) {
  int count = 0;
  if (y > 0) {
    count += map[y - 1][x] == what;
    if (x > 0) {
      count += map[y - 1][x - 1] == what;
    }
    if (x < map[y - 1].size() - 1) {
      count += map[y - 1][x + 1] == what;
    }
  }
  if (y < map.size() - 1) {
    count += map[y + 1][x] == what;
    if (x > 0) {
      count += map[y + 1][x - 1] == what;
    }
    if (x < map[y + 1].size() - 1) {
      count += map[y + 1][x + 1] == what;
    }
  }
  if (x > 0) {
    count += map[y][x - 1] == what;
  }
  if (x < map[y].size() - 1) {
    count += map[y][x + 1] == what;
  }
  return count;
}

grid step(const grid &map) {
  grid newmap = map;

  for (unsigned y = 0; y < map.size(); y += 1) {
    for (unsigned x = 0; x < map[y].size(); x += 1) {
      if (map[y][x] == '.' && count_adjacent(map, y, x, '|') >= 3) {
        newmap[y][x] = '|';
      } else if (map[y][x] == '|' && count_adjacent(map, y, x, '#') >= 3) {
        newmap[y][x] = '#';
      } else if (map[y][x] == '#' && !(count_adjacent(map, y, x, '#') >= 1 &&
                                       count_adjacent(map, y, x, '|') >= 1)) {
        newmap[y][x] = '.';
      }
    }
  }

  return newmap;
}

int count_resources(const grid &map) {
  int trees = 0;
  int lumberyards = 0;

  for (const auto &row : map) {
    for (char c : row) {
      if (c == '|') {
        trees += 1;
      } else if (c == '#') {
        lumberyards += 1;
      }
    }
  }

  return trees * lumberyards;
}

int main() {
  grid map;

  for (std::string line; std::getline(std::cin, line);) {
    map.push_back(line);
  }

  cache steps;
  bool in_cache = false;
  unsigned nminutes = 1000000000U;
  for (unsigned minute = 1; minute <= nminutes; minute += 1) {
    map = step(map);
    if (!in_cache) {
      if (minute == 10) {
        std::cout << "Part 1: " << count_resources(map) << '\n';
      }
      auto i = steps.emplace(grid_to_string(map), minute);
      if (i.second == false) {
        auto diff = minute - i.first->second;
        std::cout << "Cycle: " << diff << " at minute " << i.first->second
                  << '\n';
        nminutes -= i.first->second;
        nminutes %= diff;
        if (nminutes == 0) {
          break;
        }
        minute = 0;
        in_cache = true;
      }
    }
  }
  std::cout << "Part 2: " << count_resources(map) << '\n';

  return 0;
}
