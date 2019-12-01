#include <algorithm>
#include <cstring>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

using namespace std::literals::string_literals;

enum class next_turn { LEFT, STRAIGHT, RIGHT };

enum class direction { UP, DOWN, LEFT, RIGHT };

using worldmap = std::vector<std::string>;

bool has_cart(const worldmap &map, unsigned x, unsigned y) {
  return strchr("<>^v", map[y][x]) != NULL;
}

char cart_char(direction d) {
  switch (d) {
  case direction::UP:
    return '^';
  case direction::DOWN:
    return 'v';
  case direction::LEFT:
    return '<';
  case direction::RIGHT:
    return '>';
  }
  return '!';
}

char line_char(direction d) {
  switch (d) {
  case direction::UP:
  case direction::DOWN:
    return '|';
  case direction::LEFT:
  case direction::RIGHT:
    return '-';
  }
  return '!';
}

struct cart {
  unsigned x;
  unsigned y;
  next_turn turn;
  direction travel;
  char over;
  bool moved;
  bool crashed;
  cart(unsigned x_, unsigned y_, direction t_)
      : x(x_), y(y_), turn(next_turn::LEFT), travel(t_), over(line_char(t_)),
        moved(false), crashed(false) {}
  bool tick(worldmap &, std::vector<cart> &);
};

bool first_crash = true;

using cartlist = std::vector<cart>;

bool cart::tick(worldmap &map, std::vector<cart> &carts) {
  unsigned new_x = 0, new_y = 0;

  if (moved || crashed) {
    return false;
  }

  moved = true;

  switch (travel) {
  case direction::LEFT:
    new_x = x - 1;
    new_y = y;
    break;
  case direction::RIGHT:
    new_x = x + 1;
    new_y = y;
    break;
  case direction::UP:
    new_x = x;
    new_y = y - 1;
    break;
  case direction::DOWN:
    new_x = x;
    new_y = y + 1;
    break;
  }

  char new_over = map.at(new_y).at(new_x);

  switch (new_over) {
  case '>':
  case '<':
  case '^':
  case 'v':
    crashed = true;
    map[y][x] = over;
    for (cart &c : carts) {
      if (c.x == new_x && c.y == new_y && !c.crashed) {
        c.crashed = true;
        map[new_y][new_x] = c.over;
        break;
      }
    }
    if (first_crash) {
      first_crash = false;
      std::cout << "Part 1: " << new_x << ',' << new_y << '\n';
    }
    return true;
  case '-':
  case '|':
    break;
  case '\\':
    switch (travel) {
    case direction::LEFT:
      travel = direction::UP;
      break;
    case direction::RIGHT:
      travel = direction::DOWN;
      break;
    case direction::UP:
      travel = direction::LEFT;
      break;
    case direction::DOWN:
      travel = direction::RIGHT;
      break;
    }
    break;
  case '/':
    switch (travel) {
    case direction::LEFT:
      travel = direction::DOWN;
      break;
    case direction::RIGHT:
      travel = direction::UP;
      break;
    case direction::UP:
      travel = direction::RIGHT;
      break;
    case direction::DOWN:
      travel = direction::LEFT;
      break;
    }
    break;
  case '+':
    switch (turn) {
    case next_turn::LEFT:
      switch (travel) {
      case direction::UP:
        travel = direction::LEFT;
        break;
      case direction::RIGHT:
        travel = direction::UP;
        break;
      case direction::DOWN:
        travel = direction::RIGHT;
        break;
      case direction::LEFT:
        travel = direction::DOWN;
        break;
      }
      turn = next_turn::STRAIGHT;
      break;
    case next_turn::STRAIGHT:
      turn = next_turn::RIGHT;
      break;
    case next_turn::RIGHT:
      switch (travel) {
      case direction::UP:
        travel = direction::RIGHT;
        break;
      case direction::RIGHT:
        travel = direction::DOWN;
        break;
      case direction::DOWN:
        travel = direction::LEFT;
        break;
      case direction::LEFT:
        travel = direction::UP;
        break;
      }
      turn = next_turn::LEFT;
      break;
    }
    break;
  default:
    throw std::out_of_range{"bad map square"};
  }

  map[y][x] = over;
  map[new_y][new_x] = cart_char(travel);
  over = new_over;
  x = new_x;
  y = new_y;
  return false;
}

cartlist find_carts(const worldmap &map) {
  cartlist carts;
  for (auto y = 0U; y < map.size(); y += 1) {
    for (auto x = 0U; x < map[y].size(); x += 1) {
      switch (map[y][x]) {
      case '<':
        carts.emplace_back(x, y, direction::LEFT);
        break;
      case '>':
        carts.emplace_back(x, y, direction::RIGHT);
        break;
      case '^':
        carts.emplace_back(x, y, direction::UP);
        break;
      case 'v':
        carts.emplace_back(x, y, direction::DOWN);
        break;
      default:
        break;
      }
    }
  }
  return carts;
}

void print_map(const worldmap &map) {
  for (const auto &row : map) {
    std::cout << row << '\n';
  }
  std::cout << '\n';
}

int main() {
  worldmap map;
  for (std::string line; std::getline(std::cin, line);) {
    map.push_back(line);
  }

  auto carts = find_carts(map);
  try {
    while (true) {
      for (cart &c : carts) {
        c.moved = false;
      }
      // print_map(map);
      int crashed = 0;
      for (auto y = 0U; y < map.size(); y += 1) {
        for (auto x = 0U; x < map[y].size(); x += 1) {
          for (cart &c : carts) {
            if (c.x == x && c.y == y) {
              crashed += c.tick(map, carts);
            }
          }
        }
      }
      if (crashed) {
        carts.erase(std::remove_if(carts.begin(), carts.end(),
                                   [](const cart &c) { return c.crashed; }),
                    carts.end());
      }
      if (carts.size() == 1) {
        std::cout << "Part 2: " << carts[0].x << ',' << carts[0].y << '\n';
        break;
      }
    }
  } catch (std::out_of_range &) {
    std::cout << "Something went wrong!\n";
    return 1;
  }
  return 0;
}
