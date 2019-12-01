// Input on stdin

#include <iostream>
#include <regex>
#include <string>
#include <vector>

struct claim {
  int id;
  int left;
  int top;
  int width;
  int height;
  claim(int i_, int l_, int t_, int w_, int h_)
      : id(i_), left(l_), top(t_), width(w_), height(h_) {}
};

int main() {
  auto fabric = std::vector<std::vector<int>>(1000, std::vector<int>(1000, 0));
  auto claims = std::vector<claim>();

  std::regex claim_re{R"(#(\d+) @ (\d+),(\d+): (\d+)x(\d+))"};

  std::string line;
  while (std::getline(std::cin, line)) {
    std::smatch fields;
    if (std::regex_match(line, fields, claim_re)) {
      int id = std::stoi(fields[1].str());
      int left = std::stoi(fields[2].str());
      int top = std::stoi(fields[3].str());
      int width = std::stoi(fields[4].str());
      int height = std::stoi(fields[5].str());

      claims.emplace_back(id, left, top, width, height);

      for (int x = left; x < left + width; x += 1) {
        for (int y = top; y < top + height; y += 1) {
          fabric[x][y] += 1;
        }
      }
    }
  }

  int overlapping = 0;

  for (const auto &row : fabric) {
    for (int square : row) {
      if (square > 1) {
        overlapping += 1;
      }
    }
  }

  std::cout << "Part 1: " << overlapping << '\n';
  int sole_id = -1;

  for (const auto &c : claims) {
    [&]() {
      for (int x = c.left; x < c.left + c.width; x += 1) {
        for (int y = c.top; y < c.top + c.height; y += 1) {
          if (fabric[x][y] != 1) {
            return;
          }
        }
      }
      sole_id = c.id;
    }();
    if (sole_id >= 0) {
      break;
    }
  }
  std::cout << "Part 2: " << sole_id << '\n';

  return 0;
}
