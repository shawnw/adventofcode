#include <algorithm>
#include <complex>
#include <fstream>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <regex>
#include <string>
#include <tuple>
#include <vector>

struct point {
  std::complex<float> pos;
  std::complex<float> vel;
  point(float x, float y, float dx, float dy) : pos{x, y}, vel{dx, dy} {}
};

using skyvec = std::vector<point>;

void step(skyvec &sky) {
  for (auto &p : sky) {
    p.pos += p.vel;
  }
}

using coord = std::tuple<int, int>;
using skymap = std::map<coord, bool>;

skymap make_skymap(const skyvec &sky) {
  skymap smap;

  for (const auto &p : sky) {
    coord xy(p.pos.real(), p.pos.imag());
    smap.emplace(xy, false);
  }

  return smap;
}

void print_skymap(const skymap &smap) {
  auto [minx_i, maxx_i] = std::minmax_element(
      smap.begin(), smap.end(), [](const auto &a, const auto &b) {
        return std::get<0>(a.first) < std::get<0>(b.first);
      });
  auto [miny_i, maxy_i] = std::minmax_element(
      smap.begin(), smap.end(), [](const auto &a, const auto &b) {
        return std::get<1>(a.first) < std::get<1>(b.first);
      });

  int maxx = std::get<0>(maxx_i->first);
  int maxy = std::get<1>(maxy_i->first);

  for (int y = std::get<1>(miny_i->first); y <= maxy; y += 1) {
    for (int x = std::get<0>(minx_i->first); x <= maxx; x += 1) {
      coord xy(x, y);
      if (smap.find(xy) != smap.end()) {
        std::cout << '#';
      } else {
        std::cout << '.';
      }
    }
    std::cout << '\n';
  }
}

constexpr int line_length = 4;

bool check_points(skymap &smap, skymap::iterator &p) {

  if (p->second) {
    return false;
  }
  p->second = true;

  int x = std::get<0>(p->first);
  int y = std::get<1>(p->first);
  int found = 0;
  for (int x1 = x + 1; x1 <= x + line_length; x1 += 1) {
    auto p2 = smap.find(coord(x1, y));
    if (p2 == smap.end()) {
      break;
    } else {
      found += 1;
    }
  }
  if (found == line_length) {
    for (int x1 = x + 1; x1 <= x + line_length; x1 += 1) {
      smap.find(coord(x1, y))->second = true;
    }
    return true;
  }
  found = 0;
  for (int x1 = x - 1; x1 >= x - line_length; x1 -= 1) {
    auto p2 = smap.find(coord(x1, y));
    if (p2 == smap.end()) {
      break;
    } else {
      found += 1;
    }
  }
  if (found == line_length) {
    for (int x1 = x - 1; x1 >= x - line_length; x1 -= 1) {
      smap.find(coord(x1, y))->second = true;
    }
    return true;
  }
  found = 0;
  for (int y1 = y + 1; y1 <= y + line_length; y1 += 1) {
    auto p2 = smap.find(coord(x, y1));
    if (p2 == smap.end()) {
      break;
    } else {
      found += 1;
    }
  }
  if (found == line_length) {
    for (int y1 = y + 1; y1 <= y + 4; y1 += 1) {
      smap.find(coord(x, y1))->second = true;
    }
    return true;
  }
  found = 0;
  for (int y1 = y - 1; y1 >= y - line_length; y1 += 1) {
    auto p2 = smap.find(coord(x, y1));
    if (p2 == smap.end()) {
      break;
    } else {
      found += 1;
    }
  }
  if (found == line_length) {
    for (int y1 = y - 1; y1 >= y - line_length; y1 -= 1) {
      smap.find(coord(x, y1))->second = true;
    }
    return true;
  }
  return false;
}

bool possible_words(skymap &smap) {
  int found = 0;
  for (auto p = smap.begin(); p != smap.end(); ++p) {
    found += check_points(smap, p);
    if (found == 4) {
      return true;
    }
  }
  return false;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " INPUT.TXT\n";
    return 1;
  }

  skyvec sky;
  std::regex desc_re{
      R"(position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>)"};
  std::ifstream input{argv[1]};
  for (std::string line; std::getline(input, line);) {
    std::smatch groups;
    if (std::regex_match(line, groups, desc_re)) {
      sky.emplace_back(std::stof(groups[1].str()), std::stof(groups[2].str()),
                       std::stof(groups[3].str()), std::stof(groups[4].str()));
    } else {
      std::cerr << "Bad input line: " << line << '\n';
      return 1;
    }
  }

  int count = 0;
  while (true) {
    auto smap = make_skymap(sky);
    if (possible_words(smap)) {
      print_skymap(smap);
      std::cout << "Match (Y/N)? ";
      std::string line;
      std::cin >> line;
      if (line == "Y" || line == "y") {
        std::cout << "Steps: " << count << '\n';
        return 0;
      }
    }
    step(sky);
    count += 1;
  }
  return 0;
}
