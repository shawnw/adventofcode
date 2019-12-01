#include <iostream>
#include <vector>
#include <iterator>

int step_through(std::vector<int> jumps, int offset3 = 1) {
  int steps = 0;
  int i = 0;
  int len = jumps.size();

  while (i >= 0 && i < len) {
    auto v = jumps[i];
    if (v >= 3)
      jumps[i] += offset3;
    else
      jumps[i] += 1;
    i += v;
    steps += 1;
  }
  return steps;
}

int main() {
  std::vector<int> jumps(std::istream_iterator<int>{std::cin}, {});
  std::vector<int> test{0, 3, 0, 1, -3};

  std::cout << "Test 1: " << step_through(test) << '\n';
  std::cout << "Test 2: " << step_through(test, -1) << '\n';

  std::cout << "Part 1: " << step_through(jumps) << '\n';
  std::cout << "Part 2: " << step_through(jumps, -1) << '\n';
  
  return 0;
}
