#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>
#include <list>

using circle = std::list<int>;

circle::iterator
step_forward(circle &v, circle::iterator pos, int steps) {
  while (steps--) {
    if (pos == v.end()) {
      pos = v.begin();
    } else {
      ++pos;
    }
  }
  if (pos == v.begin()) {
    ++pos;
  }
  return pos;
}

circle::iterator
step_backwards(circle &v, circle::iterator pos, int steps) {
  while (steps--) {
    if (pos == v.begin()) {
      pos = v.end();
      --pos;
    } else {
      --pos;
    }
  }
  return pos;
}

int main(int argc, char **argv) {
  int nplayers, nmarbles;

  if (argc != 3) {
    std::cerr << "Usage: " << argv[0] << " NPLAYERS NMARBLES\n";
    return 1;
  }

  nplayers = std::stoi(argv[1]);
  nmarbles = std::stoi(argv[2]);

  circle marbles{};
  marbles.push_back(0);
  auto curr_marble = marbles.begin();

  std::vector<unsigned long long> scores(nplayers, 0ULL);

  int next_marble = 1;
  int player = 0;
  while (next_marble <= nmarbles) {
    if (next_marble % 23 == 0) {
      scores[player] += next_marble;
      auto other_marble = step_backwards(marbles, curr_marble, 7);
      scores[player] += *other_marble;
      curr_marble = marbles.erase(other_marble);
    } else {
      auto new_marble = step_forward(marbles, curr_marble, 2);
      curr_marble = marbles.insert(new_marble, next_marble);
    }

    next_marble += 1;
    player += 1;
    player %= nplayers;
  }

  std::cout << "Score: " << *std::max_element(scores.begin(), scores.end())
            << '\n';

  return 0;
}
