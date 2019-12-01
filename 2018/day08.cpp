#include <iostream>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <vector>

struct node {
  std::vector<int> metadata;
  std::vector<std::unique_ptr<node>> children;
  int subvalue;
  int metasum;
  node(int nmeta, int nchildren) : subvalue(-1), metasum(-1) {
    metadata.reserve(nmeta);
    children.reserve(nchildren);
  }
  int count_metadata();
  int value_of();
};

std::unique_ptr<node> read_metadata(std::istream &in) {
  int nchildren, nmeta;

  if (!(in >> nchildren >> nmeta)) {
    throw std::runtime_error{"Input failed!"};
  }

  auto n = std::make_unique<node>(nchildren, nmeta);

  for (int i = 0; i < nchildren; i += 1) {
    n->children.push_back(read_metadata(in));
  }

  for (int i = 0; i < nmeta; i += 1) {
    int meta;
    if (!(in >> meta)) {
      throw std::runtime_error{"Input of metadata failed!"};
    }
    n->metadata.push_back(meta);
  }

  return n;
}

int node::count_metadata() {
  if (metasum >= 0) {
    return metasum;
  }

  int meta = 0;
  for (const auto &c : children) {
    meta += c->count_metadata();
  }
  meta += std::accumulate(metadata.begin(), metadata.end(), 0);
  metasum = meta;
  return meta;
}

int node::value_of() {
  if (subvalue >= 0) {
    return subvalue;
  }

  if (children.empty()) {
    if (metasum >= 0) {
      subvalue = metasum;
    } else {
      metasum = subvalue = std::accumulate(metadata.begin(), metadata.end(), 0);
    }
    return subvalue;
  }

  int val = 0;
  for (auto c : metadata) {
    if (static_cast<std::vector<int>::size_type>(c) > children.size()) {
      continue;
    }
    val += children[c - 1]->value_of();
  }
  subvalue = val;
  return val;
}

int main(void) {
  try {
    auto root = read_metadata(std::cin);
    std::cout << "Part 1: " << root->count_metadata() << '\n';
    std::cout << "Part 2: " << root->value_of() << '\n';
  } catch (std::exception &e) {
    std::cerr << e.what() << '\n';
    return 1;
  }
  return 0;
}
