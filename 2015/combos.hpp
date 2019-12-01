#include <vector>
#include <iterator>
#include <algorithm>

constexpr int factorial(int n) {
	return n == 1 ? 1 : n * factorial(n - 1);
}

constexpr int ncr(int n, int r) {
	return factorial(n) / (factorial(r) * factorial(n - r));
}

constexpr int npr(int n, int r) {
	return factorial(n) / factorial(n - r);
}

template<typename N>
class combinations {
public:
	using result_type = std::vector<std::vector<N>>;
private:
	std::vector<N> items;
	void gencombos(std::vector<N>, int r, result_type &results);
public:
	explicit combinations(const std::vector<N> &i) : items(i) {}
	int count(int r) { return ncr(items.size(), r); }
	result_type combos(int r);
};

template<typename N>
void combinations<N>::gencombos(std::vector<N> items, int r, result_type &results) {
	if (r <= 0)
		return;
	else if (r == 1) {
		for (auto i : items) {
			std::vector<N> v = { i };
			results.push_back(std::move(v));
		}
	} else {
		int n = 0;
		while (items.size()) {
			result_type nminus1;
			auto i = items.back();
			items.pop_back();
			gencombos(items, r - 1, nminus1);
			for (auto &c : nminus1) {
				c.push_back(i);
				results.push_back(std::move(c));
			}
			n += 1;
		}
	}
}

template<typename N>
auto combinations<N>::combos(int r) -> result_type {
	if (r < 0 || r > static_cast<int>(items.size()))
		throw std::out_of_range{"combo"};
	result_type combo;
	gencombos(items, r, combo);
	return combo;
}