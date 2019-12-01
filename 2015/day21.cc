#include <iostream>
#include <string>
#include <algorithm>
#include <vector>
#include <nmmintrin.h>

struct fighter {
	int hp;
	int damage;
	int armor;
	int eq_cost;
};

struct item {
	int cost;
	int damage;
	int armor;
};

using ivec = std::vector<item>;
using intvec = std::vector<int>;

bool fight(fighter you, fighter boss) {
	int damage;
	while (1) {
			boss.hp -= std::max(1, you.damage - boss.armor);
			if (boss.hp <= 0)
				return true;
			you.hp -= std::max(1, boss.damage - you.armor);
			if (you.hp <= 0)
				return false;
	}
}

fighter equip(fighter f, const item &i) {
	f.damage += i.damage;
	f.armor += i.armor;
	f.eq_cost += i.cost;
	return f;
}

void minmax_rings(const fighter &you, const fighter &boss, ivec &rings,
	intvec &costs_win, intvec &costs_lose) {
	// 0 to 2 rings.
	if (fight(you, boss))
		costs_win.push_back(you.eq_cost);
	else
		costs_lose.push_back(you.eq_cost);
	for (int r = 1; r < 1<<rings.size(); r++) {
		if (_mm_popcnt_u32(r) > 2)
			continue;
		fighter f = you;
		for (int i = 0; i < rings.size(); i++) {
			if (r & (1 << i))
				f = equip(f, rings[i]);
		}
		if (fight(f, boss))
			costs_win.push_back(f.eq_cost);
		else
			costs_lose.push_back(f.eq_cost);
	}
}

void minmax_armor(const fighter &you, const fighter &boss, const ivec &armor,
	ivec &rings, intvec &costs_win, intvec &costs_lose) {
	// 0 or 1 armor.
	minmax_rings(you, boss, rings, costs_win, costs_lose);
	for (auto &a : armor) 
		minmax_rings(equip(you, a), boss, rings, costs_win, costs_lose);
}

void minmax_eq(const fighter &you, const fighter &boss, const ivec &weapons,
	const ivec &armor, ivec &rings, intvec &costs_win, intvec &costs_lose) {
	// 1 weapon
	for (auto &w : weapons)
		minmax_armor(equip(you, w), boss, armor, rings, costs_win, costs_lose);
}

int main(void) {
	fighter you = {100, 0, 0, 0}, boss = {0,0,0,0};
	std::string line;
	
	std::vector<item> weapons = {{8,4,0}, {10, 5, 0}, {25, 6, 0},
			{40,7,0}, {74, 8, 0}};
	std::vector<item> armor = {{13,0,1}, {31, 0, 2}, {53, 0, 3},
			{75, 0, 4}, {102, 0, 5}};
	std::vector<item> rings = {{25, 1, 0}, {50, 2, 0}, {100, 3, 0},
			{20, 0, 1}, {40, 0, 2}, {80, 0, 3}};

	while (std::getline(std::cin, line)) {
		auto p = line.find(": ");
		std::string what = line.substr(0, p);
		int q = std::stoi(line.substr(p + 2));
		if (what == "Hit Points")
			boss.hp = q;
		else if (what == "Damage")
			boss.damage = q;
		else if (what == "Armor")
			boss.armor = q;
		else {
			std::cerr << "Unknown line: " << line << '\n';
			return 1;
		}
	}
	
	std::vector<int> costs_win, costs_lose;
	minmax_eq(you, boss, weapons, armor, rings, costs_win, costs_lose);
	
	std::cout << "Minimum cost to win: " << *std::min_element(costs_win.begin(),
			costs_win.end()) << '\n';
	std::cout << "Maximum cost to lose: " << *std::max_element(costs_lose.begin(),
			costs_lose.end()) << '\n';
	
	return 0;
}
