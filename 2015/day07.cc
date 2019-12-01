// Compile with: g++ -O -std=c++14 -o day07 day07.cc
// Run as: ./day07 < day07.txt
#include <iostream>
#include <map>
#include <unordered_map>
#include <regex>
#include <string>
#include <algorithm>
#include <cctype>
#include <cstdint>

enum OPS {
	OP_ERR, OP_AND, OP_OR, OP_NOT, OP_LSHIFT, OP_RSHIFT, OP_STORE 
};

inline bool isnumber(const std::string &n) {
	return std::all_of(n.begin(), n.end(), ::isdigit);
}

OPS stoop(const std::string &s)  {
	if (s == "AND")
		return OP_AND;
	else if (s == "OR")
		return OP_OR;
	else if (s == "LSHIFT")
		return OP_LSHIFT;
	else if (s == "RSHIFT")
		return OP_RSHIFT;
	else
		return OP_ERR;
}

struct gate {
	OPS op;
	std::string lhs;
	std::string rhs;
	std::uint16_t rhsi;
	bool memoized;
	std::uint16_t val;
	gate(void) : op(OP_ERR), lhs(""), rhs(""), rhsi(0), memoized(false), val(0) {}
};

std::ostream& operator<<(std::ostream &out, const gate &g) {
	if (g.op == OP_STORE) {
		out << g.lhs;
	} else if (g.op == OP_NOT) {
		out << "NOT " << g.lhs;
	} else if (g.op == OP_AND) {
		out << g.lhs << " AND " << g.rhs;
	} else if (g.op == OP_OR) {
		out << g.lhs << " OR " << g.rhs;
	} else if (g.op == OP_LSHIFT) {
		out << g.lhs << " LSHIFT " << g.rhs;
	} else if (g.op == OP_RSHIFT) {
		out << g.lhs << " RSHIFT " << g.rhs;
	}
	return out;
}

class logic {
		std::unordered_map<std::string, gate> circuit;
		std::uint16_t output_of(const std::string &);
	public:
		std::uint16_t find_output(const std::string &);
		void dump_outputs(void);
		void dump_ops(void);
		void reset(void);
		decltype(circuit)::size_type size(void) { return circuit.size(); }
		void add(const std::string &wire, gate g) { circuit[wire] = g; }
		gate& find_gate(const std::string &wire) { return circuit[wire]; }
		int count_memoized(void);
};	

std::uint16_t logic::output_of(const std::string &num_or_wire) {
	if (isnumber(num_or_wire))
		return std::stoi(num_or_wire);
	else
		return find_output(num_or_wire);
}

std::uint16_t logic::find_output(const std::string &wire) {
	gate &g = circuit[wire];
	
	if (g.memoized) {
		return g.val;
	} else {
		std::uint16_t r = 0;
		switch (g.op) {
			case OP_ERR:
				std::cerr << "Unknown gate operation for wire " << wire << '\n';
				return 0;
			case OP_STORE:
				r = output_of(g.lhs);
				break;
			case OP_AND:
				r = output_of(g.lhs) & output_of(g.rhs);
				break;
			case OP_OR:
				r = output_of(g.lhs) | output_of(g.rhs);
				break;
			case OP_LSHIFT:
				r = output_of(g.lhs) << g.rhsi;
				break;
			case OP_RSHIFT:
				r = output_of(g.lhs) >> g.rhsi;
				break;
			case OP_NOT:
				r = ~output_of(g.lhs);
				break;
		}
		g.memoized = true;
		g.val = r;
		return r;
	}
}

int logic::count_memoized(void) {
  return std::count_if(circuit.begin(), circuit.end(),
    [](const auto &wire){ return wire.second.memoized; });
}

void logic::dump_outputs(void) {
	for (auto &wire : circuit)
		std::cout << wire.first << ": " << find_output(wire.first) << '\n';
}

void logic::dump_ops(void) {
	for (const auto &wire : circuit) 
		std::cout << wire.second << " -> " << wire.first << '\n';
}

void logic::reset(void) {
	for (auto &gate : circuit)
		gate.second.memoized = false;
}

int main(void) {
	std::string command;
	logic circuit;
	std::regex assign_op{ "(\\w+) -> (\\w+)" };
	std::regex not_op{ "NOT (\\w+) -> (\\w+)" };
	std::regex binary_op{ "(\\w+) (AND|OR|LSHIFT|RSHIFT) (\\w+) -> (\\w+)" };
	
	while (std::getline(std::cin, command)) {
		std::smatch fields;
		gate g;
		if (std::regex_match(command, fields, assign_op)) {
			g.op = OP_STORE;
			g.lhs = fields[1];
			circuit.add(fields[2], g);
		} else if (std::regex_match(command, fields, not_op)) {
			g.op = OP_NOT;
			g.lhs = fields[1];
			circuit.add(fields[2], g);
		} else if (std::regex_match(command, fields, binary_op)) {
			g.op = stoop(fields[2]);
			g.lhs = fields[1];
			g.rhs = fields[3];
			if (isnumber(fields[3]))
				g.rhsi = std::stoi(fields[3]);
			circuit.add(fields[4], g);
		} else {
			std::cerr << "Unknown gate: " << command << '\n';
		}
	}
	
	std::cout << "There are a total of " << circuit.size() << " wires.\n";
	
	auto a = circuit.find_output("a");
	std::cout << "Initial value of a: " << a << '\n';
	
	std::cout << "Used " << circuit.count_memoized() << " wires.\n";
	
	circuit.reset();
	gate b;
	b.op = OP_STORE;
	b.lhs = std::to_string(a);
	b.memoized = true;
	b.val = a;
	circuit.add("b", b);
	
	a = circuit.find_output("a");
	std::cout << "New value of a: " << a << '\n';
	
	//circuit.dump_ops();
	
	return 0;
}
	