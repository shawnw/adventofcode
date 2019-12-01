#include <iostream>
#include <string>
#include <vector>
#include <regex>

enum class OP {
	HLFA, HLFB, TPLA, TPLB, INCA, INCB, JMP, JIEA, JIEB, JIOA, JIOB
};

struct insn {
	OP op;
	int offset;
	insn(OP o, int off) : op(o), offset(off) {}
};

using bytecode = std::vector<insn>;

void execute(bytecode &bc, unsigned &a, unsigned &b) {
	int pc = 0;
	
	try {
		while (1) {
			insn i = bc.at(pc);
			switch (i.op) {
			case OP::HLFA:
				a /= 2;
				pc += 1;
				break;
			case OP::HLFB:
				b /= 2;
				pc += 1;
				break;
			case OP::TPLA:
				a *= 3;
				pc += 1;
				break;
			case OP::TPLB:
				b *= 3;
				pc += 1;
				break;
			case OP::INCA:
				a += 1;
				pc += 1;
				break;
			case OP::INCB:
				b += 1;
				pc += 1;
				break;
			case OP::JMP:
				pc += i.offset;
				break;
			case OP::JIEA:
				if ((a & 1) == 0)
					pc += i.offset;
				else
					pc += 1;
				break;
			case OP::JIEB:
				if ((b & 1) == 0)
					pc += i.offset;
				else
					pc += 1;
				break;
			case OP::JIOA:
				if (a == 1)
					pc += i.offset;
				else
					pc += 1;
				break;
			case OP::JIOB:
				if (b == 1)
					pc += i.offset;
				else
					pc += 1;
				break;
			}
		}
	} catch (std::out_of_range e) {
		return;
	}
}

int main(int argc, char **argv) {
	bytecode program;
	std::string line;
	std::regex jmp_insn { R"(jmp\s+([+-]\d+))" };
	std::regex cjmp_insn { R"((jio|jie)\s+([ab]),\s+([+-]\d+))" };
	std::regex reg_insn { R"((inc|tpl|hlf)\s+([ab]))" };
	
	while (std::getline(std::cin, line)) {
		std::smatch fields;
		OP op;
		if (std::regex_match(line, fields, jmp_insn))
			program.emplace_back(OP::JMP, std::stoi(fields[1]));
		else if (std::regex_match(line, fields, cjmp_insn)) {
			if (fields[1] == "jio")
				op = fields[2] == "a" ? OP::JIOA : OP::JIOB;
			else if (fields[1] == "jie")
				op = fields[2] == "a" ? OP::JIEA : OP::JIEB;
			program.emplace_back(op, std::stoi(fields[3]));
		} else if (std::regex_match(line, fields, reg_insn)) {
			if (fields[1] == "inc")
				op = fields[2] == "a" ? OP::INCA : OP::INCB;
			else if (fields[1] == "tpl")
				op = fields[2] == "a" ? OP::TPLA : OP::TPLB;
			else if (fields[1] == "hlf")
				op = fields[2] == "a" ? OP::HLFA : OP::HLFB;
			program.emplace_back(op, 0);
		} else {
			std::cerr << "Unknown line '" << line << "'\n";
			return 1;
		}
	}
	
	unsigned a = argc == 2, b = 0;
	execute(program, a, b);
	std::cout << "Register B is " << b << '\n';
	return 0;
}