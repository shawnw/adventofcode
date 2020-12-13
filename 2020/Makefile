# Build native-code versions of solution programs

CSC=/usr/local/bin/csc
CSCOPTS=-O3

.PHONY: all

all: day01 day02 day03 day04 day05 day06 day07 day08 day09 day10 day11

.PHONY: clean
clean:
	rm -f day[012][0-9]

%: %.scm
	$(CSC) -o $@ $(CSCOPTS) $^
