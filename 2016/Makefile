PACKAGES=batteries

SOURCES=day01.ml day02.ml day03.ml day04.ml day05.ml day06.ml	\
	day07.ml day08.ml day09.ml day10.ml day11.ml day12.ml	\
	day13.ml day14.ml day15.ml day16.ml day17.ml day18.ml	\
	day19.ml day20.ml day21.ml day22.ml day23.ml day24.ml	\
	day25.ml

all: byte
	@echo "Merry Christmas!"

byte: $(SOURCES:.ml=.byte)


native: $(SOURCES:.ml=.native)

clean:
	rm -f *.byte *.native
	rm -rf _build

%.byte : %.ml
	ocamlbuild -use-ocamlfind $@ -pkgs $(PACKAGES)

%.native: %.ml
	ocamlbuild -use-ocamlfind $@ -pkgs $(PACKAGES)

day13.ml: astar.mli astar.ml

