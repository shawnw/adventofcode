import java.util.*;
import java.nio.file.*;
import java.io.IOException;
import aoc.util.Coord;

public class Day11 {
    private static class Octopus {
        private int level;
        private boolean flashed;
        private List<Octopus> neighbors;

        public Octopus(int lvl) {
            level = lvl;
            flashed = false;
            neighbors = new ArrayList<Octopus>(8);
        }

        public void fillNeighbors(Coord me, Map<Coord, Octopus> map) {
            int x = me.x();
            int y = me.y();
            if (x > 0) {
                neighbors.add(map.get(new Coord(x - 1, y)));
                if (y > 0) {
                    neighbors.add(map.get(new Coord(x - 1, y - 1)));
                }
                if (y < 9) {
                    neighbors.add(map.get(new Coord(x - 1, y + 1)));
                 }
            }
            if (x < 9) {
                neighbors.add(map.get(new Coord(x + 1, y)));
                if (y > 0) {
                    neighbors.add(map.get(new Coord(x + 1, y - 1)));
                }
                if (y < 9) {
                    neighbors.add(map.get(new Coord(x + 1, y + 1)));
                }
            }
            if (y > 0) {
                neighbors.add(map.get(new Coord(x, y - 1)));
            }
            if (y < 9) {
                neighbors.add(map.get(new Coord(x, y + 1)));
            }
        }

        public void step() {
            if (level < 10) {
                if (!flashed) {
                    level++;
                }
                if (level == 10) {
                    flashed = true;
                    neighbors.forEach(Octopus::step);
                }
            }
        }

        public boolean hasFlashed() {
            return flashed;
        }

        public void reset() {
            flashed = false;
            if (level == 10) {
                level = 0;
            }
        }
    }

    public static void main(String[] args) {
        try {
            var input = Files.readAllLines(Path.of(args[0]));
            Map<Coord, Octopus> map = new HashMap<Coord, Octopus>(200);
            for (int x = 0; x < 10; x++) {
                int[] levels = input.get(x).chars()
                    .map(c -> Character.getNumericValue((char)c))
                    .toArray();
                for (int y = 0; y < 10; y++) {
                    map.put(new Coord(x, y), new Octopus(levels[y]));
                }
            }
            map.forEach((loc, octo) -> octo.fillNeighbors(loc, map));

            long score = 0;
            for (int step = 1; true; step++) {
                map.forEach((loc, octo) -> octo.step());
                long nFlashed = map.values().stream()
                    .filter(Octopus::hasFlashed)
                    .count();
                score += nFlashed;
                map.forEach((loc, octo) -> octo.reset());
                if (step == 100) {
                    System.out.println("Part 1: " + score);
                }
                if (nFlashed == 100) {
                    System.out.println("Part 2: " + step);
                    break;
                }
            }
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
