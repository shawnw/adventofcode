import java.util.regex.*;
import java.util.stream.*;
import java.nio.file.*;
import java.io.IOException;
import aoc.util.Coord;

public class Day17 {
    private static class Probe {
        private int x, y;
        private int x_velo, y_velo;
        private int max_y;
        private Coord ulc, lrc, initial_v;

        private enum Position { KEEP_GOING, IN_AREA, PAST }

        public Probe(int x_velo, int y_velo, Coord ulc, Coord lrc) {
            this.x = 0;
            this.y = 0;
            this.max_y = 0;
            this.x_velo = x_velo;
            this.y_velo = y_velo;
            this.initial_v = new Coord(x_velo, y_velo);
            this.ulc = ulc;
            this.lrc = lrc;
        }

        private Position step() {
            x += x_velo;
            y += y_velo;
            max_y = Integer.max(y, max_y);
            if (x_velo > 0) {
                x_velo -= 1;
            } else if (x_velo < 0) {
                x_velo += 1;
            }
            y_velo -= 1;
            if (x >= ulc.x() && x <= lrc.x() && y <= ulc.y() && y >= lrc.y()) {
                return Position.IN_AREA;
            } else if(x > lrc.x() || y < lrc.y()) {
                return Position.PAST;
            } else {
                return Position.KEEP_GOING;
            }
        }

        public boolean fire() {
            while (true) {
                Position status = step();
                if (status == Position.IN_AREA) {
                    return true;
                } else if (status == Position.PAST) {
                    return false;
                }
            }
        }

        public int maxY() { return max_y; }
        public Coord initialVelocity() { return initial_v; }
    }

    public static void main(String[] args) {
        try {
            var re =
                Pattern.compile("target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)");
            var input = Files.readAllLines(Path.of(args[0]));
            var matcher = re.matcher(input.get(0));
            if (!matcher.matches()) {
                System.err.println("Invalid input: " + input.get(0));
                System.exit(1);
            }

            Coord ulc = new Coord(Integer.parseInt(matcher.group(1)),
                                  Integer.parseInt(matcher.group(4)));
            Coord lrc = new Coord(Integer.parseInt(matcher.group(2)),
                                  Integer.parseInt(matcher.group(3)));

            var hits = IntStream.rangeClosed(1, lrc.x())
                .boxed()
                .flatMap(x -> IntStream.rangeClosed(-250, 250)
                         .mapToObj(y -> new Probe(x, y, ulc, lrc)))
                .parallel()
                .filter(Probe::fire)
                .toList();

            var part1 = hits.stream().mapToInt(Probe::maxY).max();
            System.out.println("Part 1: " + part1.orElse(Integer.MIN_VALUE));

            long part2 = hits.stream()
                .map(Probe::initialVelocity)
                .distinct()
                .count();
            System.out.println("Part 2: " + part2);

        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
