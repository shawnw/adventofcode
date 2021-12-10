import java.util.*;
import java.util.function.*;
import java.util.regex.*;
import java.util.stream.*;
import java.nio.file.*;
import java.io.*;
import aoc.util.*;

public class Day05_2 {
    private static void parseMatch(Map<Coord, Integer> coords, Matcher m, boolean part2) {
        int x1 = Integer.parseInt(m.group(1));
        int y1 = Integer.parseInt(m.group(2));
        int x2 = Integer.parseInt(m.group(3));
        int y2 = Integer.parseInt(m.group(4));
        if (x1 == x2) {
            int begy = Integer.min(y1, y2);
            int endy = Integer.max(y1, y2);
            for (int y = begy; y <= endy; y++) {
                coords.compute(new Coord(x1, y), (k,v) -> (v == null) ? 1 : v + 1);
            }
        } else if (y1 == y2) {
            int begx = Integer.min(x1, x2);
            int endx = Integer.max(x1, x2);
            for (int x = begx; x <= endx; x++) {
                coords.compute(new Coord(x, y1), (k,v) -> (v == null) ? 1 : v + 1);
            }
        } else if (part2) {
            int deltax = x1 < x2 ? 1 : -1;
            int deltay = y1 < y2 ? 1 : -1;
            for (int x = x1, y = y1;
                 !(x == x2 + deltax && y == y2 + deltay);
                 x += deltax, y += deltay) {
                coords.compute(new Coord(x, y), (k,v) -> (v == null) ? 1 : v + 1);
            }
        }
    }

    private static long solve(Map<Coord, Integer> coords) {
        return coords.values().stream().filter(v -> v > 1).count();
    }

    public static void main(String[] args) {
        try {
            var re = Pattern.compile("(\\d+),(\\d+) -> (\\d+),(\\d+)");
            var input = Files.lines(Path.of(args[0]))
                .map(line -> re.matcher(line))
                .filter(m -> m.matches()).toList();

            var coords = new HashMap<Coord, Integer>();
            for (var m : input) {
                parseMatch(coords, m, false);
            }
            long intersections1 = solve(coords);
            System.out.println("Part 1: " + intersections1);

            coords.clear();
            for (var m : input) {
                parseMatch(coords, m, true);
            }
            long intersections2 = solve(coords);
            System.out.println("Part 2: " + intersections2);

        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
