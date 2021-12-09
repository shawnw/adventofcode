import java.util.*;
import java.nio.file.*;
import java.io.*;

public class Day09 {
    private static record Coord(int x, int y) {}

    private static boolean isLowPoint(int[][] map, int x, int y) {
        int point = map[x][y];
        if (x > 0 && point >= map[x - 1][y]) {
            return false;
        } else if (x < map.length - 1 && point >= map[x + 1][y]) {
            return false;
        } else if (y > 0 && point >= map[x][y - 1]) {
            return false;
        } else if (y < map[0].length - 1 && point >= map[x][y + 1]) {
            return false;
        } else {
            return true;
        }
    }

    static int basinSize(int[][] map, Coord low) {
        int maxx = map.length;
        int maxy = map[0].length;
        boolean[][] seen = new boolean[maxx][maxy];
        Deque<Coord> pending = new ArrayDeque<Coord>();
        pending.addFirst(low);
        int size = 0;

        while (!pending.isEmpty()) {
            Coord point = pending.removeFirst();
            int x = point.x();
            int y = point.y();
            if (seen[x][y] || map[x][y] == 9) {
                continue;
            }
            seen[x][y] = true;
            size++;
            if (x > 0 && !seen[x-1][y] && map[x - 1][y] != 9) {
                pending.addLast(new Coord(x - 1, y));
            }
            if (x < maxx - 1 && !seen[x+1][y] && map[x + 1][y] != 9) {
                pending.addLast(new Coord(x + 1, y));
            }
            if (y > 0 && !seen[x][y - 1] && map[x][y - 1] != 9) {
                pending.addLast(new Coord(x, y - 1));
            }
            if (y < maxy - 1 && !seen[x][y + 1] && map[x][y + 1] != 9) {
                pending.addLast(new Coord(x, y + 1));
            }
        }

        return size;
    }

    public static void main(String[] args) {
        try {
            int[][] map =
                Files.lines(Path.of(args[0]))
                .map(line -> line.codePoints().map(Character::getNumericValue).toArray())
                .toArray(int[][]::new);
            int maxx = map.length;
            int maxy = map[0].length;
            int risk = 0;
            ArrayList<Coord> low_points = new ArrayList<Coord>();
            for (int x = 0; x < maxx; x++) {
                for (int y = 0; y < maxy; y++) {
                    if (isLowPoint(map, x, y)) {
                        risk += map[x][y] + 1;
                        low_points.add(new Coord(x, y));
                    }
                }
            }
            System.out.println("Part 1: " + risk);

            int part2 = low_points.stream()
                .mapToInt(point -> basinSize(map, point))
                .sorted()
                .skip(low_points.size() - 3)
                .reduce(1, (a,b) -> a * b);
            System.out.println("Part 2: " + part2);
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
