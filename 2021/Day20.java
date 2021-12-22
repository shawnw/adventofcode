import java.util.*;
import java.util.concurrent.*;
import java.util.stream.*;
import java.nio.file.*;
import java.io.IOException;
import aoc.util.Coord;

public class Day20 {

    private static Map<Coord, Boolean> enhance(Map<Coord, Boolean> pixels,
                                               char[] enhance_data, int turn) {
        var newPixels = new ConcurrentHashMap<Coord, Boolean>();
        var x_info = pixels.keySet().stream().mapToInt(Coord::x).summaryStatistics();
        var y_info = pixels.keySet().stream().mapToInt(Coord::y).summaryStatistics();

        Boolean defState = enhance_data[0] == '#' && (turn % 2) == 1;

        IntStream.rangeClosed(x_info.getMin() - 1, x_info.getMax() + 1)
            .boxed()
            .flatMap(x -> IntStream.rangeClosed(y_info.getMin() - 1,
                                                y_info.getMax() + 1)
                     .mapToObj(y -> new Coord(x, y)))
            .parallel()
            .forEach(c -> {
                    int index = 0;
                    int x = c.x();
                    int y = c.y();
                    if (pixels.getOrDefault(new Coord(x - 1, y - 1),
                                            defState)) {
                        index = 1;
                    }
                    if (pixels.getOrDefault(new Coord(x, y - 1), defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }
                    if (pixels.getOrDefault(new Coord(x + 1, y - 1), defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }

                    if (pixels.getOrDefault(new Coord(x - 1, y), defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }
                    if (pixels.getOrDefault(c, defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }
                    if (pixels.getOrDefault(new Coord(x + 1, y), defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }

                    if (pixels.getOrDefault(new Coord(x - 1, y + 1), defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }
                    if (pixels.getOrDefault(new Coord(x, y + 1), defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }
                    if (pixels.getOrDefault(new Coord(x + 1, y + 1), defState)) {
                        index = (index << 1) | 1;
                    } else {
                        index = index << 1;
                    }

                    newPixels.put(c, enhance_data[index] == '#');
                });

        return newPixels;
    }


    public static void main(String[] args) {
        try (var input = new Scanner(Path.of(args[0]))) {
            var enhance_data = input.nextLine().toCharArray();
            input.nextLine(); // Discard blank link

            Map<Coord, Boolean> pixels = new HashMap<Coord, Boolean>();
            for (int y = 0; input.hasNextLine(); y++) {
                var points = input.nextLine().toCharArray();
                for (int x = 0; x < points.length; x++) {
                    pixels.put(new Coord(x, y), points[x] == '#');
                }
            }

            for (int n = 0; n < 50; n++) {
                pixels = enhance(pixels, enhance_data, n);
                if (n == 1) {
                    long part1 = pixels.entrySet().stream()
                        .filter(Map.Entry::getValue)
                        .count();
                    System.out.println("Part 1: " + part1);
                }
            }

            long part2 = pixels.entrySet().parallelStream()
                .filter(Map.Entry::getValue)
                .count();
            System.out.println("Part 2: " + part2);

        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
