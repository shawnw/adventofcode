import java.util.*;
import java.util.regex.*;
import java.nio.file.*;
import java.io.IOException;
import aoc.util.*;

public class Day13 {
    private static void foldX(Set<Coord> paper, int x) {
        var folded = paper.stream().filter(c -> c.x() > x).toList();
        paper.removeAll(folded);
        paper.addAll(folded.stream()
                     .map(c -> new Coord(Math.abs(c.x() - (x * 2)), c.y()))
                     .toList());
    }

    private static void foldY(Set<Coord> paper, int y) {
        var folded = paper.stream().filter(c -> c.y() > y).toList();
        paper.removeAll(folded);
        paper.addAll(folded.stream()
                     .map(c -> new Coord(c.x(), Math.abs(c.y() - (y * 2))))
                     .toList());
    }

    private static final Pattern fold_re =
        Pattern.compile("^fold along ([xy])=(\\d+)");
    private static void doFold(Set<Coord> paper, String direction) {
        var match = fold_re.matcher(direction);
        if (match.matches()) {
            if (match.group(1).equals("x")) {
                foldX(paper, Integer.parseInt(match.group(2)));
            } else {
                foldY(paper, Integer.parseInt(match.group(2)));
            }
        } else {
            throw new UnsupportedOperationException(direction);
        }
    }

    private static void displayPaper(Set<Coord> paper) {
        int xMax = paper.stream().mapToInt(Coord::x).max().orElse(0) + 1;
        int yMax = paper.stream().mapToInt(Coord::y).max().orElse(0) + 1;
        char[][] grid = new char[yMax][xMax];
        for (int y = 0; y < yMax; y++) {
            Arrays.fill(grid[y], ' ');
        }
        for (Coord c : paper) {
            grid[c.y()][c.x()] = '#';
        }
        for (int y = 0; y < yMax; y++) {
            System.out.print(grid[y]);
            System.out.print('\n');
        }
    }

    public static void main(String[] args) {
        try (var reader = new Scanner(Path.of(args[0]))) {
            Set<Coord> paper = new HashSet<Coord>();
            while (reader.hasNextLine()) {
                String line = reader.nextLine();
                if (line.isEmpty()) {
                    break;
                }
                int[] coords = Strings.csvOfString(line);
                paper.add(new Coord(coords[0], coords[1]));
            }

            int nFolds = 0;
            while (reader.hasNextLine()) {
                doFold(paper, reader.nextLine());
                if (++nFolds == 1) {
                    System.out.println("Part 1: " + paper.size());
                }
            }
            displayPaper(paper);
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
