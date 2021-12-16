import java.util.*;
import java.nio.file.*;
import java.io.IOException;
import aoc.util.*;

public class Day15 {
    private static record CoordByRisk(Coord pos, int risk) {}

    private static List<Coord> getNeighbors(Coord pos, int maxX, int maxY) {
        var neighbors = new ArrayList<Coord>(4);
        int x = pos.x();
        int y = pos.y();
        if (x > 0) {
            neighbors.add(new Coord(x - 1, y));
        }
        if (x < maxX - 1) {
            neighbors.add(new Coord(x + 1, y));
        }
        if (y > 0) {
            neighbors.add(new Coord(x, y - 1));
        }
        if (y < maxY - 1) {
            neighbors.add(new Coord(x, y + 1));
        }
        return neighbors;
    }

    private static int getRisk(Coord pos, int[][] grid, int maxX, int maxY) {
        int x = pos.x();
        int y = pos.y();
        return ((grid[y % grid.length][x % grid[0].length]
                 + (y / grid.length)
                 + (x / grid[0].length) - 1)
                % 9)
            + 1;
    }

    private static int dijkstra(int[][] grid, boolean part2) {
        int maxY = part2 ? grid.length * 5 : grid.length;
        int maxX = part2 ? grid[0].length * 5 : grid.length;
        Coord end = new Coord(maxX - 1, maxY - 1);

        boolean[][] visited = new boolean[maxY + 1][maxX + 1];

        var queue =
            new PriorityQueue<CoordByRisk>((a,b) -> Integer.compare(a.risk(),
                                                                    b.risk()));
        queue.add(new CoordByRisk(new Coord(0, 0), 0));

        int risks[][] = new int[maxY + 1][maxX + 1];
        for (int n = 0; n < risks.length; n++) {
            Arrays.fill(risks[n], Integer.MAX_VALUE);
        }
        risks[0][0] = 0;

        while (!queue.isEmpty()) {
            var current = queue.poll();
            Coord currentNode = current.pos();
            if (current.risk() > risks[currentNode.y()][currentNode.x()]) {
                continue;
            }
            if (currentNode.equals(end)) {
                return risks[currentNode.y()][currentNode.x()];
            }
            if (visited[currentNode.y()][currentNode.x()]) {
                continue;
            }
            for (Coord neighbor : getNeighbors(currentNode, maxX, maxY)) {
                if (visited[neighbor.y()][neighbor.x()]) {
                    continue;
                }
                int neighborRisk =
                    current.risk() + getRisk(neighbor, grid, maxX, maxY);
                if (neighborRisk < risks[neighbor.y()][neighbor.x()]) {
                    risks[neighbor.y()][neighbor.x()] = neighborRisk;
                    queue.add(new CoordByRisk(neighbor, neighborRisk));
                }
            }
            visited[currentNode.y()][currentNode.x()] = true;
        }
        return -1;
    }

    public static void main(String[] args) {
        try (var lines = Files.lines(Path.of(args[0]))) {
            int[][] grid = lines.map(line ->
                                     line.codePoints()
                                     .map(Character::getNumericValue)
                                     .toArray())
                .toArray(int[][]::new);
            int risk = dijkstra(grid, false);
            System.out.println("Part 1: " + risk);
            risk = dijkstra(grid, true);
            System.out.println("Part 2: " + risk);
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
