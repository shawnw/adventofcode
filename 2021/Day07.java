import java.util.*;
import java.util.stream.*;
import java.io.*;
import aoc.util.*;

public class Day07 {
    private static int cost1(int pos, int[] crabs) {
        int fuel = 0;
        for (int crab : crabs) {
            fuel += Math.abs(pos - crab);
        }
        return fuel;
    }

    private static int cost2(int pos, int[] crabs) {
        int fuel = 0;
        for (int crab : crabs) {
            int moves = Math.abs(pos - crab);
            fuel += (moves * (moves + 1)) / 2;
        }
        return fuel;
    }

    public static void main(String[] args) {
        try {
            int[] input = Readers.intsOfCSVFile(args[0]);

            var stats = Arrays.stream(input).summaryStatistics();
            int minPos = stats.getMin();
            int maxPos = stats.getMax();

            int part1 =
                IntStream.rangeClosed(minPos, maxPos)
                .map(pos -> cost1(pos, input)).min().getAsInt();
            System.out.println("Part 1: " + part1);

            int part2 =
                IntStream.rangeClosed(minPos, maxPos)
                .map(pos -> cost2(pos, input)).min().getAsInt();
            System.out.println("Part 2: " + part2);
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
