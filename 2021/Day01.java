import java.io.*;
import java.util.*;
import java.util.stream.*;
import aoc.util.*;

class Day01 {
    public static void main(String[] args) {
        try {
            var input = Readers.intStreamOfFile(args[0]).toArray();

            int part1 = 0;
            for (int n = 1; n < input.length; n++) {
                if (input[n] > input[n - 1]) {
                    part1++;
                }
            }
            System.out.println("Part 1: " + part1);

            int part2 = 0;
            for (int n = 3; n < input.length; n++) {
                if (input[n] + input[n - 1] + input[n - 2] > input[n - 1] + input[n - 2] + input[n - 3]) {
                    part2++;
                }
            }
            System.out.println("Part 2: " + part2);
        } catch (IOException e) {
            System.err.println(e);
        }
    }
}
