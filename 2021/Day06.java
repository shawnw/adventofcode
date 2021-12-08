import java.util.*;
import aoc.util.*;
import java.io.*;

public class Day06 {
    public static void main(String[] args) {
        try {
            int[] input = Readers.intsOfCSVFile(args[0]);
            long[] gens = new long[9];
            Arrays.fill(gens, 0);
            for (int age : input) {
                gens[age]++;
            }

            for (int day = 0; day < 256; day++) {
                long parents = gens[0];
                for (int n = 1; n < gens.length; n++) {
                    gens[n - 1] = gens[n];
                }
                gens[6] += parents;
                gens[8] = parents;

                if (day == 79) {
                    long total1 = Arrays.stream(gens).sum();
                    System.out.println("Part 1: " + total1);
                }
            }

            long total2 = Arrays.stream(gens).sum();
            System.out.println("Part 2: " + total2);
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
