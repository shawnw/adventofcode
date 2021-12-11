import java.util.*;
import java.util.function.*;
import java.util.stream.*;
import java.nio.file.*;
import java.io.*;

class Day03 {
    private static class O2Filter implements Predicate<String> {
        private int pos, ones, zeros;

        public O2Filter(int n, List<String> nums) {
            pos = n;
            ones = zeros = 0;
            for (String num : nums) {
                if (num.charAt(pos) == '1') {
                    ones++;
                } else {
                    zeros++;
                }
            }
        }

        public boolean test(String num) {
            if (ones >= zeros) {
                return num.charAt(pos) == '0';
            } else {
                return num.charAt(pos) == '1';
            }
        }
    }

    public static void main(String[] args) {
        try {
            List<String> input = Files.readAllLines(Path.of(args[0]));
            int nbytes = input.get(0).length();
            int[] ones = new int[nbytes];
            Arrays.fill(ones, 0);
            for (String line : input) {
                var chars = line.toCharArray();
                for (int n = 0; n < chars.length; n++) {
                    if (chars[n] == '1') {
                        ones[n]++;
                    }
                }
            }
            int gamma = 0;
            int mask = 0;
            for (int n = 0; n < ones.length; n++) {
                mask = (mask << 1) + 1;
                if (ones[n] > input.size() - ones[n]) {
                    gamma = (gamma << 1) + 1;
                } else {
                    gamma <<= 1;
                }
            }
            int epsilon = ~gamma & mask;
            System.out.println("Part 1: " + (gamma * epsilon));

            var oxygen = input;
            var co2 = input;
            for (int n = 0; n < ones.length; n++) {
                if (oxygen.size() > 1) {
                    oxygen = oxygen.stream().filter(new O2Filter(n, oxygen)).toList();
                }
                if (co2.size() > 1) {
                    co2 = co2
                        .stream()
                        .filter(Predicate.not(new O2Filter(n, co2)))
                        .toList();
                }
                if (oxygen.size() == 1 && co2.size() == 1) {
                    break;
                }
            }

            int o2r = Integer.parseInt(oxygen.get(0), 2);
            int co2r = Integer.parseInt(co2.get(0), 2);
            System.out.println("Part 2: " + (o2r * co2r));
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
