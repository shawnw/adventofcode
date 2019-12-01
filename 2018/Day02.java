import java.lang.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;
import java.io.IOException;
// Can't figure out how to get apache commons text on Ubuntu
import org.apache.commons.lang3.StringUtils;

class Day02 {
    private int twos, threes;
    private Path data;
    
    public Day02(String input_file) {
        twos = 0;
        threes = 0;
        data = Paths.get(input_file);
    }

    private void check_id(String id) {
        Collection<Integer> v =
            id.chars().collect(HashMap<Integer,Integer>::new,
                               (map, cp) ->
                               map.put(cp, map.getOrDefault(cp, 0) + 1),
                               HashMap<Integer,Integer>::putAll).values();
        if (v.stream().anyMatch(i -> i == 2)) {
            twos += 1;
        }
        if (v.stream().anyMatch(i -> i == 3)) {
            threes += 1;
        }
    }

    private void solve() throws IOException {
        twos = 0;
        threes = 0;
        Files.lines(data).forEach(this::check_id);
        int checksum = twos * threes;
        System.out.println("Part 1: " + checksum);

        String[] alines = Files.readAllLines(data).toArray(new String[0]);
        for (int i = 0; i < alines.length; i += 1) {
            for (int j = 0; j < alines.length; j += 1) {
                if (i == j) {
                    continue;
                }
                if (StringUtils.getLevenshteinDistance(alines[i], alines[j]) == 1) {
                    StringBuilder sb = new StringBuilder();
                    for (int m = 0; m < alines[i].length(); m += 1) {
                        if (alines[i].charAt(m) == alines[j].charAt(m)) {
                            sb.append(alines[i].charAt(m));
                        }
                    }
                    System.out.println("Part 2: " + sb.toString());
                    return;
                }
            }
        }
    }
    
    public static void main(String[] argv) {
        try {
            Day02 d = new Day02(argv[0]);
            d.solve();
        } catch (Exception e) {
            System.err.println(e);
        }
    }
}
