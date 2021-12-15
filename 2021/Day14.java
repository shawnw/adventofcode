import java.util.*;
import java.util.regex.*;
import java.util.stream.*;
import java.nio.file.*;
import java.io.IOException;

public class Day14 {
    private static Map<String, Character> transforms = null;

    private static Map<String, Long> step(Map<String, Long> polymer) {
        var newPoly = new HashMap<String, Long>();
        char[] leftPair = new char[2];
        char[] rightPair = new char[2];
        for (var pair : polymer.entrySet()) {
            if (pair.getKey().length() == 2) {
                char midChar = transforms.get(pair.getKey());
                leftPair[0] = pair.getKey().charAt(0);
                leftPair[1] = midChar;
                rightPair[0] = midChar;
                rightPair[1] = pair.getKey().charAt(1);
                newPoly.merge(new String(leftPair), pair.getValue(), Long::sum);
                newPoly.merge(new String(rightPair), pair.getValue(), Long::sum);
            } else {
                newPoly.put(pair.getKey(), pair.getValue());
            }
        }
        return newPoly;
    }

    private static long score(Map<String, Long> polymer) {
        var stats = polymer.entrySet().stream()
            .collect(Collectors.groupingBy(e -> e.getKey().charAt(0),
                                           Collectors.summingLong(Map.Entry::getValue)))
            .values()
            .stream()
            .collect(Collectors.summarizingLong(Long::longValue));
        return stats.getMax() - stats.getMin();
    }

    private static final Pattern trans_re
        = Pattern.compile("^([A-Z][A-Z]) -> ([A-Z])$");
    public static void main(String[] args) {
        try (var scanner = new Scanner(Path.of(args[0]))) {
            String template = scanner.nextLine();
            transforms = new HashMap<String, Character>();
            scanner.nextLine();
            while (scanner.hasNextLine()) {
                String trans = scanner.nextLine();
                var matcher = trans_re.matcher(trans);
                if (matcher.matches()) {
                    transforms.put(matcher.group(1),
                                   matcher.group(2).charAt(0));
                } else {
                    throw new UnsupportedOperationException(trans);
                }
            }

            Map<String, Long> polymer = new HashMap<String,Long>();
            for (int n = 0; n < template.length() - 1; n++) {
                polymer.merge(template.substring(n, n + 2), 1l, Long::sum);
            }
            polymer.put(template.substring(template.length() - 1), 1l);

            for (int n = 0; n < 40; n++) {
                polymer = step(polymer);
                if (n == 9) {
                    System.out.println("Part 1: " + score(polymer));
                }
            }
            System.out.println("Part 2: " + score(polymer));
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
