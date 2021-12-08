package aoc.util;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class Readers {
    static class IntStreamReader implements Spliterator.OfInt {
        private Scanner scanner;

        public IntStreamReader(InputStream s) {
            scanner = new Scanner(s);
        }

        public IntStreamReader(Path p) throws IOException {
            scanner = new Scanner(p);
        }

        public long estimateSize() { return Long.MAX_VALUE; }

        public static int characteristics_c = Spliterator.IMMUTABLE | Spliterator.ORDERED;
        public int characteristics() { return characteristics_c; }

        public boolean tryAdvance(IntConsumer action) {
            if (scanner.hasNextInt()) {
                action.accept(scanner.nextInt());
                return true;
            } else {
                scanner.close();
                return false;
            }
        }

        public void forEachRemaining(IntConsumer action) {
            while (scanner.hasNextInt()) {
                action.accept(scanner.nextInt());
            }
        }

        public Spliterator.OfInt trySplit() { return null; }
    }

    static public IntStream intStreamOfStream(InputStream s) {
        return StreamSupport.intStream(new IntStreamReader(s), false);
    }

    static public IntStream intStreamOfFile(Path path) throws IOException {
        return StreamSupport.intStream(new IntStreamReader(path), false);
    }

    static public IntStream intStreamOfFile(String name) throws IOException {
        return intStreamOfFile(Path.of(name));
    }

    static public int[] intsOfCSVFile(Path path) throws IOException {
        try (var br = Files.newBufferedReader(path)) {
            return Arrays.stream(br.readLine().split(","))
                .mapToInt(Integer::parseInt)
                .toArray();
        }
    }

    static public int[] intsOfCSVFile(String name) throws IOException {
        return intsOfCSVFile(Path.of(name));
    }
}
