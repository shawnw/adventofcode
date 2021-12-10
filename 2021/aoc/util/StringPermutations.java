package aoc.util;
import java.util.*;
import java.util.function.*;
import org.apache.commons.collections4.iterators.*;

public class StringPermutations implements Iterable<String> {
    private class IteratorAdaptor implements Iterator<String>, Spliterator<String> {
        private PermutationIterator<Character> it;
        private int len;

        public IteratorAdaptor(String s) {
            it = new PermutationIterator<Character>(Strings.explode(s));
            len = s.length();
        }

        // Iterator functions
        @Override
        public boolean hasNext() { return it.hasNext(); }

        @Override
        public String next() { return Strings.combine(it.next()); }

        // Spliterator functions
        @Override
        public int characteristics() {
            int bits = IMMUTABLE | NONNULL;
            if (len <= 20) {
                bits |= SIZED;
            }
            return bits;
        }

        @Override
        public long estimateSize() {
            if (len <= 20) {
                long size = len;
                for (long n = len - 1; n > 1; n--) {
                    size *= n;
                }
                return size;
            } else {
                return Long.MAX_VALUE;
            }
        }

        @Override
        public boolean tryAdvance(Consumer<? super String> f) {
            if (it.hasNext()) {
                f.accept(Strings.combine(it.next()));
                return true;
            } else {
                return false;
            }
        }

        @Override
        public Spliterator<String> trySplit() {
            return null;
        }

        // Common functions
        @Override
        public void forEachRemaining(Consumer<? super String> f) {
            while (it.hasNext()) {
                f.accept(Strings.combine(it.next()));
            }
        }
    }

    private IteratorAdaptor it;

    public StringPermutations(String s) {
        it = new IteratorAdaptor(s);
    }

    @Override
    public Iterator<String> iterator() {
        return it;
    }

    @Override
    public Spliterator<String> spliterator() {
        return it;
    }

    static public StringPermutations of(String s) {
        return new StringPermutations(s);
    }
}
