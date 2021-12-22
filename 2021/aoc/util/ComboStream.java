package aoc.util;
import org.apache.commons.math3.util.CombinatoricsUtils;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.IntFunction;
import java.util.stream.*;

// Builds on Apache Commons math3 to stream over all combinations of a
// list

public class ComboStream {
    public static class ComboListSpliterator<T>
        implements Spliterator<List<T>> {
        private List<T> elems;
        private int r, n, flags;
        private Iterator<int[]> iter;
        private long nCr;

        public ComboListSpliterator(List<T> elems, int r) {
            this.flags = NONNULL;
            if (elems instanceof RandomAccess) {
                this.elems = elems;
            } else {
                this.elems = new ArrayList<T>(elems);
                this.flags |= IMMUTABLE;
            }
            this.r = r;
            this.n = this.elems.size();
            this.iter =
                CombinatoricsUtils.combinationsIterator(n, r);
            try {
                nCr = CombinatoricsUtils.binomialCoefficient(n, r);
                flags |= SIZED | SUBSIZED;
            } catch (ArithmeticException e) {
                nCr = Long.MAX_VALUE;
            }
        }

        public ComboListSpliterator(Collection<T> elems, int r) {
            this(new ArrayList<T>(elems), r);
            flags |= IMMUTABLE;
        }

        public ComboListSpliterator(T[] elems, int r) {
            this(Arrays.stream(elems)
                 .collect(Collectors.toCollection(ArrayList<T>::new)),
                 r);
            flags |= IMMUTABLE;
        }

        @Override
        public int characteristics() { return flags; }

        @Override
        public long estimateSize() { return nCr; }

        @Override
        public void forEachRemaining(Consumer<? super List<T>> f) {
            if (elems.size() != n) {
                throw new ConcurrentModificationException();
            }
            var combo = new ArrayList<T>(r);
            for (int i = 0; i < r; i++) {
                combo.add(null);
            }
            while (iter.hasNext()) {
                int[] indexes = iter.next();
                for (int i = 0; i < r; i++) {
                    combo.set(i, elems.get(indexes[i]));
                }
                f.accept(Collections.unmodifiableList(combo));
            }
        }

        @Override
        public boolean tryAdvance(Consumer<? super List<T>> f) {
            if (elems.size() != n) {
                throw new ConcurrentModificationException();
            }
            if (!iter.hasNext()) {
                return false;
            }
            int[] indexes = iter.next();
            List<T> combo = new ArrayList<T>(r);
            for (int i : indexes) {
                combo.add(elems.get(i));
            }
            f.accept(Collections.unmodifiableList(combo));
            return true;
        }

        @Override
        public Spliterator<List<T>> trySplit() { return null; }
    }

    // Return streams of every r-sized combination of elements from
    // the source, provided as Lists.

    public static <T> Stream<List<T>> listStream(List<T> elems, int r) {
        return
            StreamSupport.stream(new ComboListSpliterator<T>(elems, r),
                                 false);
    }

    public static <T> Stream<List<T>> listStream(Collection<T> elems, int r) {
        return
            StreamSupport.stream(new ComboListSpliterator<T>(elems, r),
                                 false);
    }

    public static <T> Stream<List<T>> listStream(T[] elems, int r) {
        return
            StreamSupport.stream(new ComboListSpliterator<T>(elems, r),
                                 false);
    }

    public static class ComboArraySpliterator<T> implements Spliterator<T[]> {
        private T[] elems;
        private int r, flags;
        private Iterator<int[]> iter;
        private long nCr;

        public ComboArraySpliterator(T[] elems, int r) {
            this.elems = elems;
            this.r = r;
            this.flags = NONNULL;
            this.iter =
                CombinatoricsUtils.combinationsIterator(elems.length, r);
            try {
                nCr = CombinatoricsUtils.binomialCoefficient(elems.length, r);
                flags |= SIZED | SUBSIZED;
            } catch (ArithmeticException e) {
                nCr = Long.MAX_VALUE;
            }
        }

        public ComboArraySpliterator(Collection<T> elems, T[] dummy, int r) {
            this(elems.toArray(dummy), r);
            flags |= IMMUTABLE;
        }

        public ComboArraySpliterator(Collection<T> elems,
                                     IntFunction<T[]> generator, int r) {
            this(elems.toArray(generator), r);
            flags |= IMMUTABLE;
        }

        @Override
        public int characteristics() { return flags; }

        @Override
        public long estimateSize() { return nCr; }

        @Override
        public void forEachRemaining(Consumer<? super T[]> f) {
            T[] combo = Arrays.copyOf(elems, r);
            while (iter.hasNext()) {
                int[] indexes = iter.next();
                for (int i = 0; i < r; i++) {
                    combo[i] = elems[indexes[i]];
                }
                f.accept(combo);
            }
        }

        @Override
        public boolean tryAdvance(Consumer<? super T[]> f) {
            if (!iter.hasNext()) {
                return false;
            }
            int[] indexes = iter.next();
            T[] combo = Arrays.copyOf(elems, r);
            for (int i = 0; i < r; i++) {
                combo[i] = elems[indexes[i]];
            }
            f.accept(combo);
            return true;
        }

        @Override
        public Spliterator<T[]> trySplit() { return null; }
    }

    // Return streams of every r-sized combination of elements from
    // the source, provided as arrays.

    public static <T> Stream<T[]> arrayStream(Collection<T> elems,
                                                 IntFunction<T[]> generator,
                                                 int r) {
        return
            StreamSupport.stream(new ComboArraySpliterator<T>(elems, generator,
                                                              r),
                                 false);
    }

    public static <T> Stream<T[]> arrayStream(Collection<T> elems, T[] dummy,
                                              int r) {
        return
            StreamSupport.stream(new ComboArraySpliterator<T>(elems, dummy, r),
                                 false);
    }

    public static <T> Stream<T[]> arrayStream(T[] elems, int r) {
        return StreamSupport.stream(new ComboArraySpliterator<T>(elems, r),
                                    false);
    }

    public static class AllCombosListSpliterator<T>
        implements Spliterator<List<T>> {
        private List<T> elems;
        private int r, n, flags;
        private Iterator<int[]> iter;
        private long nCr;

        public AllCombosListSpliterator(List<T> elems) {
            this.flags = NONNULL;
            if (elems instanceof RandomAccess) {
                this.elems = elems;
            } else {
                this.elems = new ArrayList<T>(elems);
                this.flags |= IMMUTABLE;
            }
            this.r = 0;
            this.n = this.elems.size();
            try {
                nCr = 0;
                for (int k = 1; k < n; k++) {
                    nCr =
                        Math.addExact(nCr,
                                      CombinatoricsUtils.binomialCoefficient(n,k));
                }
                flags |= SIZED | SUBSIZED;
            } catch (ArithmeticException e) {
                nCr = Long.MAX_VALUE;
            }
        }

        public AllCombosListSpliterator(Collection<T> elems) {
            this(new ArrayList<T>(elems));
            flags |= IMMUTABLE;
        }

        public AllCombosListSpliterator(T[] elems) {
            this(Arrays.stream(elems)
                 .collect(Collectors.toCollection(ArrayList<T>::new)));
            flags |= IMMUTABLE;
        }

        @Override
        public int characteristics() { return flags; }

        @Override
        public long estimateSize() { return nCr; }

        @Override
        public void forEachRemaining(Consumer<? super List<T>> f) {
            if (elems.size() != n) {
                throw new ConcurrentModificationException();
            }
            var combo = new ArrayList<T>();
            if (iter != null) {
                combo.ensureCapacity(r);
                for (int i = 0; i < r; i++) {
                    combo.add(null);
                }
                while (iter.hasNext()) {
                    int[] indexes = iter.next();
                    for (int i = 0; i < indexes.length; i++) {
                        combo.set(i, elems.get(indexes[i]));
                    }
                    f.accept(Collections.unmodifiableList(combo));
                }
            }
            while (++r <= elems.size()) {
                iter =
                    CombinatoricsUtils.combinationsIterator(n, r);
                for (int i = combo.size(); i < r; i++) {
                    combo.add(null);
                }
                while (iter.hasNext()) {
                    int[] indexes = iter.next();
                    for (int i = 0; i < indexes.length; i++) {
                        combo.set(i, elems.get(indexes[i]));
                    }
                    f.accept(Collections.unmodifiableList(combo));
                }
            }
        }

        @Override
        public boolean tryAdvance(Consumer<? super List<T>> f) {
            if (elems.size() != n) {
                throw new ConcurrentModificationException();
            }
            if (iter != null && iter.hasNext()) {
                int[] indexes = iter.next();
                List<T> combo = new ArrayList<T>(indexes.length);
                for (int i : indexes) {
                    combo.add(elems.get(i));
                }
                f.accept(Collections.unmodifiableList(combo));
                return true;
            }
            if (++r > elems.size()) {
                return false;
            }
            iter =
                CombinatoricsUtils.combinationsIterator(n, r);
            return tryAdvance(f);
        }

        @Override
        public Spliterator<List<T>> trySplit() {
            if (++r > elems.size()) {
                return null;
            }
            return new ComboListSpliterator<T>(elems, r);
        }
    }


    // Return a stream of every possible combination of elements of
    // the source, provided as Lists.

    public static <T> Stream<List<T>> listStreamAll(List<T> elems) {
        return
            StreamSupport.stream(new AllCombosListSpliterator<T>(elems),
                                 false);
    }

    public static <T> Stream<List<T>> parallelListStreamAll(List<T> elems) {
        return
            StreamSupport.stream(new AllCombosListSpliterator<T>(elems),
                                 true);
    }

    public static <T> Stream<List<T>> listStreamAll(Collection<T> elems) {
        return
            StreamSupport.stream(new AllCombosListSpliterator<T>(elems),
                                 false);
    }

    public static <T> Stream<List<T>> parallelListStreamAll(Collection<T> elems) {
        return
            StreamSupport.stream(new AllCombosListSpliterator<T>(elems),
                                 true);
    }

    public static <T> Stream<List<T>> listStreamAll(T[] elems) {
        return
            StreamSupport.stream(new AllCombosListSpliterator<T>(elems),
                                 false);
    }

    public static <T> Stream<List<T>> parallelListStreamAll(T[] elems) {
        return
            StreamSupport.stream(new AllCombosListSpliterator<T>(elems),
                                 true);
    }

}
