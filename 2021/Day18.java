import java.util.*;
import java.util.stream.*;
import java.nio.file.*;
import java.io.*;
import aoc.util.*;

public class Day18 {
    private static class Value {
        private Optional<Pair> p;
        private OptionalInt i;
        public Value(Optional<Pair> p, OptionalInt i) {
            this.p = p;
            this.i = i;
        }

        public Value(Pair p) {
            this.p = Optional.of(p);
            this.i = OptionalInt.empty();
        }

        public Value(int i) {
            this.p = Optional.empty();
            this.i = OptionalInt.of(i);
        }

        public Optional<Pair> p() { return p; }
        public OptionalInt i() { return i; }

        public static Value ofPair(Pair p) {
            return new Value(Optional.of(p), OptionalInt.empty());
        }
        public static Value ofInt(int i) {
            return new Value(Optional.empty(), OptionalInt.of(i));
        }

        public void setPair(Pair p) {
            this.p = Optional.of(p);
            this.i = OptionalInt.empty();
        }

        public void setInt(int i) {
            this.p = Optional.empty();
            this.i = OptionalInt.of(i);
        }

        public boolean isPair() { return p.isPresent(); }
        public Pair getPair() { return p.orElseThrow(); }
        public boolean isNumber() { return i.isPresent(); }
        public int getInt() { return i.orElseThrow(); }

        public int magnitude() {
            if (i.isPresent()) {
                return i.orElseThrow();
            } else {
                return p.orElseThrow().magnitude();
            }
        }
    }

    private static class Pair {
        private Value left;
        private Value right;
        private Pair parent;
        private int depth;

        Pair(Value left, Value right, int depth) {
            this.left = left;
            this.right = right;
            this.depth = depth;
            if (left.isPair()) { left.getPair().setParent(this); }
            if (right.isPair()) { right.getPair().setParent(this); }
        }

        public Pair parent() { return parent; }
        public void setParent(Pair p) { parent = p; }

        public Value left() { return left; }
        public Value right() { return right; }
        public void setLeft(Value v) { left = v; }
        public void setRight(Value v) { right = v; }

        public int depth() { return depth; }

        private Pair incrDepth() {
            Value newLeft = left.isPair()
                ? Value.ofPair(left.getPair().incrDepth())
                : Value.ofInt(left.getInt());
            Value newRight = right.isPair()
                ? Value.ofPair(right.getPair().incrDepth())
                : Value.ofInt(right.getInt());
            return new Pair(newLeft, newRight, depth + 1);
        }

        public static Pair add(Pair a, Pair b) {
            return new Pair(Value.ofPair(a.incrDepth()),
                            Value.ofPair(b.incrDepth()), 1).reduce();
        }

        private void populate(List<Pair> nodes, List<Value> numbers) {
            if (left.isPair()) {
                left.getPair().populate(nodes, numbers);
            } else {
                numbers.add(left);
            }
            if (left.isNumber() || right.isNumber()) {
                nodes.add(this);
            }
            if (right.isPair()) {
                right.getPair().populate(nodes, numbers);
            } else {
                numbers.add(right);
            }
        }

        private boolean split() {
            if (left.isNumber() && left.getInt() >= 10) {
                int n = left.getInt();
                Pair newPair = new Pair(Value.ofInt(n / 2),
                                        Value.ofInt((n + 1) / 2),
                                        depth + 1);
                newPair.setParent(this);
                left = Value.ofPair(newPair);
                return true;
            } else if (left.isPair() && left.getPair().split()) {
                return true;
            } else if (right.isNumber() &&
                right.getInt() >= 10) {
                int n = right.getInt();
                Pair newPair = new Pair(Value.ofInt(n / 2),
                                        Value.ofInt((n + 1) / 2),
                                        depth + 1);
                newPair.setParent(this);
                right = Value.ofPair(newPair);
                return true;
            } else if (right.isPair() && right.getPair().split()) {
                return true;
            } else {
                return false;
            }
        }

        private boolean explode() {
            List<Pair> nodes = new ArrayList<Pair>();
            List<Value> numbers = new ArrayList<Value>();
            populate(nodes, numbers);

            for (int n = 0; n < nodes.size(); n++) {
                Pair p = nodes.get(n);
                if (p.depth() >= 5 && p.left().isNumber() &&
                    p.right().isNumber()) {
                    Pair parent = p.parent();
                    if (parent.left().isPair() &&
                        parent.left().getPair() == p) {
                        parent.setLeft(Value.ofInt(0));
                    } else if (parent.right().isPair() &&
                               parent.right().getPair() == p) {
                        parent.setRight(Value.ofInt(0));
                    }
                    for (int m = 0; m < numbers.size(); m++) {
                        Value num = numbers.get(m);
                        if (num == p.left()) {
                            if (m > 0) {
                                numbers.get(m - 1)
                                    .setInt(numbers.get(m - 1).getInt()
                                            + p.left().getInt());
                            }
                        } else if (num == p.right()) {
                            if (m < numbers.size() - 1) {
                                numbers.get(m + 1)
                                    .setInt(numbers.get(m + 1).getInt()
                                            + p.right().getInt());
                            }
                        }
                    }
                    return true;
                }
            }
            return false;
        }

        private Pair reduce() {
            boolean changed = true;

            while (changed) {
                changed = explode();
                if (!changed) {
                    changed = split();
                }
            }

            return this;
        }

        public int magnitude() {
            return (left.magnitude() * 3) + (right.magnitude() * 2);
        }
    }

    private static Value parseValue(PushbackReader r, int depth)
        throws IOException {
        char ch = (char)r.read();
        if (ch == '[') {
            r.unread(ch);
            return Value.ofPair(parsePair(r, depth + 1));
        } else if (Character.isDigit(ch)) {
            int num = ch - '0';
            while (true) {
                ch = (char)r.read();
                if (Character.isDigit(ch)) {
                    num = (num * 10) + (ch - '0');
                } else {
                    r.unread(ch);
                    return Value.ofInt(num);
                }
            }
        } else {
            throw new IllegalStateException("Unexpected character "
                                            + ch + " (Wanted digit or [");
        }
    }

    private static Pair parsePair(PushbackReader r, int depth)
        throws IOException {
        char ch = (char)r.read();
        if (ch == '[') {
            Value left, right;
            left = parseValue(r, depth);
            ch = (char)r.read();
            if (ch != ',') {
                throw new IllegalStateException("Unexpected character "
                                                + ch + " (Wanted ,)");
            }
            right = parseValue(r, depth);
            ch = (char)r.read();
            if (ch != ']') {
                throw new IllegalStateException("Unexpected character "
                                               + ch + " (Wanted ])");
            }
            return new Pair(left, right, depth);
        } else {
            throw new IllegalStateException("Unexpected character "
                                            + ch + " (Wanted [)");
        }
    }

    private static Pair parsePair(String num) {
        try (var sr = new PushbackReader(new StringReader(num))) {
            return parsePair(sr, 1);
        } catch (IOException e) {
            System.err.println("In parsePair: " + e);
            System.exit(1);
        }
        return null;
    }

    public static void main(String[] args) {
        try (var lines = Files.lines(Path.of(args[0]))) {
            var input = lines.map(Day18::parsePair).toList();
            int mag = input.stream()
                .reduce(Pair::add)
                .orElseThrow()
                .magnitude();
            System.out.println("Part 1: " + mag);

            int maxMag = ComboStream.arrayStream(input, Pair[]::new, 2)
                .flatMap(p -> Stream.of(Pair.add(p[0], p[1]),
                                        Pair.add(p[1], p[0])))
                .mapToInt(Pair::magnitude)
                .max()
                .orElse(-1);
            System.out.println("Part 2: " + maxMag);
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
