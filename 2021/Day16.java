import java.util.*;
import java.util.stream.*;
import java.nio.file.*;
import java.io.IOException;
import aoc.util.*;

public class Day16 {
    private enum Opcode {
        SUM, PRODUCT, MIN, MAX, NUMBER, GT, LT, EQ;
        public static Opcode of(int op) {
            return switch (op) {
            case 0 -> SUM;
            case 1 -> PRODUCT;
            case 2 -> MIN;
            case 3 -> MAX;
            case 4 -> NUMBER;
            case 5 -> GT;
            case 6 -> LT;
            case 7 -> EQ;
            default ->
                throw new IllegalArgumentException("Opcode "
                                                   + op + " out of range");
            };
        }
    }

    private static record Packet(int version, Opcode type, OptionalLong value,
                                 Optional<List<Packet>> subPackets) {
        public int sumVersions() {
            if (value.isPresent()) {
                return version;
            } else {
                return version + subPackets.get().stream()
                    .collect(Collectors.summingInt(Packet::sumVersions));
            }
        }

        public long compute() {
            return
            switch (type) {
            case SUM ->
                subPackets.get().stream()
                .collect(Collectors.summingLong(Packet::compute));
            case PRODUCT ->
                subPackets.get().stream()
                .mapToLong(Packet::compute)
                .reduce(1L, (a,b) -> a * b);
            case MIN ->
                subPackets.get().stream()
                .mapToLong(Packet::compute)
                .min()
                .getAsLong();
            case MAX ->
                subPackets.get().stream()
                .mapToLong(Packet::compute)
                .max()
                .getAsLong();
            case NUMBER -> value.getAsLong();
            case GT ->
                {
                    var p = subPackets.get();
                    yield (p.get(0).compute() > p.get(1).compute()) ? 1 : 0;
                }
            case LT ->
                {
                    var p = subPackets.get();
                    yield (p.get(0).compute() < p.get(1).compute()) ? 1 : 0;
                }
            case EQ ->
                {
                    var p = subPackets.get();
                    yield (p.get(0).compute() == p.get(1).compute()) ? 1 : 0;
                }
            };
        }

        private void toSexpInternal(StringBuilder sb) {
            if (type == Opcode.NUMBER) {
                sb.append(' ');
                sb.append(value.getAsLong());
            } else {
                boolean isComp = (type == Opcode.LT || type == Opcode.GT
                                  || type == Opcode.EQ);
                if (isComp) {
                    sb.append(" (if");
                }
                sb.append(" (");
                sb.append(
                          switch (type) {
                          case SUM -> "+";
                          case PRODUCT -> "*";
                          case MIN -> "min";
                          case MAX -> "max";
                          case LT -> "<";
                          case GT -> ">";
                          case EQ -> "=";
                          case NUMBER -> "string";
                          });
                subPackets.get().forEach(p -> p.toSexpInternal(sb));
                sb.append(')');
                if (isComp) {
                    sb.append(" 1 0)");
                }
            }
        }
        public String toSexp() {
            var sb = new StringBuilder();
            toSexpInternal(sb);
            return sb.toString();
        }
    }

    private static int readNBits(ArrayDeque<Character> s, int bitCount) {
        int val = 0;
        for (int n = 0; n < bitCount; n++) {
            if (s.removeFirst() == '1') {
                val = (val << 1) | 1;
            } else {
                val <<= 1;
            }
        }
        return val;
    }

    private static int readPacketVersion(ArrayDeque<Character> s) {
        return readNBits(s, 3);
    }

    private static int readPacketTypeID(ArrayDeque<Character> s) {
        return readNBits(s, 3);
    }

    private static long readLiteral(ArrayDeque<Character> s) {
        // Type ID 4
        long val = 0;
        while (true) {
            int chunk = readNBits(s, 5);
            val = (val << 4) | (chunk & 0b1111);
            if ((chunk & 0b10000) == 0) {
                break;
            }
        }
        return val;
    }

    private static String hexToBinary(String hexStr) {
        return hexStr.chars().mapToObj(ch -> switch ((char)ch) {
            case '0' -> "0000";
            case '1' -> "0001";
            case '2' -> "0010";
            case '3' -> "0011";
            case '4' -> "0100";
            case '5' -> "0101";
            case '6' -> "0110";
            case '7' -> "0111";
            case '8' -> "1000";
            case '9' -> "1001";
            case 'A' -> "1010";
            case 'B' -> "1011";
            case 'C' -> "1100";
            case 'D' -> "1101";
            case 'E' -> "1110";
            case 'F' -> "1111";
            default -> throw new NumberFormatException("Non-hex character "
                                                       + (char)ch);
            })
            .collect(Collectors.joining());
    }

    private static Packet parseBinary(ArrayDeque<Character> s) {
        int version = readPacketVersion(s);
        int typeId = readPacketTypeID(s);
        if (typeId == 4) {
            // Literal value
            long val = readLiteral(s);
            return new Packet(version, Opcode.NUMBER, OptionalLong.of(val),
                              Optional.empty());
        } else {
            char bit = s.removeFirst(); // length type ID
            var packets = new ArrayList<Packet>();
            if (bit == '1') {
                int packetCount = readNBits(s, 11);
                packets.ensureCapacity(packetCount);
                for (int n = 0; n < packetCount; n++) {
                    packets.add(parseBinary(s));
                }
            } else {
                int bitLen = readNBits(s, 15);
                int targetBits = s.size() - bitLen;
                while (s.size() != targetBits) {
                    packets.add(parseBinary(s));
                }
            }
            return new Packet(version, Opcode.of(typeId), OptionalLong.empty(),
                              Optional.of(packets));
        }
    }

    private static Packet parse(String rawPacket) {
        return parseBinary(new ArrayDeque<Character>(Strings.explode(hexToBinary(rawPacket))));
    }

    public static void main(String[] args) {
        try {
            for (var line : Files.readAllLines(Path.of(args[0]))) {
                var packet = parse(line);
                System.out.println("Packet: " + line);
                System.out.println("Part 1: " + packet.sumVersions());
                System.out.println("Part 2: " + packet.compute());
                System.out.println("Scheme expression: " + packet.toSexp());
            }
        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
