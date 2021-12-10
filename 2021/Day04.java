import java.util.*;
import java.nio.file.*;
import java.io.*;
import aoc.util.*;

class Day04 {

    private static class Board {
        private record Square(int num, boolean marked) {}

        Square board[][];
        public Board(Scanner s) {
            board = new Square[5][5];
            for (int row = 0; row < 5; row++) {
                for (int col = 0; col < 5; col++) {
                    board[row][col] = new Square(s.nextInt(), false);
                }
            }
        }

        public boolean mark(int num) {
            for (int row = 0; row < 5; row++) {
                for (int col = 0; col < 5; col++) {
                    if (board[row][col].num() == num) {
                        board[row][col] = new Square(num, true);
                        return true;
                    }
                }
            }
            return false;
        }

        public boolean isWinner() {
            // Row
            if (Arrays.stream(board)
                      .anyMatch(row -> Arrays.stream(row).allMatch(sq -> sq.marked()))) {
                return true;
            }

            // Column
            for (int col = 0; col < 5; col++) {
                boolean all = true;
                for (int row = 0; row < 5; row++) {
                    if (!board[row][col].marked()) {
                        all = false;
                        break;
                    }
                }
                if (all) {
                    return true;
                }
            }
            return false;
        }

        public int score() {
            return Arrays.stream(board)
                         .flatMap(Arrays::stream)
                         .filter(sq -> !sq.marked())
                         .mapToInt(Square::num)
                         .sum();
        }
    }

    public static void main(String[] args) {
        try {
            Scanner s = new Scanner(Path.of(args[0]));
            var numbers = Strings.csvOfString(s.nextLine());

            var boards = new LinkedList<Board>();
            while (s.hasNextInt()) {
                boards.add(new Board(s));
            }

            boolean first_one = true;
            Board last_winner = null;
            int last_num = -1;
            var winners = new LinkedList<Board>();

            for (int num : numbers) {
                for (Board b : boards) {
                    if (b.mark(num) && b.isWinner()) {
                        if (first_one) {
                            System.out.println("Part 1: " + (b.score() * num));
                            first_one = false;
                        }
                        winners.add(b);
                    }
                }
                if (!winners.isEmpty()) {
                    boards.removeAll(winners);
                    last_winner = winners.getLast();
                    last_num = num;
                    winners.clear();
                }
            }
            System.out.println("Part 2: " + (last_winner.score() * last_num));

        } catch (IOException e) {
            System.err.println(e);
            System.exit(1);
        }
    }
}
