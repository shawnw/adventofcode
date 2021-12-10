package aoc.util;
import java.util.*;

public class Strings {
    static public List<Character> explode(String s) {
        return s.chars().mapToObj(c -> Character.valueOf((char)c)).toList();
    }

    static public String combine(List<Character> chars) {
        StringBuilder sb = new StringBuilder(chars.size());
        for (char c : chars) {
            sb.append(c);
        }
        return sb.toString();
    }

    static public int[] csvOfString(String s) {
        return Arrays.stream(s.split(",")).mapToInt(Integer::parseInt).toArray();
    }
}
