package aoc.util;
import java.util.*;

public class OptHashMap<K,V> extends HashMap<K,V> {
    public OptHashMap() {
        super();
    }

    public OptHashMap(Map<? extends K,? extends V> m) {
        super(m);
    }

    /** Return an Optional containing the value associated with the given
        key, or an empty one if the key isn't present. */
    public Optional<V> getOptional(K key) {
        return containsKey(key) ? Optional.of(get(key)) : Optional.empty();
    }
}
