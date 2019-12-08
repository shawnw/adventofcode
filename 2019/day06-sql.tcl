#!/usr/bin/tclsh

package require sqlite3

sqlite3 orbits ":memory:"
orbits eval {CREATE TABLE IF NOT EXISTS orbits(parent TEXT, child TEXT)}
#orbits eval {DELETE FROM orbits}

# Populate the database
orbits transaction {
    orbits eval {DROP INDEX IF EXISTS orbits_idx}
    foreach orbit [read -nonewline stdin] {
        lassign [split $orbit ")"] parent child
        orbits eval {INSERT INTO orbits VALUES ($parent, $child)}
    }
    orbits eval {CREATE INDEX orbits_idx ON orbits(child)}
}

set part1 [orbits eval {
    WITH cte AS
    (SELECT child, 1 AS len FROM orbits WHERE parent = 'COM'
     UNION ALL
     SELECT o.child, len + 1
     FROM orbits AS o
     JOIN cte ON o.parent = cte.child)
    SELECT sum(len) FROM cte;
}]

set part2 [orbits eval {
    WITH you AS
    (SELECT parent, 0 AS len FROM orbits WHERE child = 'YOU'
     UNION ALL
     SELECT o.parent, len + 1
     FROM orbits AS o
     JOIN you ON o.child = you.parent),
    san AS
    (SELECT parent, 0 AS len FROM orbits WHERE child = 'SAN'
     UNION ALL
     SELECT o.parent, len + 1
     FROM orbits AS o
     JOIN san ON o.child = san.parent)
    SELECT min(you.len + san.len)
    FROM you
    JOIN san ON you.parent = san.parent
}]

puts "Part 1: $part1"
puts "Part 2: $part2"
