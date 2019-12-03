#!/usr/bin/tclsh

package require sqlite3

sqlite3 wires ":memory:"
wires eval {CREATE TABLE IF NOT EXISTS grid(id INTEGER, x INTEGER, y INTEGER, step INTEGER)}

proc add_wire {input id} {
    set x 0
    set y 0
    set step -1
    foreach move [split $input ","] {
        regexp {^(.)([0-9]+)} $move all dir dist
        switch $dir {
            U {
                for {set newx [expr {$x + $dist}]} {$x < $newx} {incr x} {
                    incr step
                    wires eval {INSERT INTO grid VALUES ($id, $x, $y, $step)}
                }
            }
            D {
                for {set newx [expr {$x - $dist}]} {$x > $newx} {incr x -1} {
                    incr step
                    wires eval {INSERT INTO grid VALUES ($id, $x, $y, $step)}
                }
            }
            L {
                for {set newy [expr {$y - $dist}]} {$y > $newy} {incr y -1} {
                    incr step
                    wires eval {INSERT INTO grid VALUES ($id, $x, $y, $step)}
                }
            }
            R {
                for {set newy [expr {$y + $dist}]} {$y < $newy} {incr y} {
                    incr step
                    wires eval {INSERT INTO grid VALUES ($id, $x, $y, $step)}
                }
            }
            default {
                puts "Uknown direction $dir in $move"
                exit 1
            }
        }
    }
}

proc add_wires {wire1 wire2} {
    wires transaction {
        wires eval {DROP INDEX IF EXISTS grid_idx}
        wires eval {DELETE FROM grid}
        add_wire $wire1 0
        add_wire $wire2 1
        wires eval {CREATE INDEX grid_idx ON grid(x, y)}
    }
}

proc solve {wire1 wire2} {
    add_wires $wire1 $wire2
    return [wires eval {    
        SELECT min(abs(w1.x) + abs(w1.y)) AS distance,
               min(w1.step + w2.step) AS steps
        FROM grid AS w1
        JOIN grid AS w2 ON w1.id <> w2.id AND (w1.x, w1.y) = (w2.x, w2.y)
        WHERE (w1.x, w1.y) <> (0, 0) AND w1.id = 0
    }]
}

proc run_test {testno wire1 wire2 expected_dist expected_steps} {
    puts -nonewline "Test $testno: $wire1 and $wire2:"
    lassign [solve $wire1 $wire2] dist steps
    if {$dist == $expected_dist && $steps == $expected_steps} {
        puts " passed."
    } elseif {$dist != $expected_dist} {
        puts " failed! Distance $dist, expected $expected_dist"
        exit 1
    } elseif {$steps != $expected_steps} {
        puts " failed! Steps $steps, expected $expected_steps"
        exit 1
    }
}

run_test 1 "R8,U5,L5,D3" "U7,R6,D4,L4" 6 30
run_test 2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" 159 610
run_test 3 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" \
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" 135 410

gets stdin wire1
gets stdin wire2
lassign [solve $wire1 $wire2] part1 part2
puts "Part 1: $part1"
puts "Part 2: $part2"
