#!/usr/bin/tclsh

package require struct::graph
package require struct::graph::op

proc solve {orbitlist} {
    ::struct::graph orbs
    foreach orbit $orbitlist {
        lassign [split $orbit ")"] parent child
        if {![orbs node exists $parent]} {
            orbs node insert $parent
        }
        if {![orbs node exists $child]} {
            orbs node insert $child
        }
       orbs arc setweight [orbs arc insert $parent $child] 1
    }
    
    set lengths [::struct::graph::op::dijkstra orbs COM \
                     -arcmode directed -outputformat distances]
    set part1 0
    foreach len [dict values $lengths] {
        incr part1 $len
    }
    
    set part2 [expr {[::struct::graph::op::distance orbs YOU SAN] - 2}]
    return [list $part1 $part2]
}

lassign [solve [read -nonewline stdin]] part1 part2
puts "Part 1: $part1"
puts "Part 2: $part2"
