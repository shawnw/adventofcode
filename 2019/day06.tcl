#!/usr/bin/tclsh

package require struct::graph
package require struct::graph::op

proc solve {orbitlist} {
    ::struct::graph orbs
    set san x
    set you x
    foreach orbit $orbitlist {
        lassign [split $orbit ")"] parent child
        if {![orbs node exists $parent]} {
            orbs node insert $parent
        }
        if {![orbs node exists $child]} {
            orbs node insert $child
        }
        if {$child == "SAN"} {
            set san $parent
        }
        if {$child == "YOU"} {
            set you $parent
        }
        orbs arc setweight [orbs arc insert $parent $child] 1
    }
    
    set lengths [::struct::graph::op::dijkstra orbs COM \
                     -arcmode directed -outputformat distances]
    set part1 0
    foreach len [dict values $lengths] {
        incr part1 $len
    }
    
    set part2 [::struct::graph::op::distance orbs $you $san]
    return [list $part1 $part2]
}

set orbitlist [split [read -nonewline stdin] \n]
lassign [solve $orbitlist] part1 part2
puts "Part 1: $part1"
puts "Part 2: $part2"
