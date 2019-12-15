#!/usr/bin/tclsh

package require {math::constants}
::math::constants::constants pi

proc pop {lst} {
    upvar $lst l
    set head [lindex $l 0]
    set l [lreplace $l 0 0]
    return $head
}

set grid [dict create]
set x -1
set y -1

while {[gets stdin line] >= 0} {
    set x -1
    incr y
    foreach point [split $line {}] {
        incr x
        if {$point eq "#"} {
            dict set grid [list $x $y] {}
        }
    }
}

dict for {coord _} $grid {
    dict for {other _} $grid {
        if {$coord eq $other} { continue }
        set rad [expr {atan2([lindex $other 1] - [lindex $coord 1],
                             [lindex $other 0] - [lindex $coord 0])}]
        if {$rad < ($pi / 2.0 * -1.0)} {
            set rad [expr {$pi + $pi + $rad}]
        }
        set key [expr {int($rad * 1000000)}]
        if {![dict exists $grid $coord $key]} {
            dict set grid $coord $key [list $other]
        } else {
            set tmp [dict get $grid $coord $key]
            set tmp [linsert $tmp end $other]
            dict set grid $coord $key $tmp
        }
    }
}

set station ""
set stationlen 0
dict for {coord keys} $grid {
    set len [dict size $keys]
    if {$station eq "" || $len > $stationlen} {        
        set station $coord
        set stationlen $len
    }
}

puts "Part 1: $stationlen"

for {set i 0} {$i < 200} {} {
    foreach key [lsort -integer [dict keys [dict get $grid $station]]] {
        if {[llength [dict get $grid $station $key]] == 0} { continue }
        incr i
        set targets [dict get $grid $station $key]
        set target [pop targets]
        dict set grid $station $key $targets
        if {$i == 200} {
            set part2 [expr {[lindex $target 0] * 100 + [lindex $target 1]}]
            break
        }
    }
}
puts "Part 2: $part2"
