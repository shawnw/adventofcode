#!/usr/bin/tclsh

package require math::numtheory
namespace import ::math::numtheory::lcm

proc new_moon {data} {
    if {[scan $data "<x=%d, y=%d, z=%d>" x y z] != 3} {
        error "Invalid line $data"
    } else {
        return [dict create x $x y $y z $z vx 0 vy 0 vz 0]
    }
}

proc foreach_idx {var idx lst bdy} {
    upvar $var v $idx i
    set i 0
    foreach v $lst {
        switch [catch { uplevel 1 $bdy } status ] {
            0 { incr i }
            1 { error $status }
            2 { return $status }
            3 { break }
            4 { incr i }
        }
    }
}

proc identity {val} {
    return $val
}

proc simulate {moons steps} {
    set nmoons [llength $moons]
    for {set i 0} {$i < $steps} {incr i} {
        for {set m1 0} {$m1 < $nmoons} {incr m1} {
            set moon1 [lindex $moons $m1]
            for {set m2 0} {$m2 < $nmoons} {incr m2} {
                if {$m1 == $m2} { continue }
                set moon2 [lindex $moons $m2]
                if {[dict get $moon1 x] > [dict get $moon2 x]} {
                    dict incr moon1 vx -1
                } elseif {[dict get $moon1 x] < [dict get $moon2 x]} {
                    dict incr moon1 vx 
                }
                if {[dict get $moon1 y] > [dict get $moon2 y]} {
                    dict incr moon1 vy -1
                } elseif {[dict get $moon1 y] < [dict get $moon2 y]} {
                    dict incr moon1 vy
                }
                if {[dict get $moon1 z] > [dict get $moon2 z]} {
                    dict incr moon1 vz -1
                } elseif {[dict get $moon1 z] < [dict get $moon2 z]} {
                    dict incr moon1 vz
                }
            }
            lset moons $m1 $moon1
        }
        set moons [lmap moon $moons {
            dict update moon x x vx vx y y vy vy z z vz vz {
                set x [expr {$x + $vx}]
                set y [expr {$y + $vy}]
                set z [expr {$z + $vz}]
            }
            identity $moon
        }]
    }
    set total 0
    foreach moon $moons {
        lassign [dict values [dict filter $moon key "\[xyz\]"]] x y z
        lassign [dict values [dict filter $moon key "v\[xyz\]"]] vx vy vz
        incr total [expr {(abs($x) + abs($y) + abs($z)) *
                          (abs($vx) + abs($vy) + abs($vz))}]
    }
    return $total
}

proc find_cycle {moons} {
    set nmoons [llength $moons]
    set found 0
    set counts {0 0 0}
    
    for {set i 1} {1} {incr i} {
        for {set m1 0} {$m1 < $nmoons} {incr m1} {
            set moon1 [lindex $moons $m1]
            for {set m2 0} {$m2 < $nmoons} {incr m2} {
                if {$m1 == $m2} { continue }
                set moon2 [lindex $moons $m2]
                if {[dict get $moon1 x] > [dict get $moon2 x]} {
                    dict incr moon1 vx -1
                } elseif {[dict get $moon1 x] < [dict get $moon2 x]} {
                    dict incr moon1 vx
                }
                if {[dict get $moon1 y] > [dict get $moon2 y]} {
                    dict incr moon1 vy -1
                } elseif {[dict get $moon1 y] < [dict get $moon2 y]} {
                    dict incr moon1 vy
                }
                if {[dict get $moon1 z] > [dict get $moon2 z]} {
                    dict incr moon1 vz -1
                } elseif {[dict get $moon1 z] < [dict get $moon2 z]} {
                    dict incr moon1 vz
                }
            }
            lset moons $m1 $moon1
        }
        set xzeros 0
        set yzeros 0
        set zzeros 0
        for {set m 0} {$m < $nmoons} {incr m} {
            set moon [lindex $moons $m]
            dict update moon x x vx vx y y vy vy z z vz vz {
                set x [expr {$x + $vx}]
                set y [expr {$y + $vy}]
                set z [expr {$z + $vz}]
                if {$vx == 0} { incr xzeros }
                if {$vy == 0} { incr yzeros }
                if {$vz == 0} { incr zzeros }
            }
            lset moons $m $moon
        }
        if {$xzeros == $nmoons && [lindex $counts 0] == 0} {
            incr found
            lset counts 0 $i
        }
        if {$yzeros == $nmoons && [lindex $counts 1] == 0} {
            incr found
            lset counts 1 $i
        }
        if {$zzeros == $nmoons && [lindex $counts 2] == 0} {
            incr found
            lset counts 2 $i
        }
        if {$found == 3} {
            set steps [lcm [lindex $counts 0] [lindex $counts 1]]
            set steps [lcm $steps [lindex $counts 2]]
            return [expr {$steps * 2}]
        }
    }
}

set testno 1
proc test {positions steps expected1 expected2} {
    global testno
    set moons [lmap moon $positions { new_moon $moon }]
    set total [simulate $moons $steps]
    if {$total == $expected1} {
        puts "Test $testno.1: Passed."
    } else {
        puts "Test $testno.1: Failed. Got $total, expected $expected1."
    }

    set cycle [find_cycle $moons]
    if {$cycle == $expected2} {
        puts "Test $testno.2: Passed."
    } else {
        puts "Test $testno.2: Failed. Got $cycle, expected $expected2."
    }
    incr testno
}

test [list "<x=-1, y=0, z=2>" \
          "<x=2, y=-10, z=-7>" \
          "<x=4, y=-8, z=8>" \
          "<x=3, y=5, z=-1>"] 10 179 2772
test [list "<x=-8, y=-10, z=0>" \
          "<x=5, y=5, z=10>" \
          "<x=2, y=-7, z=3>" \
          "<x=9, y=-8, z=-3>"] 100 1940 4686774924
          
set moons [lmap line [split [read -nonewline stdin] "\n"] { new_moon $line }]
puts "Part 1: [simulate $moons 1000]"
puts "Part 2: [find_cycle $moons]"
