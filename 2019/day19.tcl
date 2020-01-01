#!/usr/bin/tclsh

source intcode.tcl

proc play {program} {
    set part1 0
    set grid [dict create]
    puts "Scanning..."
    for {set y 0} {$y < 5000} {incr y} {
        set past 0
        for {set x 0} {$past == 0 && $x < 5000} {incr x} {
            coroutine Drone ::intcode::run $program [list $x $y]
            lassign [Drone] state affect
            if {$state ne "output"} {
                error "Inconsistent state: $state, should be output"
            }
            if {[Drone] ne "halted"} {
                error "Inconsistent state $state, should be halted"
            }
            if {$affect == 1 && $x < 50 && $y < 50} {
                incr part1
            }            
            dict set grid $x $y $affect
            if {$affect == 0 && $x > 0} {
                if {[dict get $grid [expr {$x - 1}] $y] == 1} {
                    set past 1
                }
            }
            if {$affect == 1 && $x >= 99 && $y >= 99} {
                set failed 0
                for {set tx [expr {$x - 99}]} {$failed == 0 && $tx <= $x} \
                    {incr tx} {
                        for {set ty [expr {$y - 99}]} {$ty <= $y} {incr ty} {
                            if {[dict exists $grid $tx $ty] && [dict get $grid $tx $ty] == 0} {
                                set failed 1
                                break
                            }
                        }
                    }
                if {$failed == 0} {
                    set cx [expr {$x - 99}]
                    set cy [expr {$y - 99}]
                    return [list $part1 [expr {$cx * 10000 + $cy}]]
                }
            }
        }
    }
}

set program [::intcode::compile [read -nonewline stdin]]
lassign [play $program] part1 part2
puts "Part 1: $part1"
puts "Part 2: $part2"

