#!/usr/bin/tclsh

proc calc_fuel {mass} { return [expr {$mass / 3 - 2 }] }

set total1 0
set total2 0

while {[gets stdin mass] >= 0} {
    set fuel [calc_fuel $mass]
    incr total1 $fuel
    for {set totalfuel 0} {$fuel > 0} {set fuel [calc_fuel $fuel]} {
        incr totalfuel $fuel
    }
    incr total2 $totalfuel
}

puts "Part 1: $total1"
puts "Part 2: $total2"
