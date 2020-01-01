#!/usr/bin/tclsh

package require math::complexnumbers
namespace import ::math::complexnumbers::complex ::math::complexnumbers::real \
    ::math::complexnumbers::imag
namespace path {::tcl::mathfunc}

source intcode.tcl


proc solve {prog input} {
    namespace import ::math::complexnumbers::+
    set location [complex 0 0]
    set grid [dict create $location $input]
    set direction up
    
    coroutine Robot ::intcode::run $prog

    while {1} {
        lassign [Robot $input] state output
        set input {}
        switch $state {
            starting {}
            input {
                if {[dict exists $grid $location]} {
                    set input [dict get $grid $location]
                } else {
                    dict set $grid $location 0
                    set input 0
                }
            }
            halted {
                break
            }
            output {
                dict set grid $location $output
                lassign [Robot] state turn
                if {$state ne "output"} {
                    error "Expected more output, got $output"
                }
                if {$turn == 0} {
                    switch $direction {
                        up {
                            set direction left
                            set location [+ $location [complex -1 0]]
                        }
                        left {
                            set direction down
                            set location [+ $location [complex 0 -1]]
                        }
                        down {
                            set direction right
                            set location [+ $location [complex 1 0]]
                        }
                        right {
                            set direction up
                            set location [+ $location [complex 0 1]]
                        }
                    }
                } else {
                    switch $direction {
                        up {
                            set direction right
                            set location [+ $location [complex 1 0]]
                        }
                        right {
                            set direction down
                            set location [+ $location [complex 0 -1]]
                        }
                        down {
                            set direction left
                            set location [+ $location [complex -1 0]]
                        }
                        left {
                            set direction up
                            set location [+ $location [complex 0 1]]
                        }
                    }
                }                
            }
        }
    }
    return $grid
}

set prog [::intcode::compile [read -nonewline stdin]]

puts "Part 1: [dict size [solve $prog 0]]"

set part2 [solve $prog 1]

set minx ""
set maxx ""
set miny ""
set maxy ""
dict for {loc color} $part2 {
    if {$minx eq ""} {
        set minx [real $loc]
        set maxx [real $loc]
        set miny [imag $loc]
        set maxy [imag $loc]
    } else {
        set minx [min $minx [real $loc]]
        set maxx [max $maxx [real $loc]]
        set miny [min $miny [imag $loc]]
        set maxy [max $maxy [imag $loc]]
    }
}

puts "Part 2:"
for {set y $maxy} {$y >= $miny} {incr y -1} {
    for {set x $minx} {$x <= $maxx} {incr x} {
        set loc [complex $x $y]
        if {[dict exists $part2 $loc]} {
            switch [dict get $part2 $loc] {
                1 { puts -nonewline "#" }
                0 { puts -nonewline " " }
            }
        } else {
            puts -nonewline " "
        }
    }
    puts ""
}



