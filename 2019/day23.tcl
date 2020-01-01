#!/usr/bin/tclsh

source intcode.tcl

proc allempty {d} {
    dict for {comp queue} $d {
        if {[llength $queue] > 0} {
            return 0
        }
    }
    return 1
}

proc play {program} {
    set computers [lrepeat 50 -1]
    set queues [dict create]
    set states [lrepeat 50 -1]
    set nat_x -1
    set nat_y -1
    set prev_natx -2
    set prev_naty -2
    set first_y 1
    
    for {set i 0} {$i < 50} {incr i} {
        set name Computer$i
        lset states $i [coroutine $name ::intcode::run $program [list $i -1]]
        lset computers $i $name
        dict set queues $i {}
    }

    while 1 {
        if {[lsearch -not -exact $states input] >= 0 && [allempty $queues]} {
            dict lappend queues 0 [list $nat_x $nat_y]
            if {$nat_y == $prev_naty} {
                return $nat_y
            }
            set prev_natx $nat_x
            set prev_naty $nat_y
            set nat_x -1
            set nat_y -1
        }
        foreach_idx comp i $computers {
            if {$comp == -1} {
                continue
            }
            if {[lindex $states $i] eq "input"} {
                set queue [dict get $queues $i]
                if {[llength $queue] > 0} {
                    set queue [lassign $queue input]
                    dict set queues $i $queue
                } else {
                    continue
                }
            } else {
                set input {}
            }
            lassign [$comp $input] state address
            lset states $i $state
            switch $state {
                starting {}
                halted {
                    puts "Computer $i halted"
                    lset computers $i -1
                }
                output {
                    lassign [$comp] state x
                    if {$state ne "output"} {
                        error "Invalid state $state, expecting output x"
                    }
                    lassign [$comp] state y
                    if {$state ne "output"} {
                        error "invalid state $state, expecting output y"
                    }
                    if {$address == 255} {
                        if {$first_y} {
                            set first_y 0
                            yield $y
                        }
                        set nat_x $x
                        set nat_y $y
                    } else {
                        dict lappend queues $address [list $x $y]
                    }
                }
                input {}
            }
        }
    }
}

puts "Part 1: [coroutine Day23 play [::intcode::compile [read -nonewline stdin]]]"
puts "Part 2: [Day23]"
