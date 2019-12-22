#!/usr/bin/tclsh

proc decode {insn} {
    set op [format "%05s" $insn]
    set opcode [string trimleft [string range $op 3 end] "0"]
    set mode1 [string index $op 2]
    set mode2 [string index $op 1]
    set mode3 [string index $op 0]
    return [list $opcode $mode1 $mode2 $mode3]
}

proc getinsn {uinsns pos {level 1}} {
    upvar $level $uinsns insns
    if {[dict exists $insns $pos]} {
        return [dict get $insns $pos]
    } else {
        dict set insns $pos 0
        return 0
    }
}

proc getparam {mode insns ip rbp} {
    set n [expr $ip]
    switch $mode {
        1 {
            return [getinsn $insns $n 2]
        }
        2 {
            set base [getinsn $insns $n 2]
            return [getinsn $insns [expr {$base + $rbp}] 2]
        }
        0 {
            return [getinsn $insns [getinsn $insns $n 2] 2]
        }
        default {
            error "Unknown mode $mode"
        }
    }
}

proc run {insns {input {}}} {
    set ip 0
    set rbp 0
    while 1 {
        lassign [decode [dict get $insns $ip]] op mode1 mode2 mode3
        switch $op {
            1 { # Addition
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                set r2 [getinsn insns [expr {$ip+3}]]
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                dict set insns $r2 [expr {$r0 + $r1}]
                incr ip 4
            }
            2 { # Multiplication
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                set r2 [getinsn insns [expr {$ip+3}]]
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                dict set insns $r2 [expr {$r0 * $r1}]
                incr ip 4
            }            
            3 { # Input
                while {[llength $input] == 0} {
                    set input [yield input]
                }
                set r0 [getinsn insns [expr {$ip+1}]]
                if {$mode1 == 2} {
                    incr r0 $rbp
                }
                dict set insns $r0 [lindex $input 0]
                set input [lrange $input 1 end]
                incr ip 2
            }
            4 { # Output
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                incr ip 2
                yield [list output $r0]
            }
            5 { # Jump if true
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                if {$r0 != 0} {
                    set ip $r1
                } else {
                    incr ip 3
                }
            }
            6 { # Jump if false
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                if {$r0 == 0} {
                    set ip $r1
                } else {
                    incr ip 3
                }
            }
            7 { # Less-than
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                set r2 [getinsn insns [expr {$ip+3}]]
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                dict set insns $r2 [expr {$r0 < $r1}]
                incr ip 4
            }
            8 { # Equals
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                set r2 [getinsn insns [expr {$ip+3}]]
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                dict set insns $r2 [expr {$r0 == $r1}]
                incr ip 4
            }
            9 { # Adjust relative base pointer
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                incr rbp $r0
                incr ip 2
            }
            99 { # Halt
                return halted
            }
            default {
                error "Unknown opcode $op at position $ip"
            }
        }
    }
}

proc compile {program} {
    set prog [dict create]
    set i 0
    foreach insn [split $program ","] {
        dict set prog $i $insn
        incr i
    }
    return $prog
}

proc play {program} {
    set part1 0
    set grid [dict create]
    puts "Scanning..."
    for {set y 0} {$y < 5000} {incr y} {
        set past 0
        for {set x 0} {$past == 0 && $x < 5000} {incr x} {
            lassign [coroutine Drone run $program [list $x $y]] state affect
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

set program [compile [read -nonewline stdin]]
lassign [play $program] part1 part2
puts "Part 1: $part1"
puts "Part 2: $part2"

