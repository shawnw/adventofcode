#!/usr/bin/tclsh

package require math::complexnumbers
namespace import ::math::complexnumbers::complex ::math::complexnumbers::real \
    ::math::complexnumbers::imag

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
    yield starting
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
                while {$input eq ""} {
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
                yield halted
            }
            default {
                error "Unknown opcode $op at position $ip"
            }
        }
    }
}

proc foreach_idx {var idx lst bdy} {
    upvar $var v $idx i
    set i 0
    foreach v $lst {
        uplevel 1 $bdy
        incr i
    }
}

proc solve {prog input} {
    set insns [dict create]
    foreach_idx op i [split $prog ","] {
        dict set insns $i $op
    }

    set location [complex 0 0]
    set grid [dict create $location $input]
    set direction up
    
    coroutine Robot run $insns

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
                            set location [::math::complexnumbers::+ $location [complex -1 0]]
                        }
                        left {
                            set direction down
                            set location [::math::complexnumbers::+ $location [complex 0 -1]]
                        }
                        down {
                            set direction right
                            set location [::math::complexnumbers::+ $location [complex 1 0]]
                        }
                        right {
                            set direction up
                            set location [::math::complexnumbers::+ $location [complex 0 1]]
                        }
                    }
                } else {
                    switch $direction {
                        up {
                            set direction right
                            set location [::math::complexnumbers::+ $location [complex 1 0]]
                        }
                        right {
                            set direction down
                            set location [::math::complexnumbers::+ $location [complex 0 -1]]
                        }
                        down {
                            set direction left
                            set location [::math::complexnumbers::+ $location [complex -1 0]]
                        }
                        left {
                            set direction up
                            set location [::math::complexnumbers::+ $location [complex 0 1]]
                        }
                    }
                }                
            }
        }
    }
    return $grid
}

proc min {a b} {
    if {$a > $b} {
        return $b
    } else {
        return $a
    }
}

proc max {a b} {
    if {$a > $b} {
        return $a
    } else {
        return $b
    }
}

set prog [read -nonewline stdin]

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



