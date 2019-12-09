#!/usr/bin/tclsh

array set insns {}

proc decode {insn} {
    set op [format %05s $insn]
    set opcode [string trimleft [string range $op 3 end] "0"]
    set mode1 [string index $op 2]
    set mode2 [string index $op 1]
    set mode3 [string index $op 0]
    return [list $opcode $mode1 $mode2 $mode3]
}

proc getinsn {pos} {
    global insns
    if {![info exists insns($pos)]} {
        set insns($pos) 0
        return 0
    }
    return $insns($pos)
}

proc getparam {rbp mode ip offset} {
    set n [expr {$ip + $offset}]
    switch $mode {
        1 {
            return [getinsn $n]
        }
        2 {
            set base [getinsn $n]
            return [getinsn [expr {$base + $rbp}]]
        }
        0 {
            return [getinsn [getinsn $n]]
        }
        default {
            error "Unknown mode $mode"
        }
    }
}

proc run {input} {
    global insns
    set rbp 0
    set ip 0
    set inputpos 0
    set output {}
    while 1 {
        lassign [decode $insns($ip)] op mode1 mode2 mode3
        switch $op {
            1 { # Addition
                set r0 [getparam $rbp $mode1 $ip 1]
                set r1 [getparam $rbp $mode2 $ip 2]
                set r2 $insns([expr {$ip + 3}])
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                set insns($r2) [expr {$r0 + $r1}]
                incr ip 4
            }
            2 { # Multiplication
                set r0 [getparam $rbp $mode1 $ip 1]
                set r1 [getparam $rbp $mode2 $ip 2]
                set r2 $insns([expr {$ip + 3}])
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                set insns($r2) [expr {$r0 * $r1}]
                incr ip 4
            }
            3 { # Input
                set r0 $insns([expr {$ip + 1}])
                if {$mode1 == 2} {
                    incr r0 $rbp
                }
                set val [lindex $input $inputpos]
                incr inputpos
                set insns($r0) $val
                incr ip 2
            }
            4 { # Output
                set r0 [getparam $rbp $mode1 $ip 1]
                lappend output $r0
                incr ip 2
            }
            5 { # Jump if true
                set r0 [getparam $rbp $mode1 $ip 1]
                set r1 [getparam $rbp $mode2 $ip 2]
                if {$r0 != 0} {
                    set ip $r1
                } else {
                    incr ip 3
                }
            }   
            6 { # Jump if false
                set r0 [getparam $rbp $mode1 $ip 1]
                set r1 [getparam $rbp $mode2 $ip 2]
                if {$r0 == 0} {
                    set ip $r1
                } else {
                    incr ip 3
                }
            }
            7 { # Less-than
                set r0 [getparam $rbp $mode1 $ip 1]
                set r1 [getparam $rbp $mode2 $ip 2]
                set r2 $insns([expr {$ip + 3}])
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                set insns($r2) [expr {$r0 < $r1}]
                incr ip 4
            }
            8 { # Equals
                set r0 [getparam $rbp $mode1 $ip 1]
                set r1 [getparam $rbp $mode2 $ip 2]
                set r2 $insns([expr {$ip + 3}])
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                set insns($r2) [expr {$r0 == $r1}]
                incr ip 4
            }
            9 { # Adjust relative base pointer
                set r0 [getparam $rbp $mode1 $ip 1]
                incr rbp $r0
                incr ip 2
            }
            99 { # Halt
                return $output
            }
            default {
                error "Unknown opcode $insns($ip) at position $ip"
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

proc setup {prog} {
    global insns
    array unset insns
    foreach_idx word i [split $prog ","] {
        set insns($i) $word
    }
}

set testno 1
proc test {prog input expected} {
    global testno insns
    setup $prog
    set output [run $input]
    if {[regexp $expected $output]} {
        puts "Test $testno: passed."
    } else {
        puts "Test $testno: failed. Expected $expected, got $output"
    }
    incr testno
}

test "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" "" \
    "^109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99$"
test "1102,34915192,34915192,7,4,7,99,0" "" "^\[0-9\]\{16\}$"
test "104,1125899906842624,99" "" "^1125899906842624$"

set program [read -nonewline stdin]
setup $program
puts "Part 1: [run 1]"
setup $program
puts "Part 2: [run 2]"
