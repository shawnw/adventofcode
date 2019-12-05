#!/usr/bin/tclsh

proc decode {insn} {
    set op [format %05s $insn]
    set opcode [string trimleft [string range $op 3 end] "0"]
    set mode1 [string index $op 2]
    set mode2 [string index $op 1]
    set mode3 [string index $op 0]
    return [list $opcode $mode1 $mode2 $mode3]
}

proc getparam {ureg mode insns n} {
    upvar $ureg reg
    set reg [if $mode {
        lindex $insns $n
    } else {
        lindex $insns [lindex $insns $n]
    }]
}

proc run {uinsns input} {
    upvar $uinsns insns
    set ip 0
    set inputpos 0
    set output {}
    while 1 {
        lassign [decode [lindex $insns $ip]] op mode1 mode2 mode3
        switch $op {
            1 { # Addition
                getparam r0 $mode1 $insns $ip+1
                getparam r1 $mode2 $insns $ip+2
                set r2 [lindex $insns $ip+3]
                lset insns $r2 [expr {$r0 + $r1}]
                incr ip 4
            }
            2 { # Multiplication
                getparam r0 $mode1 $insns $ip+1
                getparam r1 $mode2 $insns $ip+2
                set r2 [lindex $insns $ip+3]
                lset insns $r2 [expr {$r0 * $r1}]
                incr ip 4
            }
            3 { # Input
                set r0 [lindex $insns $ip+1]
                set val [lindex $input $inputpos]
                incr inputpos
                lset insns $r0 $val
                incr ip 2
            }
            4 { # Output
                getparam r0 $mode1 $insns $ip+1
                lappend output $r0
                incr ip 2
            }
            5 { # Jump if true
                getparam r0 $mode1 $insns $ip+1
                getparam r1 $mode2 $insns $ip+2
                if {$r0 != 0} {
                    set ip $r1
                } else {
                    incr ip 3
                }
            }   
            6 { # Jump if false
                getparam r0 $mode1 $insns $ip+1
                getparam r1 $mode2 $insns $ip+2
                if {$r0 == 0} {
                    set ip $r1
                } else {
                    incr ip 3
                }
            }
            7 { # Less-than
                getparam r0 $mode1 $insns $ip+1
                getparam r1 $mode2 $insns $ip+2
                set r2 [lindex $insns $ip+3]
                lset insns $r2 [expr {$r0 < $r1}]
                incr ip 4
            }
            8 { # Equals
                getparam r0 $mode1 $insns $ip+1
                getparam r1 $mode2 $insns $ip+2
                set r2 [lindex $insns $ip+3]
                lset insns $r2 [expr {$r0 == $r1}]
                incr ip 4
            }
            99 { # Halt
                return $output
            }
            default {
                puts "Unknown opcode $op at position $ip"
                return -1
            }
        }
    }
}

set testno 1
proc test {insns expected} {
    global testno
    set prog [split $insns ","]
    set orig $prog
    run prog {}
    if {$prog == $expected} {
        puts "Test $testno: passed."
    } else {
        puts "Test $testno: $orig failed. Expected $expected got $prog"
    }
    incr testno
}

proc test2 {insns input expected_output} {
    global testno
    set prog [split $insns ","]
    set orig $prog
    set output [run prog $input]
    if {$output == $expected_output} {
        puts "Test $testno: passed."
    } else {
        puts "test $testno: $orig failed. Expected output $expected_output got $output"
    }
    incr testno
}

test2 "3,0,4,0,99" {1} {1}
test "1002,4,3,4,33" {1002 4 3 4 99} 
test "1101,100,-1,4,0" {1101 100 -1 4 99}
test2 "3,9,8,9,10,9,4,9,99,-1,8" {8} {1}
test2 "3,9,8,9,10,9,4,9,99,-1,8" {5} {0}
test2 "3,9,7,9,10,9,4,9,99,-1,8" {5} {1}
test2 "3,9,7,9,10,9,4,9,99,-1,8" {9} {0}
test2 "3,3,1108,-1,8,3,4,3,99" {8} {1}
test2 "3,3,1108,-1,8,3,4,3,99" {5} {0}
test2 "3,3,1107,-1,8,3,4,3,99" {5} {1}
test2 "3,3,1107,-1,8,3,4,3,99" {9} {0}
test2 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" {0} {0}
test2 "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" {5} {1}
test2 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" {0} {0}
test2 "3,3,1105,-1,9,1101,0,0,12,4,12,99,1" {5} {1}
test2 "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" {5} {999}
test2 "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" {8} {1000}
test2 "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" {10} {1001}

gets stdin input
set prog1 [split $input ","]
set prog2 $prog1
puts "Part 1: [run prog1 {1}]"
puts "Part 2: [run prog2 {5}]"
