#!/usr/bin/tclsh

proc run {uinsns} {
    upvar $uinsns insns
    set ip 0
    while 1 {
        switch [lindex $insns $ip] {
            1 { # Addition
                set r0 [lindex $insns [lindex $insns $ip+1]]
                set r1 [lindex $insns [lindex $insns $ip+2]]
                set r2 [lindex $insns $ip+3]
                lset insns $r2 [expr {$r0 + $r1}]
                incr ip 4
            }
            2 { # Multiplication
                set r0 [lindex $insns [lindex $insns $ip+1]]
                set r1 [lindex $insns [lindex $insns $ip+2]]
                set r2 [lindex $insns $ip+3]
                lset insns $r2 [expr {$r0 * $r1}]
                incr ip 4
            }
            99 { # Halt
                return [lindex $insns 0]
            }
            default {
                puts "Unknown opcode [lindex $insns $ip] at position $ip"
                return -1
            }
        }
    }
}

proc gen_seq {{n 1}} {
    yield
    while 1 {
        yield $n
        incr n
    }
}

proc runtest {input expected} {
    puts -nonewline "Test [testno]: $input"
    run input
    if {$input == $expected} {
        puts " passed."
    } else {
        puts " failed! Got \[$input\], expected \[$expected\]"
    }
}

# Test cases
if 1 {
    coroutine testno gen_seq
    runtest {1 9 10 3 2 3 11 0 99 30 40 50} {3500 9 10 70 2 3 11 0 99 30 40 50}
    runtest {1 0 0 0 99} {2 0 0 0 99}
    runtest {2 3 0 3 99} {2 3 0 6 99}
    runtest {2 4 4 5 99 0} {2 4 4 5 99 9801}
    runtest {1 1 1 4 99 5 6 0 99} {30 1 1 4 2 5 6 0 99}
    rename testno {}
}

gets stdin line
set insns [split $line ","]

set p1_insns $insns
lset p1_insns 1 12
lset p1_insns 2 2
puts "Part 1: [run p1_insns]"

for {set noun 0} {$noun < 100} {incr noun} {
    for {set verb 0} {$verb < 100} {incr verb} {
        set p2_insns $insns
        lset p2_insns 1 $noun
        lset p2_insns 2 $verb
        if {[run p2_insns] == 19690720} {
            puts "Part 2: [expr {100 * $noun + $verb}]"
            exit 0
        }
    }
}
puts "No solution found!"
