#!/usr/bin/tclsh

proc run {uinsns} {
    upvar $uinsns insns
    set ip 0
    while {1} {
        set insn [lindex $insns $ip]
        if {$insn == 1} { # Addition
            set r0 [lindex $insns [lindex $insns $ip+1]]
            set r1 [lindex $insns [lindex $insns $ip+2]]
            set r2 [lindex $insns $ip+3]
            lset insns $r2 [expr {$r0 + $r1}]
            incr ip 4
        } elseif {$insn == 2} { # Multiplication
            set r0 [lindex $insns [lindex $insns $ip+1]]
            set r1 [lindex $insns [lindex $insns $ip+2]]
            set r2 [lindex $insns $ip+3]
            lset insns $r2 [expr {$r0 * $r1}]
            incr ip 4
        } elseif {$insn == 99} { # Halt
            return [lindex $insns 0]
        } else {
            puts "Unknown opcode $insn at position $ip"
            return -1
        }
    }
}

# Test cases
if {1} {
    set testprog [list 1 9 10 3 2 3 11 0 99 30 40 50]
    run testprog
    puts "Test 1: $testprog"
    set testprog [list 1 0 0 0 99]
    run testprog
    puts "Test 2: $testprog"
    set testprog [list 2 3 0 3 99]
    run testprog
    puts "Test 3: $testprog"
    set testprog [list 2 4 4 5 99 0]
    run testprog
    puts "Test 4: $testprog"
    set testprog [list 1 1 1 4 99 5 6 0 99]
    run testprog
    puts "Test 5: $testprog"
}

gets stdin line
set insns [split $line ","]
set p1_insns $insns
lset p1_insns 1 12
lset p1_insns 2 2
puts "Part1: [run p1_insns]"
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
