#!/usr/bin/tclsh

namespace path {::tcl::mathfunc}

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

proc run {insns input} {
    yield starting
    set ip 0
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
                while {$input eq ""} {
                    set input [yield input]
                }
                set r0 [lindex $insns $ip+1]
                lset insns $r0 [lindex $input 0]
                set input [lrange $input 1 end]
                incr ip 2
            }
            4 { # Output
                getparam r0 $mode1 $insns $ip+1
                incr ip 2
                yield [list output $r0]
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
                yield halted
            }
            default {
                error "Unknown opcode $op at position $ip"
            }
        }
    }
}

proc solve {prog begin end} {
    set maxoutput -1
    for {set a $begin} {$a < $end} {incr a} {
        for {set b $begin} {$b < $end} {incr b} {
            if {$a == $b} {
                continue
            }
            for {set c $begin} {$c < $end} {incr c} {
                if {$a == $c || $b == $c} {
                    continue
                }
                for {set d $begin} {$d < $end} {incr d} {
                    if {$a == $d || $b == $d || $c == $d} {
                        continue
                    }
                    for {set e $begin} {$e < $end} {incr e} {
                        if {$a == $e || $b == $e || $c == $e || $d == $e} {
                            continue
                        }
                        coroutine ampA run $prog [list $a 0]
                        coroutine ampB run $prog $b
                        coroutine ampC run $prog $c
                        coroutine ampD run $prog $d
                        coroutine ampE run $prog $e
                        set input ""
                        set prevoutput ""
                        set running 1
                        while {$running} {
                            foreach amp {ampA ampB ampC ampD ampE} {
                                set outputted 0
                                while {1} {
#                                   puts -nonewline "Running $amp... "
                                    lassign [$amp $input] state output
                                    set input ""
                                    switch $state {
                                        starting {
#                                          puts "starting out"
                                        }
                                        input {
#                                           puts -nonewline "needs input... "
                                            if {$prevoutput ne ""} {
                                                set input $prevoutput
                                                set prevoutput ""
#                                               puts "using $input"
                                            } else {
#                                               puts "none in queue"
                                                break
                                            }
                                        }
                                        output {
#                                           puts "outputting $output"
                                            set prevoutput $output
                                            set outputted 1
                                            if {$amp eq "ampE"} {
                                                set result $output
                                            }
                                            break
                                        }
                                        halted {
#                                           puts "halted"
                                            if {$amp eq "ampE"} {
                                                set running 0
                                            }
                                            break
                                        }
                                    }
                                    if {$outputted == 0} { set prevoutput "" }
                                }
                            }
                        }
                        set ampA {}
                        set ampB {}
                        set ampC {}
                        set ampD {}
                        set ampE {}
                        set maxoutput [max $maxoutput $result]
                    }
                }
            }
        }
    }
    return $maxoutput
}

set testno 1
proc test {insns expected {begin 0} {end 5}} {
    global testno
    set result [solve [split $insns ","] $begin $end]
    if {$result == $expected} {
        puts "Test $testno: Passed."
    } else {
        puts "Test $testno: Failed. Program $insns expected $expected gave $result"
    }
    incr testno
}

test "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" 43210
test "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" 54321
test "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" 65210
test "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" 139629729 5 10
test "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" 18216 5 10

set prog [split [read -nonewline stdin] ","]
puts "Part 1: [solve $prog 0 5]"
puts "Part 2: [solve $prog 5 10]"
