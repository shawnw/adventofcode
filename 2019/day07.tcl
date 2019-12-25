#!/usr/bin/tclsh

package require struct::list
namespace path {::tcl::mathfunc}

proc decode {insn} {
    set op [format "%05s" $insn]
    set opcode [string trimleft [string range $op 3 end] "0"]
    set mode1 [string index $op 2]
    set mode2 [string index $op 1]
    set mode3 [string index $op 0]
    return [list $opcode $mode1 $mode2 $mode3]
}

proc getaddr {uinsns pos {level 1}} {
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
            return [getaddr $insns $n 2]
        }
        2 {
            set base [getaddr $insns $n 2]
            return [getaddr $insns [expr {$base + $rbp}] 2]
        }
        0 {
            return [getaddr $insns [getaddr $insns $n 2] 2]
        }
        default {
            error "Unknown mode $mode"
        }
    }
}

proc run {insns next {input {}}} {
    set arg [yield [info coroutine]]
    if {$arg ne ""} {
        lappend input {*}$arg
    }
    set ip 0
    set rbp 0
    set output {}
    while 1 {
        lassign [decode [dict get $insns $ip]] op mode1 mode2 mode3
        switch $op {
            1 { # Addition
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                set r2 [getaddr insns [expr {$ip+3}]]
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                dict set insns $r2 [expr {$r0 + $r1}]
                incr ip 4
            }
            2 { # Multiplication
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                set r2 [getaddr insns [expr {$ip+3}]]
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                dict set insns $r2 [expr {$r0 * $r1}]
                incr ip 4
            }            
            3 { # Input
                while {[llength $input] == 0} {
                    if {[llength $output] > 0} {
                        set input [yieldto $next $output]
                        set output {}
                    } else {
                        error "[info coroutine] wanting input, no output queued"
                    }
                }
                
                set r0 [getaddr insns [expr {$ip+1}]]
                if {$mode1 == 2} {
                    incr r0 $rbp
                }
                set arg [::struct::list shift input]
                dict set insns $r0 $arg
                incr ip 2
            }
            4 { # Output
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                incr ip 2
                lappend output $r0
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
                set r2 [getaddr insns [expr {$ip+3}]]
                if {$mode3 == 2} {
                    incr r2 $rbp
                }
                dict set insns $r2 [expr {$r0 < $r1}]
                incr ip 4
            }
            8 { # Equals
                set r0 [getparam $mode1 insns $ip+1 $rbp]
                set r1 [getparam $mode2 insns $ip+2 $rbp]
                set r2 [getaddr insns [expr {$ip+3}]]
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
                if {[info coroutine] eq "::ampE"} {
                    return $output
                } else {
                    rename [info coroutine] ""
                    yieldto $next $output
                }
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
        switch [catch { uplevel 1 $bdy } status] {
            0 { incr i }
            1 { return -code error -level 2 $status }
            2 { return -level 2 $status }
            3 { break }
            4 { incr i }
        }
    }
}

proc compile {program} {
    set prog [dict create]
    foreach_idx insn i [split $program ","] {
        dict set prog $i $insn
    }
    return $prog
}

proc solve {prog begin end} {
    set maxoutput -1
    set phases {}
    for {set phase $begin} {$phase < $end} {incr phase} {
        lappend phases $phase
    }
    ::struct::list foreachperm trial $phases {
        lassign $trial a b c d e
        coroutine ampA run $prog ampB $a
        coroutine ampB run $prog ampC $b
        coroutine ampC run $prog ampD $c
        coroutine ampD run $prog ampE $d
        coroutine ampE run $prog ampA $e
        set maxoutput [max $maxoutput [ampA 0]]
    }
    return $maxoutput
}

set testno 1
proc test {insns expected {begin 0} {end 5}} {
    global testno
    set result [solve [compile $insns] $begin $end]
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

set prog [compile [read -nonewline stdin]]
puts "Part 1: [solve $prog 0 5]"
puts "Part 2: [solve $prog 5 10]"
