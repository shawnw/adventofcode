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

namespace eval intcode {
    namespace export run execute compile
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

    proc run {insns {input {}}} {
        set arg [yield [list starting [info coroutine]]]
        if {$arg ne ""} {
            lappend input {*}$arg
        }
        set ip 0
        set rbp 0
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
                        set input [yield input]
                    }
                    set r0 [getaddr insns [expr {$ip+1}]]
                    if {$mode1 == 2} {
                        incr r0 $rbp
                    }
                    set input [lassign $input arg]
                    dict set insns $r0 $arg
                    incr ip 2
                }
                4 { # Output
                    set r0 [getparam $mode1 insns $ip+1 $rbp]
                    incr ip 2
                    set arg [yield [list output $r0]]
                    if {$arg ne ""} {
                        lappend input {*}$arg
                    }                
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
        foreach_idx insn i [split $program ","] {
            dict set prog $i $insn
        }
        return $prog
    }

    # Run until halted, return all output.
    proc execute {program {input ""}} {
    coroutine Prog run $program $input
    set result {}
    while 1 {
        lassign [Prog] state output
        switch $state {
            starting {}
            halted {
                break
            }
            input {
                error "Program wants additional input?"
            }
            output {
                lappend result $output
            }
        }
    }
    return $result
}


    
}
