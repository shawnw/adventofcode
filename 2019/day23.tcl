#!/usr/bin/tclsh

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
    set arg [yield starting]
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
        lset states $i [coroutine $name run $program $i]
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
                    set input -1
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

puts "Part 1: [coroutine Day23 play [compile [read -nonewline stdin]]]"
puts "Part 2: [Day23]"
