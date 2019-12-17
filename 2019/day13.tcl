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
    set grid [dict create]
    set ball_x -1
    set paddle_x -1
    set score -1
    set input {}
    for {lassign [coroutine Robot run $program] state output} \
        {$state ne "halted"} \
        {lassign [Robot $input] state output} {
            set input {}
            switch $state {
                input {
                    if {$paddle_x == $ball_x} {
                        set input 0
                    } elseif {$paddle_x < $ball_x} {
                        set input 1
                    } else {
                        set input -1
                    }
                }
                output {
                    set x $output
                    lassign [Robot] state y
                    if {$state ne "output"} { error "Invalid state" }
                    lassign [Robot] state tile
                    if {$state ne "output"} { error "Invalid state" }
                    if {$x == -1 && $y == 0} {
                        set score $tile
                    } else {
                        dict set grid $x $y $tile
                        if {$tile == 4} {
                            set ball_x $x
                        } elseif {$tile == 3} {
                            set paddle_x $x
                        }
                    }
                }
            }
        }
    return [list $grid $score]
}

# Draw the game using fancy unicode character graphics
proc draw {grid height width} {
    for {set y 0} {$y <= $height} {incr y} {
        for {set x 0} {$x <= $width} {incr x} {
            switch [dict get $grid $x $y] {
                0 { puts -nonewline " " }
                1 {
                    if {$x == 0 || $x == $width} {
                        if {$y == 0} {
                            if {$x == 0} {
                                puts -nonewline "\u2554"
                            } else {
                                puts -nonewline "\u2557"
                            }
                        } else {
                            puts -nonewline "\u2551"
                        }
                    } else {
                        puts -nonewline "\u2550"
                    }
                }
                2 { puts -nonewline "\u2591" }
                3 {
                    # \U1F3D3 pingpong paddle - not in my font
                    puts -nonewline "\u2B64"
                }
                4 {
                    # \u26BD soccer ball - too wide
                    puts -nonewline "\u25CF"
                }
            }
        }
        puts ""
    }
}

proc find_dimensions {grid} {
    namespace import ::tcl::mathfunc::max
    set blocks 0
    set width 0
    set height 0
    dict for {x col} $grid {
        set width [max $width $x]
        dict for {y tile} $col {
            set height [max $height $y]
            if {$tile == 2} {
                incr blocks
            }
        }
    }
    return [list $width $height $blocks]
}

set program [compile [read -nonewline stdin]]
lassign [play $program] grid
lassign [find_dimensions $grid] width height blocks

draw $grid $height $width
puts "Part 1: $blocks"

dict set program 0 2
lassign [play $program] grid score
draw $grid $height $width
puts "Part 2: $score"
