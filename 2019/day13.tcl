#!/usr/bin/tclsh

source intcode.tcl

proc play {program} {
    set grid [dict create]
    set ball_x -1
    set paddle_x -1
    set score -1
    set input {}
    for {lassign [coroutine Robot ::intcode::run $program] state output} \
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

set program [::intcode::compile [read -nonewline stdin]]
lassign [play $program] grid
lassign [find_dimensions $grid] width height blocks

draw $grid $height $width
puts "Part 1: $blocks"

dict set program 0 2
lassign [play $program] grid score
draw $grid $height $width
puts "Part 2: $score"
