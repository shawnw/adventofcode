#!/usr/bin/wish

# $ ./day08.tcl < input.txt

package require Tk

proc layers {raw width height} {
    set output {}
    set offset 0
    set strlen [string length $raw]
    set chunklen [expr {$width * $height}]
    while {$offset < $strlen} {
        set layer [string range $raw $offset [expr {$chunklen + $offset - 1}]]
        if {[string length $layer] != $chunklen} {
            error "Bad size: [string length $layer] should be $chunklen"
        }
        lappend output [split $layer ""]
        incr offset $chunklen
    }
    return $output
}

proc countdigits {layer} {
    set zero 0
    set one 0
    set two 0
    foreach pixel $layer {
        switch $pixel {
            0 { incr zeros }
            1 { incr ones }
            2 { incr twos }
            default { error "Unknown pixel value $pixel" }
        }
    }
    return [list $zeros $ones $twos]
}

set width 25
set height 6
set scale 20
set image [layers [read -nonewline stdin] $width $height]

set minzeros [expr {$width * $height + 1}]
set part1 0
foreach layer $image {
    lassign [countdigits $layer] zeros ones twos
    if {$zeros < $minzeros} {
        set minzeros $zeros
        set part1 [expr {$ones * $twos}]
    }
}

set render [lindex $image 0]
set len [llength $render]
foreach layer [lrange $image 1 end] {
    for {set i 0} {$i < $len} {incr i} {
        if {[lindex $render $i] == 2} {
            lset render $i [lindex $layer $i]
        }
    }
}

# Create the GUI
wm title . "Advent Of Code 2019 Day 8"
grid [ttk::frame .c -padding "3 3 12 12"] -column 0 -row 0 -sticky news
grid columnconfigure . 0 -weight 1
grid rowconfigure . 0 -weight 1
grid [ttk::label .c.part1 -text "Part 1: $part1"] -column 1 -row 1 -sticky we
grid [ttk::label .c.labelpart2 -text "Part 2:"] -column 1 -row 2 -sticky we
grid [tk::canvas .c.part2 -background green  -width [expr {$width * $scale}] \
          -height [expr {$height * $scale}]] -padx 10 -pady 10 -column 1 -row 3 \
    -sticky news
grid [ttk::button .c.button -text "Quit" -command exit] -column 1 -row 4 -sticky we

# And paint the canvas
set x 0
set y 0
set i 1
foreach pixel $render {
    switch $pixel {
        0 { .c.part2 create rectangle [expr {$x + $scale}] $y $x [expr {$y + $scale}] \
                -fill white -outline white }
        1 { .c.part2 create rectangle [expr {$x + $scale}] $y $x [expr {$y + $scale}] \
                -fill black -outline black }
        2 {}
    }
    if {($i % $width) == 0} {
        incr y $scale
        set x 0
    } else {
        incr x $scale
    }
    incr i
}
