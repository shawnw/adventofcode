#!/usr/bin/env tclsh

# For each policy, build a regular expression to validate the given password.
# Because I like pain.

set re {^(\d+)-(\d+) (.): (.*)}

proc solve1 {input} {
    set valid 0
    foreach line $input {
        if {[regexp $::re $line -> min max char password]} {
            set validator [format {^(?:[^%s]*%s){%d,%d}[^%s]*$} \
                               $char $char $min $max $char]
            if {[regexp $validator $password]} { incr valid }
        }
    }
    puts $valid
}

proc solve2 {input} {
    set valid 0
    foreach line $input {
        if {[regexp $::re $line -> idx1 idx2 char password]} {
            incr idx1 -1
            set n [expr {$idx2 - $idx1 - 2}]
            set validator \
                [format {(?:^.{%d}%s.{%d}[^%s])
                         |
                         (?:^.{%d}[^%s].{%d}%s)} \
                     $idx1 $char $n $char $idx1 $char $n $char]
            if {[regexp -expanded $validator $password]} { incr valid }
        }
    }
    puts $valid
}

set input [split [read stdin] \n]
solve1 $input
solve2 $input
