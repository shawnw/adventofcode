#!/usr/bin/env tclsh

# For each policy, build a regular expression to validate the given password.
# Because I like pain.

set re {^(\d+)-(\d+) (.): (.*)}

proc solve1 {input} {
    set valid 0
    foreach line $input {
        if {[regexp $::re $line -> min max char password]} {
            set validator [format {^(?:[^%s]*%s[^%s]*){%d,%d}$} \
                               $char $char $char $min $max]
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
            incr idx2 -1
            set validator \
                [string cat \
                     "(?:^[string repeat . $idx1]$char[string repeat . [expr {$idx2-$idx1-1}]]\[^$char\])" \
                     "|" \
                     "(?:^[string repeat . $idx1]\[^$char\][string repeat . [expr {$idx2-$idx1-1}]]$char)"]
            if {[regexp $validator $password]} { incr valid }
        }
    }
    puts $valid
}

set input [split [read stdin] \n]
solve1 $input
solve2 $input
