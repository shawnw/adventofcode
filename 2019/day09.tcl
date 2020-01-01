#!/usr/bin/tclsh

source intcode.tcl

set testno 1
proc test {prog input expected} {
    global testno insns
    set prog [::intcode::compile $prog]
    set output [::intcode::execute $prog]
    if {[regexp $expected $output]} {
        puts "Test $testno: passed."
    } else {
        puts "Test $testno: failed. Expected $expected, got $output"
    }
    incr testno
}

test "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" "" \
    "^109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99$"
test "1102,34915192,34915192,7,4,7,99,0" "" "^\[0-9\]\{16\}$"
test "104,1125899906842624,99" "" "^1125899906842624$"

set program [::intcode::compile [read -nonewline stdin]]
puts "Part 1: [::intcode::execute $program 1]"
puts "Part 2: [::intcode::execute $program 2]"
