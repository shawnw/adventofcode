#!/usr/bin/tclsh

proc valid {passwd} {
    set aschars [split $passwd ""]
    set p [lindex $aschars 0]
    set replen 1
    set leasttwo 0
    set justtwo 0
    foreach c [lrange $aschars 1 end] {
        if {$p > $c} {
            return {0 0}
        } elseif {$p == $c} {
            incr replen
            if {$replen == 2} {
                set leasttwo 1
            }
        } else {
            if {$replen == 2} {
                set justtwo 1
            }
            set replen 1
        }
        set p $c
    }
    if {$replen == 2} {
        set justtwo 1
    }
    return [list $leasttwo $justtwo]
}

set tests [dict create 111111 {1 0} 223450 {0 0} 123789 {0 0} 112233 {1 1} \
               123444 {1 0} 111122 {1 1}]
set testno 1
dict for {passwd expected} $tests {
    set failed 0
    lassign [valid $passwd] res1 res2
    if {$res1 != [lindex $expected 0]} {
        set failed 1
        puts "Test $testno: $passwd failed valid1"
    }
    if {$res2 != [lindex $expected 1]} {
        set failed 1
        puts "Test $testno: $passwd failed valid2"
    }
    if {$failed == 0} {
        puts "Test $testno: Passed."
    }
    incr testno
}

# echo XXXXXX-YYYYYY | ./day04
gets stdin line
lassign [split $line -] start stop
set matched1 0
set matched2 0
for {set passwd $start} {$passwd <= $stop} {incr passwd} {
    lassign [valid $passwd] pass1 pass2
    incr matched1 $pass1
    incr matched2 $pass2
}
puts "Part 1: $matched1"
puts "Part 2: $matched2"
