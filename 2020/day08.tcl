#!/usr/bin/env tclsh
package require Tcl 8.6

proc nop_coro {num} {
    lassign [yieldto return -level 0 [info coroutine]] ip acc
    set ::seen($ip) 1
    set ::numbers($ip) $num
    incr ip
    lassign [yieldto [lindex $::instructions $ip] $ip $acc] ip acc
    error $acc
}

proc nop {num} {
    lappend ::instructions [coroutine nop[incr ::cn] nop_coro $num]
}

proc jmp_coro {num} {
    lassign [yieldto return -level 0 [info coroutine]] ip acc
    set ::seen($ip) 1
    set ::numbers($ip) $num
    incr ip $num
    lassign [yieldto [lindex $::instructions $ip] $ip $acc] ip acc
    error $acc
}

proc jmp {num} {
    lappend ::instructions [coroutine jmp[incr ::cj] jmp_coro $num]
}

proc acc_coro {num} {
    lassign [yieldto return -level 0 [info coroutine]] ip acc
    set ::seen($ip) 1
    incr acc $num
    incr ip
    lassign [yieldto [lindex $::instructions $ip] $ip $acc] ip acc
    error $acc
}

proc acc {num} {
    lappend ::instructions [coroutine acc[incr ::ca] acc_coro $num]
}

proc game_over {} {
    lassign [yieldto return -level 0 [info coroutine]] ip acc
    return $acc
}

proc solve1 {filename} {
    source $filename
    lappend ::instructions [coroutine Done game_over]
    catch {[lindex $::instructions 0] 0 0} acc
    puts "Part 1: $acc"
}

proc solve2 {filename} {
    # Work backwards
    for {set ip [expr {[llength $::instructions] - 1}]} {$ip > 0} {incr ip -1} {
        if {[info exists ::seen($ip)]} {
            if {[string match {::acc*} [lindex $::instructions $ip]]} {
                continue
            }
            unset ::instructions
            source $filename
            lappend ::instructions [coroutine Done game_over]
            switch -glob -- [lindex $::instructions $ip] {
                ::nop* {
                    lset ::instructions $ip [coroutine Test jmp_coro $::numbers($ip)]
                }
                ::jmp* {
                    lset ::instructions $ip [coroutine Test nop_coro $::numbers($ip)]
                }
            }
            if {[catch {[lindex $::instructions 0] 0 0} acc] == 0} {
                puts "Part 2: $acc"
                return
            }
        }
    }
}

solve1 [lindex $argv 0]
solve2 [lindex $argv 0]
