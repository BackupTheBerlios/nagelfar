#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set cho [open test.html w]

puts $cho "<pre>"
set ch [open test.tcl r]
set n 1
while {[gets $ch line] != -1} {

    if {[regexp {^([^#]*)(#.*)$} $line -> pre post]} {
        set line "$pre<span style=\"color: #b22222\">$post</span>"
    }
    puts $cho [format "<span style=\"color: #808080\">%3d</span>  %s" $n $line]
    incr n
}
close $ch
puts $cho "</pre>"

puts $cho "<br><br>"
puts $cho "The result of checking the above is:"
puts $cho "<br><br>"

puts $cho "<pre>"
set ch [open test.result r]
puts -nonewline $cho [read $ch]
close $ch
puts $cho "</pre>"

close $cho
