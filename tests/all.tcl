#!/bin/sh
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set thisScript [file normalize [file join [pwd] [info script]]]
set thisDir    [file dirname $thisScript]

package require tcltest 2.2
namespace import tcltest::*
tcltest::configure -verbose "body error"
#testConstraint knownbug 1
#tcltest::configure -match nagelfar-\[5\].*
#tcltest::configure -match tk-*

proc createTestFile {scr} {
    set ch [open _testfile_ w]
    puts $ch $scr
    close $ch
}

proc execTestFile {args} {
    set xx(-fn) _testfile_
    set xx(-flags) {}
    array set xx $args
    set fn $xx(-fn)
    array unset xx -fn
    set flags $xx(-flags)
    array unset xx -flags

    set res [eval [list exec [info nameofexecutable] nagelfar.tcl $fn] \
            [array get xx] $flags] ;#2>@ stderr
    # Simplify result by shortening standard result
    regsub {Checking file _testfile_\n?} $res "%%" res
    return $res
}    

proc cleanupTestFile {} {
    file delete -force _testfile_
}

foreach test [glob -nocomplain $thisDir/*.test] {
    source $test
}

cleanupTestFile
