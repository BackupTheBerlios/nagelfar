#!/bin/sh
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set thisScript [file normalize [file join [pwd] [info script]]]
set thisDir    [file dirname $thisScript]

package require tcltest
namespace import tcltest::*
tcltest::configure -verbose "body error" -singleproc 1
#testConstraint knownbug 1
if {$argc > 0} {
    eval tcltest::configure $argv
}

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
    
    set file nagelfar.tcl
    if {[file exists ${file}_i]} {
        set file ${file}_i
    }
    set res [eval [list exec [info nameofexecutable] $file $fn] \
            [array get xx] $flags] ;#2>@ stderr
    # Simplify result by shortening standard result
    regsub {Checking file _testfile_\n?} $res "%%" res
    return $res
}    

proc cleanupTestFile {} {
    file delete -force _testfile_
}

tcltest::testsDirectory $thisDir
tcltest::runAllTests

cleanupTestFile
