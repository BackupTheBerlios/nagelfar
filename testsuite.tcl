#!/bin/sh
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

package require tcltest 2.2
namespace import tcltest::*

proc createTestFile {scr} {
    set ch [open _testfile_ w]
    puts $ch $scr
    close $ch
}

proc execTestFile {args} {
    set xx(-fn) _testfile_
    array set xx $args
    set fn $xx(-fn)
    array unset xx -fn
    eval [list exec [info nameofexecutable] syntax.tcl $fn] [array get xx] 
}    

test syntax-1.1 {
    Command line checks
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile -fn _____
} -returnCodes 1 -result {*Could not find file _____*} -match glob

test syntax-2.1 {
    Basic functionality
} -setup {
    createTestFile {
        set bepa 2
        set apa $bepa
    }
} -body {
    execTestFile
} -result {Checking file _testfile_} -match glob

test syntax-2.2 {
    Basic functionality
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile -filter *Unknown*
} -result {Checking file _testfile_} -match glob

test syntax-3.1 {
    Basic errors
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile
} -result {*Unknown variable "bepa"*} -match glob


file delete _testfile_
