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
    eval [list exec [info nameofexecutable] nagelfar.tcl $fn] [array get xx] \
            ;#2>@ stderr
}    


test nagelfar-1.1 {
    Command line checks
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile -fn _____
} -returnCodes 1 -result {*Could not find file _____*} -match glob


test nagelfar-2.1 {
    Basic functionality
} -setup {
    createTestFile {
        set bepa 2
        set apa $bepa
    }
} -body {
    execTestFile
} -result {Checking file _testfile_} -match glob

test nagelfar-2.2 {
    Basic functionality
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile -filter *Unknown*
} -result {Checking file _testfile_} -match glob


test nagelfar-3.1 {
    Basic errors
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile
} -result {*Unknown variable "bepa"*} -match glob

test nagelfar-3.2 {
    Basic errors
} -setup {
    createTestFile {
        proc hej {a b c} {
        }
        set apa [hej a b]
    }
} -body {
    execTestFile
} -result {*Wrong number of arguments (2) to "hej"*} -match glob

test nagelfar-3.3 {
    Basic errors
} -setup {
    createTestFile {
        list apa [list a b [list a b]] a]
        list apa [list a b [list a b] a]]
    }
} -body {
    execTestFile
} -result {*Unescaped end bracket*Unescaped end bracket*} -match glob

test nagelfar-4.1 {
    Options checking
} -setup {
    createTestFile {
        lsort -ascii -command xxx -decreasing -dictionary -increasing \
                -index 0 -integer -real -unique [list 1 2 3]
    }
} -body {
    execTestFile
} -result {Checking file _testfile_} -match glob

test nagelfar-4.2 {
    Options checking
} -setup {
    createTestFile {
        lsort -d [list 1 2 3]
    }
} -body {
    execTestFile
} -result {*Ambigous option*-d *-dictionary*} -match glob

test nagelfar-4.3 {
    Options checking
} -setup {
    createTestFile {
        lsort -dictionary -index [list 1 2 3]
    }
} -body {
    execTestFile
} -result {*Wrong number of arguments*} -match glob

test nagelfar-4.4 {
    Options checking
} -setup {
    createTestFile {
        fconfigure xx -blocking 1 -encoding 0 -mode
    }
} -body {
    execTestFile
} -result {*Missing value for last option*} -match glob

test nagelfar-4.5 {
    Options checking
} -setup {
    createTestFile {
        # This should see that i is set
        string is integer -strict -failindex i 789
        puts $i
    }
} -body {
    execTestFile
} -result {Checking file _testfile_} -match glob


file delete _testfile_
