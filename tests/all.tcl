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
#tcltest::configure -match nagelfar-1\[5678\].*

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

test nagelfar-1.1 {
    Command line checks
} -body {
    createTestFile {
        set apa $bepa
    }
    execTestFile -fn _____
} -returnCodes 1 -result {Could not find file '_____'}

test nagelfar-1.2 {
    Command line checks
} -body {
    createTestFile {
        set apa $bepa
    }
    execTestFile -encoding gurkmeja
} -returnCodes 1 -result {*Bad encoding name: "gurkmeja"*} -match glob

test nagelfar-1.3 {
    Command line checks
} -body {
    createTestFile {
        set apa bepa
    }
    execTestFile -encoding ascii
} -returnCodes 0 -result {%%}

test nagelfar-2.1 {
    Basic functionality
} -body {
    createTestFile {
        set bepa 2
        set apa $bepa
        return -1
        return -code error -1
    }
    execTestFile
} -result {%%}

test nagelfar-2.2 {
    Basic functionality
} -body {
    createTestFile {
        set apa $bepa
    }
    execTestFile -filter *Unknown*
} -result {%%}

test nagelfar-2.3 {
    Basic functionality
} -body {
    createTestFile {
        proc apa {w} {
            variable Priv
            global Torv

            after cancel $Torv(afterId)
            puts $Priv(repeated)
            incr Priv(repeated)
        }
    }
    execTestFile
} -result {%%}


test nagelfar-3.1 {
    Basic errors
} -body {
    createTestFile {
        set apa $bepa
    }
    execTestFile
} -result {%%Line   2: E Unknown variable "bepa"}

test nagelfar-3.2 {
    Basic errors
} -body {
    createTestFile {
        proc hej {a b c} {
        }
        set apa [hej a b]
    }
    execTestFile
} -result {%%Line   4: E Wrong number of arguments (2) to "hej"}

test nagelfar-3.3 {
    Basic errors
} -body {
    createTestFile {
        list apa [list a b [list a b]] a]
        list apa [list a b [list a b] a]]
    }
    execTestFile
} -result {%%Line   2: N Unescaped end bracket*Line   3: N Unescaped end bracket} -match glob

test nagelfar-3.4 {
    Basic errors
} -body {
    createTestFile {
        set apa bepa
        set cepa [set $apa]
    }
    execTestFile
} -result {%%Line   3: N Suspicious variable name "$apa"}

test nagelfar-3.5 {
    Basic errors
} -body {
    createTestFile {
        proc apa {w} {
            set Miffo(gurka) 1
            
            incr Miffo(hampa)
            incr Miffo(gurka)
        }
    }
    execTestFile
} -result {%%Line   5: E Unknown variable "Miffo(hampa)"}

test nagelfar-4.1 {
    Options checking
} -body {
    createTestFile {
        lsort -ascii -command xxx -decreasing -dictionary -increasing \
                -index 0 -integer -real -unique [list 1 2 3]
    }
    execTestFile
} -result {%%}

test nagelfar-4.2 {
    Options checking
} -body {
    createTestFile {
        lsort -d [list 1 2 3]
    }
    execTestFile
} -result {%%Line   2: E Ambigous option for lsort, -d -> -decreasing/-dictionary}

test nagelfar-4.3 {
    Options checking
} -body {
    createTestFile {
        lsort -dictionary -index [list 1 2 3]
    }
    execTestFile
} -result {%%Line   2: E Wrong number of arguments (3) to "lsort"}

# This error gives a different message which I hope I can correct.
test nagelfar-4.4 {
    Options checking
} -constraints { 
    knownbug
} -body {
    createTestFile {
        fconfigure xx -blocking 1 -encoding 0 -mode
    }
    execTestFile
} -result {*Missing value for last option*} -match glob

test nagelfar-4.5 {
    Options checking
} -body {
    createTestFile {
        # This should see that i is set
        string is integer -strict -failindex i 789
        puts $i
    }
    execTestFile
} -result {%%}

test nagelfar-4.6 {
    Options checking
} -body {
    createTestFile {
        # Here, -apa cannot be an option
        string match -apa gurka
    }
    execTestFile
} -result {%%}

test nagelfar-4.7 {
    Options checking
} -body {
    createTestFile {
        # There was a bug with glob chars in options
        string match -?* gurka burka
    }
    execTestFile
} -result {%%Line   3: E Bad option -?* to string match}

test nagelfar-5.1 {
    Procedure checking
} -body {
    createTestFile {
        proc info {apa} {
            return $apa
        }
        info hejsan
    }
    execTestFile
} -result {%%Line   2: W Procedure "info" does not match previous definition*}\
        -match glob

test nagelfar-5.2 {
    Procedure checking
} -body {
    createTestFile {
        proc hej {a b c} {
            return $a
        }
        set x 1
        set y 2
        set z 3
        hej $x $y $z
    }
    execTestFile
} -result {%%}

test nagelfar-5.3 {
    Procedure checking, detecting upvar
} -body {
    createTestFile {
        proc hej {a b c} {
            upvar $b apa
            return $apa
        }
        set x 1
        set y 2
        set z 3
        hej $x $y $z
    }
    execTestFile
} -result {%%Line   9: N Suspicious variable name "$y"}

test nagelfar-5.4 {
    Procedure checking, detecting upvar
} -body {
    createTestFile {
        proc hej {a b c} {
            upvar $a apa
            return $apa
        }
        set y 2
        set z 3
        hej x $y $z
    }
    execTestFile
} -result {%%Line   8: E Unknown variable "x"}

test nagelfar-5.5 {
    Procedure checking, detecting upvar
} -body {
    createTestFile {
        proc hej {a b c} {
            upvar $c apa
            set apa 1
        }
        set y 2
        set z 3
        hej $y $z x
        list $x
    }
    execTestFile
} -result {%%}

test nagelfar-5.6 {
    Procedure checking, "wrong order"
} -body {
    createTestFile {
        proc hej {a b c} {
            return [hopp apa bepa cepa]
        }
        proc hopp {a b} {
            return $a
        }
    }
    execTestFile -flags -2pass
} -result {%%Line   3: E Wrong number of arguments (3) to "hopp"}

test nagelfar-6.1 {
    Expression checking
} -body {
    createTestFile {
        expr {1 + ""}
    }
    execTestFile
} -result {%%Line   2: E Bad expression: can't use empty string as operand of "+"}

test nagelfar-6.2 {
    Expression checking
} -body {
    createTestFile {
        set apa 1
        expr {1 + $apa /}
    }
    execTestFile
} -result {%%Line   3: E Bad expression: premature end of expression}

test nagelfar-6.3 {
    Expression checking
} -body {
    createTestFile {
        set apa 10
        set bepa 5
        # This gave a divide by zero error in the first implementation
        expr {1 / ($apa - $bepa)}
    }
    execTestFile
} -result {%%}

test nagelfar-6.4 {
    Expression checking
} -body {
    createTestFile {
        set apa 10
        expr {$apa == {$bepa}}
    }
    execTestFile
} -result {%%}

test nagelfar-7.1 {
    Command: upvar
} -body {
    createTestFile {
        upvar 1 bepa
    }
    execTestFile
} -result {%%Line   2: E Wrong number of arguments (2) to "upvar"}

test nagelfar-7.2 {
    Command: upvar
} -body {
    createTestFile {
        set x hej
        upvar 1 bepa $x
    }
    execTestFile
} -result {%%Line   3: N Suspicious upvar variable "$x"}

test nagelfar-7.3 {
    Command: upvar
} -body {
    createTestFile {
        set x hej
        upvar $x $x bepa
    }
    execTestFile
} -result {%%Line   3: N Non constant level to upvar: "$x"}

test nagelfar-8.1 {
    Variable handling
} -constraints knownbug -body {
    createTestFile {
        proc hej {x y} {
            global item
            list item($x,$y)
        }
    }
    execTestFile
} -result {should detect missing dollar}

test nagelfar-8.2 {
    Variable handling, -novar flag
} -body {
    createTestFile {
        proc hej {x y} {
            set apa bepa
            set cepa apa
        }
    }
    execTestFile -flags -novar
} -result {%%}

test nagelfar-9.1 {
    if statement, as comment
} -body {
    createTestFile {
        if 0 {
            set y $x
        }
        expr {$y}
    }
    execTestFile
} -result {%%Line   5: E Unknown variable "y"}

test nagelfar-9.2 {
    if statement, as comment
} -body {
    createTestFile {
        if 0 then {
            set y $x
        }
        expr {$y}
    }
    execTestFile
} -result {%%Line   5: E Unknown variable "y"}

test nagelfar-10.1 {
    Brace alignment 
} -constraints { 
    knownbug
} -body {
    createTestFile "
        cmd xx yy \\
            apa {
               hejsan
            }
    "
    execTestFile
} -result {%%}

test nagelfar-10.2 {
    Brace alignment 
} -body {
    createTestFile {
        list xx yy {
            hejsan
         }
    }
    execTestFile
} -result {%%Line   4: N Close brace not aligned with line 2 (8 9)}

test nagelfar-11.1 {
    Line numbers
} -body {
    createTestFile "
        list xx yy \\
                zz \$y
        set apa \$bepa
        if 1 {
            list xx yy \\
                    zz \\
                    zz \\
                    zz \\
                    \$x
        }
    "
    execTestFile
} -result {^%%Line\s+3:.*Line\s+4:.*Line\s+10:} -match regexp

test nagelfar-11.2 {
    Line numbers, line 1
} -body {
    createTestFile {apa bepa
    }
    execTestFile
} -result {%%Line   1: W Unknown command "apa"}

test nagelfar-12.1 {
    Comments, bad in switch
} -body {
    createTestFile {
        switch apa {
            hej {
                set x 1
            }
            # A bad comment
            hopp {
                set y 1
            }
        }
    }
    execTestFile
} -result "%%Line   6: W Switch pattern starting with #. This could be a bad comment.*" -match glob

test nagelfar-12.2 {
    Comments, bad in list
} -body {
    createTestFile {
        array set apa {
            elem1 val1
            # A bad comment
            elem2 val2
        }
    }
    execTestFile
} -result "%%Line   4: N Suspicious \# char. Possibly a bad comment."

test nagelfar-12.3 {
    Comments, bad in list
} -body {
    createTestFile {
        miffo apa {
            elem1 val1
            # A bad comment
            elem2 val2
        }
        proc miffo {a b} {}
    }
    execTestFile -flags -2pass
} -result "%%Line   4: N Suspicious \# char. Possibly a bad comment."

test nagelfar-12.4 {
    Comments, should not complain too much
} -body {
    createTestFile {
        if {[catch {
            set apa bepa
            # An ok comment
            set apa bepa
        }]} {
            # An ok comment
        }
        if 0 {
            set apa bepa
            # An ok comment
            set apa bepa
        }
    }
    execTestFile
} -result "%%"

test nagelfar-13.1 {
    Syntax database, multiple ?
} -body {
    createTestFile {
        array set hej {1 2}
        array names hej
        array names hej *a*
        array names hej -regexp *a*
    }
    execTestFile
} -result {%%}

test nagelfar-13.2 {
    Syntax database, v token
} -body {
    createTestFile "
        ##syntax Miffo v

        set apa \[Miffo \\
                hej\]
    "
    execTestFile
} -result "%%Line   5: E Unknown variable \"hej\""

test nagelfar-13.3 {
    Syntax database, n token
} -body {
    createTestFile "
        ##syntax Miffo n
        set hej 1
        set apa \[Miffo \\
                \$hej\]
    "
    execTestFile
} -result "%%Line   5: N Suspicious variable name \"\$hej\""

test nagelfar-14.1 {
    Namespaces, procs in namespace
} -body {
    createTestFile {
        namespace eval apa {}
        proc apa::bepa {hej hopp} {
            set apa $hej
        }
        proc apa::cepa {hej hopp} {
            set apa [bepa $hej $hopp]
        }
    }
    execTestFile
} -result {%%}

test nagelfar-14.2 {
    Namespaces, procs in namespace
} -body {
    createTestFile {
        namespace eval apa {
            proc bepa {hej hopp} {
                set apa $hej
            }
        }
        proc apa::cepa {hej hopp} {
            set apa [bepa $hej $hopp]
        }
    }
    execTestFile
} -result {%%}

test nagelfar-14.3 {
    Namespaces, imported procs
} -constraints { 
    knownbug
} -body {
    createTestFile {
        namespace eval apa {}
        proc apa::bepa {hej hopp} {
            set apa $hej
        }
        namespace import apa::bepa
        proc cepa {hej hopp} {
            set apa [bepa $hej $hopp]
        }
    }
    execTestFile
} -result {%%}

test nagelfar-15.1 {
    Tk, object tracking
} -body {
    createTestFile {
        set apa [frame .f -padx 3 -pady 3]
        $apa configure -padx 2 -miffo 3
        $apa gurka hejsan
    }
    execTestFile
} -result {%%Line   3: E Bad option -miffo*Line   4: E Unknown subcommand "gurka"*} -match glob

test nagelfar-15.2 {
    Tk, options
} -body {
    createTestFile {
        set bepa hej
        set cepa hopp
        set apa [entry .e -textvariable bepa]
        $apa configure -textvariable cepa
    }
    execTestFile
} -result {%%}

test nagelfar-16.1 {
    Types checking
} -body {
    createTestFile {
        set apa [list x y z]
        llength $apa
    }
    execTestFile
} -result {%%}

# Testing the after command which has the special thing
# of accepting either an int or a subcommand as first argument.
test nagelfar-17.1 {
    Command: after
} -body {
    createTestFile { # FIXA: Implement and test this properly
        after 10
        after 20 {set apa 5}
        set id [after 30 set apa 5]
        after cancel $id
        after cancel set apa 5
        after idle {set apa 5}
        after idle set apa 5
        after info $id
    }
    execTestFile
} -result {%%}


test nagelfar-18.1 {
    Command: subcommands
} -body {
    # Proper detection of subcommands should tell the test that
    # apa is a variable name.
    createTestFile {
        set apa 1
        trace variable apa w Hej
        trace add variable apa write Hej
    }
    execTestFile
} -result {%%}

cleanupTestFile
