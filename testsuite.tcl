#!/bin/sh
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

package require tcltest 2.2
namespace import tcltest::*
tcltest::configure -verbose "body error"
#testConstraint knownbug 1
#tcltest::configure -match nagelfar-6.*

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


test nagelfar-1.1 {
    Command line checks
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile -fn _____
} -returnCodes 1 -result {Could not find file _____}


test nagelfar-2.1 {
    Basic functionality
} -setup {
    createTestFile {
        set bepa 2
        set apa $bepa
        return -1
        return -code error -1
    }
} -body {
    execTestFile
} -result {%%}

test nagelfar-2.2 {
    Basic functionality
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile -filter *Unknown*
} -result {%%}

test nagelfar-2.3 {
    Basic functionality
} -setup {
    createTestFile {
        proc apa {w} {
            variable Priv
            global Torv

            after cancel $Torv(afterId)
            puts $Priv(repeated)
            incr Priv(repeated)
        }
    }
} -body {
    execTestFile
} -result {%%}


test nagelfar-3.1 {
    Basic errors
} -setup {
    createTestFile {
        set apa $bepa
    }
} -body {
    execTestFile
} -result {%%Line   2: E Unknown variable "bepa"}

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
} -result {%%Line   4: E Wrong number of arguments (2) to "hej"}

test nagelfar-3.3 {
    Basic errors
} -setup {
    createTestFile {
        list apa [list a b [list a b]] a]
        list apa [list a b [list a b] a]]
    }
} -body {
    execTestFile
} -result {%%Line   2: N Unescaped end bracket*Line   3: N Unescaped end bracket} -match glob

test nagelfar-3.4 {
    Basic errors
} -setup {
    createTestFile {
        set apa bepa
        set cepa [set $apa]
    }
} -body {
    execTestFile
} -result {%%Line   3: N Suspicious variable name "$apa"}

test nagelfar-3.5 {
    Basic errors
} -setup {
    createTestFile {
        proc apa {w} {
            set Miffo(gurka) 1
            
            incr Miffo(hampa)
            incr Miffo(gurka)
        }
    }
} -body {
    execTestFile
} -result {%%Line   5: E Unknown variable "Miffo(hampa)"}

test nagelfar-4.1 {
    Options checking
} -setup {
    createTestFile {
        lsort -ascii -command xxx -decreasing -dictionary -increasing \
                -index 0 -integer -real -unique [list 1 2 3]
    }
} -body {
    execTestFile
} -result {%%}

test nagelfar-4.2 {
    Options checking
} -setup {
    createTestFile {
        lsort -d [list 1 2 3]
    }
} -body {
    execTestFile
} -result {%%Line   2: E Ambigous option for lsort, -d -> -decreasing/-dictionary}

test nagelfar-4.3 {
    Options checking
} -setup {
    createTestFile {
        lsort -dictionary -index [list 1 2 3]
    }
} -body {
    execTestFile
} -result {%%Line   2: E Wrong number of arguments (3) to "lsort"}

# This error gives a different message which I hope I can correct.
test nagelfar-4.4 {
    Options checking
} -constraints { 
    knownbug
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
} -result {%%}

test nagelfar-5.1 {
    Procedure checking
} -setup {
    createTestFile {
        proc info {apa} {
            return $apa
        }
        info hejsan
    }
} -body {
    execTestFile
} -result {%%Line   2: W Procedure "info" does not match previous definition*}\
        -match glob

test nagelfar-5.2 {
    Procedure checking
} -setup {
    createTestFile {
        proc hej {a b c} {
            return $a
        }
        set x 1
        set y 2
        set z 3
        hej $x $y $z
    }
} -body {
    execTestFile
} -result {%%}

test nagelfar-5.3 {
    Procedure checking, detecting upvar
} -setup {
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
} -body {
    execTestFile
} -result {%%Line   9: N Suspicious variable name "$y"}

test nagelfar-5.4 {
    Procedure checking, detecting upvar
} -setup {
    createTestFile {
        proc hej {a b c} {
            upvar $a apa
            return $apa
        }
        set y 2
        set z 3
        hej x $y $z
    }
} -body {
    execTestFile
} -result {%%Line   8: E Unknown variable "x"}

test nagelfar-5.5 {
    Procedure checking, detecting upvar
} -setup {
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
} -body {
    execTestFile
} -result {%%}

test nagelfar-5.6 {
    Procedure checking, "wrong order"
} -setup {
    createTestFile {
        proc hej {a b c} {
            return [hopp apa bepa cepa]
        }
        proc hopp {a b} {
            return $a
        }
    }
} -body {
    execTestFile -flags -2pass
} -result {%%Line   3: E Wrong number of arguments (3) to "hopp"}

test nagelfar-6.1 {
    Expression checking
} -setup {
    createTestFile {
        expr {1 + ""}
    }
} -body {
    execTestFile
} -result {%%Line   2: E Bad expression: can't use empty string as operand of "+"}

test nagelfar-6.2 {
    Expression checking
} -setup {
    createTestFile {
        set apa 1
        expr {1 + $apa /}
    }
} -body {
    execTestFile
} -result {%%Line   3: E Bad expression: premature end of expression}

test nagelfar-6.3 {
    Expression checking
} -setup {
    createTestFile {
        set apa 10
        set bepa 5
        # This gave a divide by zero error in the first implementation
        expr {1 / ($apa - $bepa)}
    }
} -body {
    execTestFile
} -result {%%}

test nagelfar-6.4 {
    Expression checking
} -setup {
    createTestFile {
        set apa 10
        expr {$apa == {$bepa}}
    }
} -body {
    execTestFile
} -result {%%}

test nagelfar-7.1 {
    Command: upvar
} -setup {
    createTestFile {
        upvar 1 bepa
    }
} -body {
    execTestFile
} -result {%%Line   2: E Wrong number of arguments (2) to "upvar"}

test nagelfar-7.2 {
    Command: upvar
} -setup {
    createTestFile {
        set x hej
        upvar 1 bepa $x
    }
} -body {
    execTestFile
} -result {%%Line   3: N Suspicious upvar variable "$x"}

test nagelfar-7.3 {
    Command: upvar
} -setup {
    createTestFile {
        set x hej
        upvar $x $x bepa
    }
} -body {
    execTestFile
} -result {%%Line   3: N Non constant level to upvar: "$x"}

test nagelfar-8.1 {
    Variable handling
} -constraints knownbug -setup {
    createTestFile {
        proc hej {x y} {
            global item
            list item($x,$y)
        }
    }
} -body {
    execTestFile
} -result {should detect missing dollar}

test nagelfar-9.1 {
    if statement, as comment
} -setup {
    createTestFile {
        if 0 {
            set y $x
        }
        expr {$y}
    }
} -body {
    execTestFile
} -result {%%Line   5: E Unknown variable "y"}

test nagelfar-9.2 {
    if statement, as comment
} -setup {
    createTestFile {
        if 0 then {
            set y $x
        }
        expr {$y}
    }
} -body {
    execTestFile
} -result {%%Line   5: E Unknown variable "y"}

test nagelfar-10.1 {
    Brace alignment 
} -constraints { 
    knownbug
} -setup {
    createTestFile "
        cmd xx yy \\
            apa {
               hejsan
            }
    "
} -body {
    execTestFile
} -result {%%}

test nagelfar-10.2 {
    Brace alignment 
} -setup {
    createTestFile {
        list xx yy {
            hejsan
         }
    }
} -body {
    execTestFile
} -result {%%Line   4: N Close brace not aligned with line 2 (8 9)}

test nagelfar-11.1 {
    Line numbers
} -setup {
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
} -body {
    execTestFile
} -result {^%%Line\s+3:.*Line\s+4:.*Line\s+10:} -match regexp

test nagelfar-12.1 {
    Comments, bad in switch
} -setup {
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
} -body {
    execTestFile
} -result "%%Line   6: W Switch pattern starting with #. This could be a bad comment.*" -match glob

test nagelfar-12.2 {
    Comments, bad in list
} -setup {
    createTestFile {
        array set apa {
            elem1 val1
            # A bad comment
            elem2 val2
        }
    }
} -body {
    execTestFile
} -result "%%Line   4: N Suspicious \# char. Possibly a bad comment."

test nagelfar-12.3 {
    Comments, bad in list
} -setup {
    createTestFile {
        miffo apa {
            elem1 val1
            # A bad comment
            elem2 val2
        }
        proc miffo {a b} {}
    }
} -body {
    execTestFile -flags -2pass
} -result "%%Line   4: N Suspicious \# char. Possibly a bad comment."

file delete _testfile_
