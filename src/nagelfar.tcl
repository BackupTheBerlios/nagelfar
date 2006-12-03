#!/bin/sh
#----------------------------------------------------------------------
#  nagelfar.tcl, a syntax checker for Tcl.
#  Copyright (c) 1999-2005, Peter Spjuth  (peter.spjuth@space.se)
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; see the file COPYING.  If not, write to
#  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
#  Boston, MA 02111-1307, USA.
#
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set debug 0
package require Tcl 8.4

package provide app-nagelfar 1.0
set version "Version 1.1.6 2006-12-03"

set thisScript [file normalize [file join [pwd] [info script]]]
set thisDir    [file dirname $thisScript]

# Follow any link
set tmplink $thisScript
while {[file type $tmplink] == "link"} {
    set tmplink [file readlink $tmplink]
    set tmplink [file normalize [file join $thisDir $tmplink]]
    set thisDir [file dirname $tmplink]
}
unset tmplink

# Search where the script is to be able to place e.g. ctext there.
if {[info exists ::starkit::topdir]} {
    lappend auto_path [file dirname [file normalize $::starkit::topdir]]
} else {
    lappend auto_path $thisDir
}

#####################
# Syntax check engine
#####################

# Arguments to many procedures:
# index     : Index of the start of a string or command.
# cmd       : Command
# argv      : List of arguments
# wordstatus: List of status for the words in argv
# indices   : List of indices where every word in argv starts
# knownVars : An array that keeps track of variables known in this scope

# Interpretation of wordstatus:
# 1 constant
# 2 braced
# 4 quoted
# 8 {*}-expanded

# Moved out message handling to make it more flexible
proc echo {str {info 0}} {
    if {[info exists ::Nagelfar(resultWin)]} {
        $::Nagelfar(resultWin) configure -state normal
        $::Nagelfar(resultWin) insert end $str\n [lindex {{} info} $info]
        $::Nagelfar(resultWin) configure -state disabled
    } elseif {$::Nagelfar(embedded)} {
        lappend ::Nagelfar(chkResult) $str
    } else {
        puts stdout $str
    }
    update
}

# Debug output
proc decho {str} {
    if {[info exists ::Nagelfar(resultWin)]} {
        $::Nagelfar(resultWin) configure -state normal
        $::Nagelfar(resultWin) insert end $str\n error
        $::Nagelfar(resultWin) configure -state disabled
    } else {
        puts stderr $str
    }
    update
}

# Error message from program, not from syntax check
proc errEcho {msg} {
    if {$::Nagelfar(gui)} {
        tk_messageBox -title "Nagelfar Error" -type ok -icon error \
                -message $msg
    } else {
        puts stderr $msg
    }
}

# Standard error message.
# severity : How severe a message is E/W/N for Error/Warning/Note
proc errorMsg {severity msg i} {
    if {[info exists ::Nagelfar(currentMessage)] && \
            $::Nagelfar(currentMessage) != ""} {
        lappend ::Nagelfar(messages) [list $::Nagelfar(currentMessageLine) \
                $::Nagelfar(currentMessage)]
    }

    set ::Nagelfar(currentMessage) ""
    switch $severity {
        E {}
        W { if {$::Prefs(severity) == "E"} return }
        N { if {$::Prefs(severity) != "N"} return }
        default {
            decho "Internal error: Bad severity '$severity' passed to errorMsg"
            return
        }
    }

    set pre ""
    if {$::currentFile != ""} {
        set pre "$::currentFile: "
    }
    set line [calcLineNo $i]
    set pre "${pre}Line [format %3d $line]: $severity "
    set ::Nagelfar(indent) [string repeat " " [string length $pre]]
    set ::Nagelfar(currentMessage) $pre$msg
    set ::Nagelfar(currentMessageLine) $line
}

# Continued message. Used to give extra info after an error.
proc contMsg {msg {i {}}} {
    if {$::Nagelfar(currentMessage) == ""} return
    append ::Nagelfar(currentMessage) "\n" $::Nagelfar(indent)
    if {$i != ""} {
        regsub -all {%L} $msg [calcLineNo $i] msg
    }
    append ::Nagelfar(currentMessage) $msg
}

# Initialize message handling.
proc initMsg {} {
    set ::Nagelfar(messages) {}
    set ::Nagelfar(currentMessage) ""
    set ::Nagelfar(commentbrace) {}
}

# Called after a file has been parsed, to flush messages
proc flushMsg {} {
    if {[info exists ::Nagelfar(currentMessage)] && \
            $::Nagelfar(currentMessage) != ""} {
        lappend ::Nagelfar(messages) [list $::Nagelfar(currentMessageLine) \
                $::Nagelfar(currentMessage)]
    }
    set msgs [lsort -integer -index 0 $::Nagelfar(messages)]
    foreach msg $msgs {
        set text [lindex $msg 1]
        set print 1
        foreach filter $::Nagelfar(filter) {
            if {[string match $filter $text]} {
                set print 0
                break
            }
        }
        if {$print} {
            echo [lindex $msg 1]
        }
    }
}

# Report any unbalanced braces in comments that have been noticed
proc reportCommentBrace {fromIx toIx} {
    set fromLn [calcLineNo $fromIx]
    set toLn   [calcLineNo $toIx]
    set new {}
    foreach {n lineNo} $::Nagelfar(commentbrace) {
        if {$fromLn <= $lineNo && $lineNo <= $toLn} {
            contMsg "Unbalanced brace in comment in line $lineNo."
        } else {
            lappend new $n $lineNo
        }
    }
    # Only report it once
    set ::Nagelfar(commentbrace) $new
}

# Trim a string to fit within a length.
proc trimStr {str {len 10}} {
    set str [string trim $str]
    if {[string length $str] > $len} {
        set str [string range $str 0 [expr {$len - 4}]]...
    }
    return $str
}

# Test for comments with unmatched braces.
proc checkPossibleComment {str lineNo} {
    # Count braces
    set n1 [llength [split $str \{]]
    set n2 [llength [split $str \}]]
    if {$n1 != $n2} {
        lappend ::Nagelfar(commentbrace) [expr {$n1 - $n2}] $lineNo
    }
}

# This is called when a comment is encountered.
# It allows syntax information to be stored in comments
proc checkComment {str index knownVarsName} {
    upvar $knownVarsName knownVars

    if {[string match "##nagelfar *" $str]} {
        set rest [string range $str 11 end]
        if {[catch {llength $rest}]} {
            errorMsg N "Bad list in ##nagelfar comment" $index
            return
        }
        if {[llength $rest] == 0} return
        set cmd [lindex $rest 0]
        set first [lindex $rest 1]
        set rest [lrange $rest 2 end]
        switch -- $cmd {
            syntax {
                set ::syntax($first) $rest
            }
            return {
                set ::return($first) $rest
            }
            subcmd {
                set ::subCmd($first) $rest
            }
            option {
                set ::option($first) $rest
            }
            variable {
                set type [join $rest]
                markVariable $first 1 "" 1 $index knownVars type
            }
            ignore -
            filter {
                # FIXA, syntax for several lines
                set line [calcLineNo $index]
                incr line
                switch -- $first {
                    N { addFilter "*Line *$line: N *[join $rest]*" }
                    W { addFilter "*Line *$line: \[NW\] *[join $rest]*" }
                    E { addFilter "*Line *$line:*[join $rest]*" }
                    default { addFilter "*Line *$line:*$first [join $rest]*" }
                }
            }
            default {
                errorMsg N "Bad type in ##nagelfar comment" $index
                return
            }
        }
    } elseif {[regexp {\#\s*(FRINK|PRAGMA):\s*nocheck} $str -> keyword]} {
        # Support Frink's inline comment
        set line [calcLineNo $index]
        incr line
        addFilter "*Line *$line:*"
    }

}

# Handle a stack of current namespaces.
proc currentNamespace {} {
    lindex $::Nagelfar(namespaces) end
}

proc pushNamespace {ns} {
    lappend ::Nagelfar(namespaces) $ns
}

proc popNamespace {} {
    set ::Nagelfar(namespaces) [lrange $::Nagelfar(namespaces) 0 end-1]
}

# Handle a stack of current procedures.
proc currentProc {} {
    lindex $::Nagelfar(procs) end
}

proc pushProc {p} {
    lappend ::Nagelfar(procs) $p
}

proc popProc {} {
    set ::Nagelfar(procs) [lrange $::Nagelfar(procs) 0 end-1]
}

# Return the index of the first non whitespace char following index "i".
proc skipWS {str len i} {
    set j [string length [string trimleft [string range $str $i end]]]
    return [expr {$len - $j}]
}

# Scan the string until the end of one word is found.
# When entered, i points to the start of the word.
# Returns the index of the last char of the word.
proc scanWord {str len index i} {
    set si1 $i
    set si2 $i
    set c [string index $str $i]

    if {$c eq "\{" && $::Nagelfar(allowExpand)} {
        if {[string range $str $i [expr {$i + 2}]] eq "{*}"} {
            set ni [expr {$i + 3}]
            set nc [string index $str $ni]
            if {![string is space $nc]} {
                # Non-space detected, it is expansion
                set c $nc
                set i $ni
                set si2 $i
            } else {
                errorMsg N "Standalone {*} can be confusing. I recommend \"*\"." $i
            }
        } elseif {[string range $str $i [expr {$i + 7}]] eq "{expand}"} {
            set ni [expr {$i + 8}]
            set nc [string index $str $ni]
            if {![string is space $nc]} {
                # Non-space detected, it is expansion
                set c $nc
                set i $ni
                set si2 $i
            }
        }
    }

    if {[string equal $c "\{"]} {
        set closeChar \}
        set charType brace
    } elseif {[string equal $c "\""]} {
        set closeChar \"
        set charType quote
    } else {
        set closeChar ""
    }

    if {![string equal $closeChar ""]} {
        for {} {$i < $len} {incr i} {
            # Search for closeChar
            set i [string first $closeChar $str $i]
            if {$i == -1} {
                # This should never happen since no incomplete lines should
                # reach this function.
                decho "Internal error: Did not find close char in scanWord.\
                        Line [calcLineNo $index]."
                return $len
            }
            set word [string range $str $si2 $i]
            if {[info complete $word]} {
                # Check for following whitespace
                set j [expr {$i + 1}]
                set nextchar [string index $str $j]
                if {$j == $len || [string is space $nextchar]} {
                    return $i
                }
                errorMsg E "Extra chars after closing $charType." \
                        [expr {$index + $i}]
                contMsg "Opening $charType of above was on line %L." \
                        [expr {$index + $si2}]
                # Extra info for this particular case
                if {$charType eq "brace" && $nextchar eq "\{"} {
                    contMsg "It might be a missing space between \} and \{"
                }
                # Switch over to scanning for whitespace
                incr i
                break
            }
        }
    }

    for {} {$i < $len} {incr i} {
        # Search for unescaped whitespace
        if {[regexp -start $i -indices {(^|[^\\])(\\\\)*\s} $str match]} {
            set i [lindex $match 1]
        } else {
            set i $len
        }
        if {[info complete [string range $str $si2 $i]]} {
            return [expr {$i - 1}]
        }
    }

    # Theoretically, no incomplete string should come to this function,
    # but some precaution is never bad.
    if {![info complete [string range $str $si2 end]]} {
        decho "Internal error in scanWord: String not complete.\
                Line [calcLineNo [expr {$index + $si1}]]."
        decho $str
        return -code break
    }
    return [expr {$i - 1}]
}

# Split a statement into words.
# Returns a list of the words, and puts a list with the indices
# for each word in indicesName.
proc splitStatement {statement index indicesName} {
    upvar $indicesName indices
    set indices {}

    set len [string length $statement]
    if {$len == 0} {
        return {}
    }
    set words {}
    set i 0
    # There should not be any leading whitespace in the string that
    # reaches this function. Check just in case.
    set i [skipWS $statement $len $i]
    if {$i != 0 && $i < $len} {
        decho "Internal error:"
        decho " Whitespace in splitStatement. [calcLineNo $index]"
    }
    # Comments should be descarded earlier
    if {[string equal [string index $statement $i] "#"]} {
        decho "Internal error:"
        decho " A comment slipped through to splitStatement. [calcLineNo $index]"
        return {}
    }
    while {$i < $len} {
        set si $i
        lappend indices [expr {$i + $index}]
        set i [scanWord $statement $len $index $i]
        lappend words [string range $statement $si $i]
        incr i
        set i [skipWS $statement $len $i]
    }
    return $words
}

# FIXA Options may be non constant.

# Look for options in a command's arguments.
# Check them against the list in the option database, if any.
# Returns a syntax string corresponding to the number of arguments "used".
# If 'pair' is set, all options should take a value.
proc checkOptions {cmd argv wordstatus indices {startI 0} {max 0} {pair 0}} {
    global option

    # How many is the limit imposed by the number of arguments?
    set maxa [expr {[llength $argv] - $startI}]
    # Pairs swallow an even number of args.
    if {$pair && ($maxa % 2) == 1} {
        incr maxa -1
    }

    if {$max == 0 || $maxa < $max} {
        set max $maxa
    }
    if {$maxa == 0} {
        return {}
    }
    set check [info exists option($cmd)]
    if {!$check && $::Nagelfar(dbpicky)} {
        errorMsg N "DB: Missing options for command $cmd" 0
    }
    set i 0
    set used 0
    set skip 0
    set skipSyn x
    set replaceSyn {}
    # Since in most cases startI is 0, I believe foreach is faster.
    foreach arg $argv ws $wordstatus index $indices {
	if {$i < $startI} {
	    incr i
	    continue
	}
        if {$skip} {
            set skip 0
            lappend replaceSyn $skipSyn
            set skipSyn x
	    incr used
	    continue
	}
	if {$max != 0 && $used >= $max} {
	    break
	}
	if {[string match "-*" $arg]} {
	    incr used
            lappend replaceSyn x
	    set skip $pair
	    if {($ws & 1) && $check} { # Constant
                set ix [lsearch -exact $option($cmd) $arg]
		if {$ix == -1} {
                    # Check ambiguity.
                    if {![regexp {[][?*]} $arg]} {
                        # Only try globbing if $arg is free from glob chars.
                        set match [lsearch -all -inline -glob $option($cmd) $arg*]
                    } else {
                        set match {}
                    }
                    if {[llength $match] == 0} {
                        errorMsg E "Bad option $arg to $cmd" $index
                        set item ""
                    } elseif {[llength $match] > 1} {
                        errorMsg E "Ambigous option for \"$cmd\",\
                                $arg -> [join $match /]" $index
                        set item ""
                    } else {
                        errorMsg W "Shortened option for \"$cmd\",\
                                $arg -> [lindex $match 0]" $index

                        set item "$cmd [lindex $match 0]"
                    }
                } else {
                    set item "$cmd [lindex $option($cmd) $ix]"
                }
                if {$item ne ""} {
                    if {[info exists option($item)]} {
                        set skip 1
                        if {[regexp {^[lnvc]$} $option($item)]} {
                            set skipSyn $option($item)
                        }
                    }
                }
	    }
	    if {[string equal $arg "--"]} {
                set skip 0
		break
	    }
	} else { # If not -*
	    break
	}
    }
    if {$skip} {
        errorMsg E "Missing value for last option." $index
    }
    #decho "options to $cmd : $replaceSyn"
    return $replaceSyn
}

# Make a list of a string. This is easy, just treat it as a list.
# But we must keep track of indices, so our own parsing is needed too.
proc splitList {str index iName} {
    upvar $iName indices

    # Make a copy to perform list operations on
    set lstr [string range $str 0 end]

    set indices {}
    if {[catch {set n [llength $lstr]}]} {
	errorMsg E "Bad list" $index
	return {}
    }
    # Parse the string to get indices for each element
    set escape 0
    set level 0
    set len [string length $str]
    set state whsp

    for {set i 0} {$i < $len} {incr i} {
	set c [string index $str $i]
	switch -- $state {
	    whsp { # Whitespace
		if {[string is space $c]} continue
		# End of whitespace, i.e. a new element
		if {[string equal $c "\{"]} {
		    set level 1
		    set state brace
                    lappend indices [expr {$index + $i + 1}]
		} elseif {[string equal $c "\""]} {
		    set state quote
                    lappend indices [expr {$index + $i + 1}]
		} else {
		    if {[string equal $c "\\"]} {
			set escape 1
		    }
		    set state word
                    lappend indices [expr {$index + $i}]
		}
	    }
	    word {
		if {[string equal $c "\\"]} {
		    set escape [expr {!$escape}]
		} else {
		    if {!$escape} {
			if {[string is space $c]} {
			    set state whsp
			    continue
			}
		    } else {
			set escape 0
		    }
		}
	    }
	    quote {
		if {[string equal $c "\\"]} {
		    set escape [expr {!$escape}]
		} else {
		    if {!$escape} {
			if {[string equal $c "\""]} {
			    set state whsp
			    continue
			}
		    } else {
			set escape 0
		    }
		}
	    }
	    brace {
		if {[string equal $c "\\"]} {
		    set escape [expr {!$escape}]
		} else {
		    if {!$escape} {
			if {[string equal $c "\{"]} {
			    incr level
			} elseif {[string equal $c "\}"]} {
			    incr level -1
			    if {$level <= 0} {
				set state whsp
			    }
			}
		    } else {
			set escape 0
		    }
		}
	    }
	}
    }

    if {[llength $indices] != $n} {
	# This should never happen.
        decho "Internal error: Length mismatch in splitList.\
                Line [calcLineNo $index]."
        decho "nindices: [llength $indices]  nwords: $n"
#        decho :$str:
        foreach l $lstr ix $indices {
            decho :$ix:[string range $l 0 10]:
        }
    }
    return $lstr
}

# Parse a variable name, check for existance
# This is called when a $ is encountered
# "i" points to the first char after $
# Returns the type of the variable
proc parseVar {str len index iName knownVarsName} {
    upvar $iName i $knownVarsName knownVars
    set si $i
    set c [string index $str $si]

    if {[string equal $c "\{"]} {
	# A variable ref starting with a brace always ends with next brace,
	# no exceptions that I know of
	incr si
	set ei [string first "\}" $str $si]
	if {$ei == -1} {
	    # This should not happen.
	    errorMsg E "Could not find closing brace in variable reference." \
                    $index
	}
	set i $ei
	incr ei -1
	set var [string range $str $si $ei]
	set vararr 0
	# check for an array
	if {[string equal [string index $str $ei] ")"]} {
	    set pi [string first "(" $str $si]
	    if {$pi != -1 && $pi < $ei} {
		incr pi -1
		set var [string range $str $si $pi]
		incr pi 2
		incr ei -1
		set varindex [string range $str $pi $ei]
		set vararr 1
		set varindexconst 1
	    }
	}
    } else {
	for {set ei $si} {$ei < $len} {incr ei} {
	    set c [string index $str $ei]
	    if {[string is wordchar $c]} continue
	    # :: is ok.
	    if {[string equal $c ":"]} {
		set c [string index $str [expr {$ei + 1}]]
		if {[string equal $c ":"]} {
		    incr ei
		    continue
		}
	    }
	    break
	}
	if {[string equal [string index $str $ei] "("]} {
	    # Locate the end of the array index
	    set pi $ei
	    set apa [expr {$si - 1}]
	    while {[set ei [string first ")" $str $ei]] != -1} {
		if {[info complete [string range $str $apa $ei]]} {
		    break
		}
		incr ei
	    }
	    if {$ei == -1} {
		# This should not happen.
		errorMsg E "Could not find closing parenthesis in variable\
                        reference." $index
		return
	    }
	    set i $ei
	    incr pi -1
	    set var [string range $str $si $pi]
	    incr pi 2
	    incr ei -1
	    set varindex [string range $str $pi $ei]
	    set vararr 1
	    set varindexconst [parseSubst $varindex \
                    [expr {$index + $pi}] type knownVars]
	} else {
	    incr ei -1
	    set i $ei
	    set var [string range $str $si $ei]
	    set vararr 0
	}
    }

    # By now:
    # var is the variable name
    # vararr is 1 if it is an array
    # varindex is the array index
    # varindexconst is 1 if the array index is a constant

    if {$::Prefs(noVar) || $var == ""} {
        return ""
    }

    if {[string match ::* $var]} {
	# Skip qualified names until we handle namespace better. FIXA
        # Handle types for constant names
        if {!$vararr} {
            set full $var
        } elseif {$varindexconst} {
            set full ${var}($varindex)
        } else {
            set full ""
        }
        if {$full ne "" && [info exists knownVars(type,$full)]} {
            return $knownVars(type,$full)
        }
	return ""
    }
    if {![info exists knownVars(known,$var)]} {
	errorMsg E "Unknown variable \"$var\"" $index
    }
    if {![info exists knownVars(set,$var)]} {
        set knownVars(read,$var) 1
        # Why was this here?? FIXA
        #if {[info exists knownVars(local,$var)]} {
        #    errorMsg E "Unknown variable \"$var\"" $index
        #}
    }
    if {$vararr && [info exists knownVars(type,$var\($varindex\))]} {
        return [set knownVars(type,$var\($varindex\))]
    }
    if {[info exists knownVars(type,$var)]} {
        return $knownVars(type,$var)
    }
    return ""
    # Make use of markVariable. FIXA
    # If it's a constant array index, maybe it should be checked? FIXA
}

# Check for substitutions in a word
# Check any variables referenced, and parse any commands within brackets.
# Returns 1 if the string is constant, i.e. no substitutions
# Returns 0 if any substitutions are present
proc parseSubst {str index typeName knownVarsName} {
    upvar $typeName type $knownVarsName knownVars

    set type ""

    # First do a quick check for $ or [
    # If the word ends in "]" and there is no "[" it is considered
    # suspicious and we continue checking.
    if {[string first \$ $str] == -1 && [string first \[ $str] == -1 && \
            [string index $str end] ne "\]"} {
	return 1
    }

    set result 1
    set len [string length $str]
    set escape 0
    set notype 0
    set types {}
    for {set i 0} {$i < $len} {incr i} {
        set c [string index $str $i]
        if {[string equal $c "\\"]} {
            set escape [expr {!$escape}]
            set notype 1
        } elseif {!$escape} {
	    if {[string equal $c "\$"]} {
		incr i
		lappend types [parseVar $str $len $index i knownVars]
		set result 0
	    } elseif {[string equal $c "\["]} {
		set si $i
		for {} {$i < $len} {incr i} {
		    if {[info complete [string range $str $si $i]]} {
			break
		    }
		}
		if {$i == $len} {
		    errorMsg E "URGA:$si\n:$str:\n:[string range $str $si end]:" $index
		}
		incr si
		incr i -1
		lappend types [parseBody [string range $str $si $i] \
                        [expr {$index + $si}] knownVars]
		incr i
		set result 0
	    } else {
                set notype 1
                if {[string equal $c "\]"] && $i == ($len - 1)} {
                    # Note unescaped bracket at end of word since it's
                    # likely to mean it should not be there.
                    errorMsg N "Unescaped end bracket" [expr {$index + $i}]
                }
            }
        } else {
            set escape 0
            set notype 1
        }
    }
    if {!$notype && [llength $types] == 1} {
        set type [lindex $types 0]
    }
    return $result
}

# Parse an expression
proc parseExpr {str index knownVarsName} {
    upvar $knownVarsName knownVars

    # First do a quick check for $ or [
    if {[string first "\$" $str] == -1 && [string first "\[" $str] == -1} {
        set exp $str
    } else {
        # This is similar to parseSubst, just that it also check for braces
        set exp ""
        set result 1
        set len [string length $str]
        set escape 0
        set brace 0
        for {set i 0} {$i < $len} {incr i} {
            set c [string index $str $i]
            if {[string equal $c "\\"]} {
                set escape [expr {!$escape}]
            } elseif {!$escape} {
                if {[string equal $c "\{"]} {
                    incr brace
                } elseif {[string equal $c "\}"]} {
                    if {$brace > 0} {
                        incr brace -1
                    }
                } elseif {$brace == 0} {
                    if {[string equal $c "\$"]} {
                        incr i
                        parseVar $str $len $index i knownVars
                        append exp {$dummy}
                        continue
                    } elseif {[string equal $c "\["]} {
                        set si $i
                        for {} {$i < $len} {incr i} {
                            if {[info complete [string range $str $si $i]]} {
                                break
                            }
                        }
                        if {$i == $len} {
                            errorMsg E "URGA:$si\n:$str:\n:[string range $str $si end]:" $index
                        }
                        incr si
                        incr i -1
                        parseBody [string range $str $si $i] \
                                [expr {$index + $si}] knownVars
                        incr i
                        append exp {$dummy}
                        continue
                    }
                }
            } else {
                set escape 0
            }
            append exp $c
        }
    }

    # The above have replaced any variable substitution or command
    # substitution in the expression by "$dummy"
    set dummy 1

    # This uses [expr] to do the checking which means that the checking
    # can't recognise anything that differs from the Tcl version Nagelfar
    # is run with. For example, the new operators in 8.4 "eq" and "ne"
    # will be accepted even if the database was generated using an older
    # Tcl version.  A small problem and hard to fix, so I'm ignoring it.

    if {[catch [list expr $exp] msg]} {
        regsub {syntax error in expression.*:\s+} $msg {} msg
        if {[string match "*divide by zero*" $msg]} return
        errorMsg E "Bad expression: $msg" $index
    }
}

# This is to detect bad comments in constant lists.
# This will cause messages if there are comments in blocks
# that are not recognised as code.
proc checkForComment {word index} {
    # Check for "#"
    set si 0
    while {[set si [string first \# $word $si]] >= 0} {
        # Is it first in a line?
        if {[string index $word [expr {$si - 1}]] eq "\n"} {
            errorMsg N "Suspicious \# char. Possibly a bad comment." \
                    [expr {$index + $si}]
            break
        }
        incr si
    }
}

# List version of checkForComment
proc checkForCommentL {words wordstatus indices} {
    foreach word $words ws $wordstatus i $indices {
        if {$ws & 2} { # Braced
            checkForComment $word $i
        }
    }
}

# A "macro" for checkCommand to print common error message
# It should not be called from anywhere else.
proc WA {} {
    upvar "cmd" cmd "index" index "argc" argc "argv" argv "indices" indices
    errorMsg E "Wrong number of arguments ($argc) to \"$cmd\"" $index

    set t 1
    set line [calcLineNo $index]
    foreach ix $indices {
        set aline [calcLineNo $ix]
        if {$aline != $line} {
            contMsg "Argument $t at line $aline"
        }
        incr t
    }
}

# Check a command that have a syntax defined in the database
# 'firsti' says at which index in argv et.al. the arguments begin.
# Returns the return type of the command
proc checkCommand {cmd index argv wordstatus wordtype indices {firsti 0}} {
    upvar "constantsDontCheck" constantsDontCheck "knownVars" knownVars

    set argc [llength $argv]
    set syn $::syntax($cmd)
    set type ""
    if {[info exists ::return($cmd)]} {
        set type $::return($cmd)
        #puts T:$cmd:$type
    }
#    decho "Checking $cmd against syntax $syn"

    # Check if the syntax definition has multiple entries
    if {[string index [lindex $syn 0] end] == ":"} {
        set na [expr {$argc - $firsti}]
        set newsyn {}
        set state search
        foreach tok $syn {
            if {$state == "search"} {
                if {$tok == ":" || $tok == "${na}:"} {
                    set state copy
                }
            } elseif {$state == "copy"} {
                if {[string index $tok end] == ":"} {
                    break
                }
                lappend newsyn $tok
            }
        }
        if {[llength $newsyn] == 0} {
            echo "Can't parse syntax definition for \"$cmd\": \"$syn\""
            return $type
        }
        set syn $newsyn
    }

    if {[string is integer -strict $syn]} {
	if {($argc - $firsti) != $syn} {
	    WA
	}
        checkForCommentL $argv $wordstatus $indices
	return $type
    } elseif {[string equal [lindex $syn 0] "r"]} {
	if {($argc - $firsti) < [lindex $syn 1]} {
	    WA
	} elseif {[llength $syn] >= 3 && ($argc - $firsti) > [lindex $syn 2]} {
	    WA
	}
        checkForCommentL $argv $wordstatus $indices
	return $type
    }

    # Calculate the minimum number of arguments needed by non-optional
    # tokens. If this is the same number as the actual arguments, we
    # know that no optional tokens may consume anything.
    # This prevents e.g. options checking on arguments that cannot be
    # options due to their placement.

    if {![info exists ::cacheMinArgs($syn)]} {
        set minargs 0
        set i 0
        set last [llength $syn]
        foreach token $syn {
            incr i
            if {[string length $token] <= 1} {
                incr minargs
            } else {
                set last $i
            }
        }
        set ::cacheEndArgs($syn) [expr {[llength $syn] - $last}]
        set ::cacheMinArgs($syn) $minargs
    }
    set anyOptional  [expr {($argc - $firsti) > $::cacheMinArgs($syn)}]
    set lastOptional [expr {$argc - $::cacheEndArgs($syn)}]

    # Treat syn as a stack. That way a token can replace itself without
    # increasing i and thus hand over checking to another token.

    set i $firsti
    while {[llength $syn] > 0} {
        set token [lindex $syn 0]
        set syn [lrange $syn 1 end]

	set tok [string index $token 0]
	set mod [string index $token 1]
	# Basic checks for modifiers
	switch -- $mod {
	    "" { # No modifier, and out of arguments, is an error
		if {$i >= $argc} {
		    set i -1
		    break
		}
	    }
	    "*" - "." { # No more arguments is ok.
		if {$i >= $argc} {
		    set i $argc
		    break
		}
	    }
	}
        # Is it optional and there can't be any optional?
        if {$mod ne "" && !$anyOptional} {
            continue
        }
	switch -- $tok {
	    x - X {
		# x* matches anything up to the end.
		if {[string equal $mod "*"]} {
                    checkForCommentL [lrange $argv $i end] \
                            [lrange $wordstatus $i end] \
                            [lrange $indices $i end]
		    set i $argc
		    break
		}
		if {![string equal $mod "?"] || $i < $argc} {
                    # Check braced for comments
                    if {([lindex $wordstatus $i] & 2) && $tok != "X"} {
                        checkForComment [lindex $argv $i] [lindex $indices $i]
                    }
		    incr i
		}
	    }
            E -
	    e { # An expression
		if {![string equal $mod ""]} {
		    echo "Modifier \"$mod\" is not supported for \"$tok\" in\
                            syntax for $cmd."
		}
		if {([lindex $wordstatus $i] & 1) == 0} { # Non constant
                    if {$tok == "E"} {
                        errorMsg W "No braces around expression in\
                                $cmd statement." [lindex $indices $i]
                    } elseif {$::Prefs(warnBraceExpr)} {
                        # Allow pure command substitution if warnBraceExpr == 1
                        if {$::Prefs(warnBraceExpr) == 2 || \
                                [string index [lindex $argv $i] 0] != "\[" || \
                                [string index [lindex $argv $i] end] != "\]" } {
                            errorMsg W "No braces around expression in\
                                    $cmd statement." [lindex $indices $i]
                        }
                    }
                } elseif {[lindex $wordstatus $i] & 2} { # Braced
                    # FIXA: This is not a good check in e.g. a catch.
                    #checkForComment [lindex $argv $i] [lindex $indices $i]
                }
		parseExpr [lindex $argv $i] [lindex $indices $i] knownVars
		incr i
	    }
	    c { # A code block
		if {![string equal $mod ""]} {
		    echo "Modifier \"$mod\" is not supported for \"c\" in\
                            syntax for $cmd."
		}
		if {([lindex $wordstatus $i] & 1) == 0} { # Non constant
                    # No braces around non constant code.
                    # Special case: [list ...]
                    set arg [lindex $argv $i]
                    if {[string match {\[list*} $arg]} {
                        # FIXA: Check the code
                        #echo "(List code)"
                    } else {
                        errorMsg W "No braces around code in $cmd\
                                statement." [lindex $indices $i]
                    }
		} else {
                    set ::instrumenting([lindex $indices $i]) 1
                }
		parseBody [lindex $argv $i] [lindex $indices $i] knownVars
		incr i
	    }
	    s { # A subcommand
		if {![string equal $mod ""] && ![string equal $mod "."]} {
		    echo "Modifier \"$mod\" is not supported for \"s\" in\
                            syntax for $cmd."
		}
		lappend constantsDontCheck $i
		if {([lindex $wordstatus $i] & 1) == 0} { # Non constant
		    errorMsg N "Non static subcommand to \"$cmd\"" \
                            [lindex $indices $i]
		} else {
		    set arg [lindex $argv $i]
		    if {[info exists ::subCmd($cmd)]} {
			if {[lsearch $::subCmd($cmd) $arg] == -1} {
                            set ix [lsearch -glob $::subCmd($cmd) $arg*]
                            if {$ix == -1} {
                                errorMsg E "Unknown subcommand \"$arg\" to \"$cmd\""\
                                        [lindex $indices $i]
                            } else {
                                # Check ambiguity.
                                set match [lsearch -all -inline -glob \
                                        $::subCmd($cmd) $arg*]
                                if {[llength $match] > 1} {
                                    errorMsg E "Ambigous subcommand for\
                                            \"$cmd\", $arg ->\
                                            [join $match /]" \
                                            [lindex $indices $i]
                                } elseif {$::Prefs(warnShortSub)} {
                                    # Report shortened subcmd?
                                    errorMsg W "Shortened subcommand for\
                                            \"$cmd\", $arg ->\
                                            [lindex $match 0]" \
                                            [lindex $indices $i]
                                }
                                set arg [lindex $::subCmd($cmd) $ix]
                            }
			}
		    } elseif {$::Nagelfar(dbpicky)} {
                        errorMsg N "DB: Missing subcommands for $cmd" 0
                    }
		    # Are there any syntax definition for this subcommand?
		    set sub "$cmd $arg"
		    if {[info exists ::syntax($sub)]} {
			set stype [checkCommand $sub $index $argv $wordstatus \
                                $wordtype \
                                $indices [expr {[llength $sub] - 1}]]
                        if {$stype != ""} {
                            set type $stype
                        }
			set i $argc
			break
		    } elseif {$::Nagelfar(dbpicky)} {
                        #errorMsg N "DB: Missing syntax for subcommand $sub" 0
                    }
		}
		incr i
	    }
	    l -
	    v -
	    n { # A call by name
                if {[string equal $mod "?"]} {
		    if {$i >= $argc} {
			set i $argc
			break
		    }
		}
		set ei [expr {$i + 1}]
		if {[string equal $mod "*"]} {
		    set ei $lastOptional
		}
		while {$i < $ei} {
		    if {[string equal $tok "v"]} {
			# Check the variable
                        if {[string match ::* [lindex $argv $i]]} {
                            # Skip qualified names until we handle
                            # namespace better. FIXA
                        } elseif {[markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] [lindex $wordtype $i] \
                                2 [lindex $indices $i]\
                                knownVars vtype]} {
			    errorMsg E "Unknown variable \"[lindex $argv $i]\""\
                                    [lindex $indices $i]
			}
		    } elseif {[string equal $tok "n"]} {
			markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] [lindex $wordtype $i] 1 \
                                [lindex $indices $i] knownVars ""
		    } else {
			markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] [lindex $wordtype $i] 0 \
                                [lindex $indices $i] knownVars ""
		    }

		    lappend constantsDontCheck $i
		    incr i
		}
	    }
	    o {
                set max [expr {$lastOptional - $i}]
                if {![string equal $mod "*"]} {
                    set max 1
                }
                set oSyn [checkOptions $cmd $argv $wordstatus $indices $i $max]
                set used [llength $oSyn]
                if {$used == 0 && ($mod == "" || $mod == ".")} {
                    errorMsg E "Expected an option as argument $i to \"$cmd\"" \
                            [lindex $indices $i]
                    return $type
                }

                if {[lsearch -not $oSyn "x"] >= 0} {
                    # Feed the syntax back into the check loop
                    set syn [concat $oSyn $syn]
                } else {
                    incr i $used
                }
            }
	    p {
                set max [expr {$lastOptional - $i}]
                if {![string equal $mod "*"]} {
                    set max 2
                }
                set oSyn [checkOptions $cmd $argv $wordstatus $indices $i \
                        $max 1]
                set used [llength $oSyn]
                if {$used == 0 && ($mod == "" || $mod == ".")} {
                    errorMsg E "Expected an option as argument $i to \"$cmd\"" \
                            [lindex $indices $i]
                    return $type
                }
                if {[lsearch -not $oSyn "x"] >= 0} {
                    # Feed the syntax back into the check loop
                    set syn [concat $oSyn $syn]
                } else {
                    incr i $used
                }
	    }
	    default {
		echo "Unsupported token $token in syntax for $cmd"
	    }
	}
    }
    # Have we used up all arguments?
    if {$i != $argc} {
	WA
    }
    return $type
}

# Central function to handle known variable names.
# If check is 2, check if it is known, return 1 if unknown
# If check is 1, mark the variable as known and set
# If check is 0, mark the variable as known
proc markVariable {var ws wordtype check index knownVarsName typeName} {
    upvar $knownVarsName knownVars
    if {$typeName ne ""} {
        upvar $typeName type
    } else {
        set type ""
    }

    if {$::Prefs(noVar)} {
        set type ""
        return 0
    }

    set varBase $var
    set varArray 0
    set varIndex ""
    set varBaseWs $ws
    set varIndexWs $ws

    # is it an array?
    set i [string first "(" $var]
    if {$i != -1} {
	incr i -1
	set varBase [string range $var 0 $i]
	incr i 2
	set varIndex [string range $var $i end-1]
	# Check if the base is free from substitutions
	if {($varBaseWs & 1) == 0 && [regexp {^(::)?(\w+(::)?)+$} $varBase]} {
	    set varBaseWs 1
	}
	set varArray 1
    }

    # If the base contains substitutions it can't be checked.
    if {($varBaseWs & 1) == 0} {
        # Experimental foreach check FIXA
        if {[string match {$*} $var]} {
            set name [string range $var 1 end]
            if {[info exists ::foreachVar($name)]} {
                # Mark them as known instead
                foreach name $::foreachVar($name) {
                    markVariable $name 1 "" $check $index knownVars ""
                }
                #return 1
            }
        }
        if {$wordtype ne "varName"} {
            errorMsg N "Suspicious variable name \"$var\"" $index
        }
	return 0
    }

    if {$check == 2} {
        set type ""
	if {![info exists knownVars(known,$varBase)]} {
	    return 1
	}
	if {$varArray && ($varIndexWs & 1) && \
                [info exists knownVars(local,$varBase)]} {
	    if {![info exists knownVars(known,$var)]} {
		return 1
	    }
	}
	if {[info exists knownVars(type,$var)]} {
            set type $knownVars(type,$var)
        } else {
            set type $knownVars(type,$varBase)
        }
	return 0
    } else {
	if {![info exists knownVars(known,$varBase)]} {
            if {[currentProc] ne ""} {
                set knownVars(known,$varBase) 1
                set knownVars(local,$varBase) 1
                set knownVars(type,$varBase)  $type
            } else {
                set knownVars(known,$varBase) 1
                set knownVars(namespace,$varBase) [currentNamespace]
                set knownVars(type,$varBase)  $type
            }
        }
        if {1 || $type ne ""} {
            # Warn if changed?? FIXA
            set knownVars(type,$varBase) $type
        }
        if {$check == 1} {
            set knownVars(set,$varBase) 1
        }
        # If the array index is constant, mark the whole name
	if {$varArray && ($varIndexWs & 1)} {
	    if {![info exists knownVars(known,$var)]} {
		set knownVars(known,$var) 1
                set knownVars(type,$var)  $type
                if {[info exists knownVars(local,$varBase)]} {
                    set knownVars(local,$var) 1
                }
	    }
            if {$check == 1} {
                set knownVars(set,$var) 1
            }
	}
    }
}

# This is called when an unknown command is encountered.
# If not encountered it is stored to be checked last.
# Returns a list with a partial command where the first element
# is the resolved name with qualifier.
proc lookForCommand {cmd ns index} {
    # Get both the namespace and global possibility
    if {[string match "::*" $cmd]} {
        set cmd1 [string range $cmd 2 end]
        set cmd2 ""
    } elseif {$ns ne "__unknown__" } {
        set cmd1 "${ns}::$cmd"
        if {[string match "::*" $cmd1]} {
            set cmd1 [string range $cmd1 2 end]
        }
        set cmd2 $cmd
    } else {
        set cmd1 $cmd
        set cmd2 ""
    }

    #puts "MOO cmd '$cmd' ns '$ns' '$cmd1' '$cmd2'"
    if {[info exists ::knownAliases($cmd1)]} {
        return $::knownAliases($cmd1)
    }
    if {[info exists ::knownAliases($cmd2)]} {
        return $::knownAliases($cmd2)
    }

    if {[lsearch $::knownCommands $cmd1] >= 0} {
        return [list $cmd1]
    }
    if {$cmd2 != "" && [lsearch $::knownCommands $cmd2] >= 0} {
        return [list $cmd2]
    }
    if {[lsearch $::knownCommands $cmd] >= 0} {
        return [list $cmd]
    }

    lappend ::unknownCommands [list $cmd $cmd1 $cmd2 $index]
    return ""
}

# Parse one statement and check the syntax of the command
# Returns the return type of the statement
proc parseStatement {statement index knownVarsName} {
    upvar $knownVarsName knownVars
    set words [splitStatement $statement $index indices]
    if {[llength $words] == 0} {return}

    if {$::Nagelfar(firstpass)} {
        if {[lindex $words 0] eq "proc"} {
            # OK
        } elseif {[lindex $words 0] eq "namespace" && \
                [lindex $words 1] eq "eval" && \
                [llength $words] == 4 && \
                ![regexp {[][$\\]} [lindex $words 2]] && \
                ![regexp {^[{"]?\s*["}]?$} [lindex $words 3]]} {
            # OK
        } else {
            return ""
        }
    }

    set type ""
    set words2 {}
    set wordstatus {}
    set wordtype {}
    set indices2 {}
    foreach word $words index $indices {
        set ws 0
        set wtype ""
        if {[string length $word] > 3 && [string match "{\\*}*" $word]} {
            set ws 8
            set word [string range $word 3 end]
        } elseif {[string length $word] > 8 && \
                [string match "{expand}*" $word]} {
            set ws 8
            set word [string range $word 8 end]
            if {$::Nagelfar(allowOldExpand)} {
                errorMsg W "Obsolete {expand} syntax. Use {*}." $index
            } else {
                errorMsg E "Obsolete {expand} syntax. Use {*}." $index
            }
        }
        set char [string index $word 0]
        if {[string equal $char "\{"]} {
            incr ws 3 ;# Braced & constant
            set word [string range $word 1 end-1]
	    incr index
        } else {
            if {[string equal $char "\""]} {
                set word [string range $word 1 end-1]
		incr index
		incr ws 4
            }
            if {[parseSubst $word $index wtype knownVars]} {
                # A constant
                incr ws 1
            }
        }
        if {($ws & 9) == 9} {
            # An expanded constant, unlikely but we can just as well handle it
            if {[catch {llength $word}]} {
                errorMsg E "Expanded word is not a valid list." $index
            } else {
                foreach apa $word {
                    lappend words2 $apa
                    lappend wordstatus 1
                    lappend wordtype ""
                    # For now I don't bother to track correct indices
                    lappend indices2 $index
                }
            }
        } else {
            lappend words2 $word
            lappend wordstatus $ws
            lappend wordtype $wtype
            lappend indices2 $index
        }
    }

    set cmd [lindex $words2 0]
    set index [lindex $indices2 0]
    set cmdtype [lindex $wordtype 0]
    set cmdws [lindex $wordstatus 0]

    # Expanded command, nothing to check...
    if {($cmdws & 8)} {
        return
    }

    # If the command contains substitutions we can not determine
    # which command it is, so we skip it, unless the type is known
    # to be an object.
    if {($cmdws & 1) == 0} {
        if {[string match "_obj,*" $cmdtype]} {
            set cmd $cmdtype
        } else {
            # Detect missing space after command
            if {[regexp {^[\w:]+\{} $cmd]} {
                errorMsg W "Suspicious command \"$cmd\"" $index
            }
            return
        }
    }

    set argv [lrange $words2 1 end]
    set wordtype   [lrange $wordtype 1 end]
    set wordstatus [lrange $wordstatus 1 end]
    set indices [lrange $indices2 1 end]
    set argc [llength $argv]

    # FIXA: handle {expand} better
    foreach ws $wordstatus {
        if {$ws & 8} {
            return
        }
    }

    # The parsing below can pass information to the constants checker
    # This list primarily consists of args that are supposed to be variable
    # names without a $ in front.
    set noConstantCheck 0
    set constantsDontCheck {}

    # Any command that can't be described in the syntax database
    # have their own special check implemented here.
    # Any command that can be checked by checkCommand should
    # be in the syntax database.
    switch -glob -- $cmd {
	proc {
	    if {$argc != 3} {
                if {!$::Nagelfar(firstpass)} { # Messages in second pass
                    WA
                }
		return
	    }
	    # Skip the proc if any part of it is not constant
            # FIXA: Maybe accept substitutions as part of namespace?
            foreach ws $wordstatus {
                if {($ws & 1) == 0} {
                    errorMsg N "Non constant argument to proc \"[lindex $argv 0]\".\
                            Skipping." $index
                    return
                }
	    }
            if {$::Nagelfar(gui)} {progressUpdate [calcLineNo $index]}
	    parseProc $argv $indices
            set noConstantCheck 1
	}
	.* { # FIXA, check code in any -command.
             # Even widget commands should be checked.
	     # Maybe in checkOptions ?
	    return
	}
	bind { # FIXA, check the code
	    return
	}
	global {
	    foreach var $argv ws $wordstatus {
		if {$ws & 1} {
                    set knownVars(known,$var)     1
                    set knownVars(namespace,$var) ""
                    set knownVars(type,$var)      ""
		} else {
		    errorMsg N "Non constant argument to $cmd: $var" $index
		}
	    }
            set noConstantCheck 1
	}
	variable {
	    set i 0
	    foreach {var val} $argv {ws1 ws2} $wordstatus {
                set ns [currentNamespace]
                if {[regexp {^(.*)::([^:]+)$} $var -> root var]} {
                    set ns $root
                    if {[string match "::*" $ns]} {
                        set ns [string range $ns 2 end]
                    }
                }
                if {$ns ne "__unknown__"} {
                    if {$ws1 & 1} {
                        set knownVars(namespace,$var) $ns
                    }
                    if {($ws1 & 1) || [string is wordchar $var]} {
                        set knownVars(known,$var) 1
                        set knownVars(type,$var)  ""
                        if {$i < $argc - 1} {
                            set knownVars(set,$var) 1
                        }
                        lappend constantsDontCheck $i
                    } else {
                        errorMsg N "Non constant argument to $cmd: $var" $index
                    }
                }
		incr i 2
	    }
	}
	upvar {
            if {$argc < 2} {
                WA
                return
            }
            set level [lindex $argv 0]
            set oddA [expr {$argc % 2 == 1}]
            set hasLevel 0
            if {[lindex $wordstatus 0] & 1} {
                # Is it a level ?
                if {[regexp {^[\\\#0-9]} $level]} {
                    if {!$oddA} {
                        WA
                        return
                    }
                    set hasLevel 1
                } else {
                    if {$oddA} {
                        WA
                        return
                    }
                    set level 1
                }
            } else {
                # Assume it is not a level unless odd number of args.
                if {$oddA} {
                    # Warn here? FIXA
                    errorMsg N "Non constant level to $cmd: \"$level\"" $index
                    set hasLevel 1
                    set level ""
                } else {
                    set level 1
                }
            }
            if {$hasLevel} {
                set tmp [lrange $argv 1 end]
                set tmpWS [lrange $wordstatus 1 end]
                set i 2
            } else {
                set tmp $argv
                set tmpWS $wordstatus
                set i 1
            }

	    foreach {other var} $tmp {wsO wsV} $tmpWS {
                if {($wsV & 1) == 0} {
                    # The variable name contains substitutions
                    errorMsg N "Suspicious upvar variable \"$var\"" $index
                } else {
                    set knownVars(known,$var) 1
                    set knownVars(type,$var)  ""
                    lappend constantsDontCheck $i
                    if {$other eq $var} { # Allow "upvar xx xx" construct
                        lappend constantsDontCheck [expr {$i - 1}]
                    }
                    if {($wsO & 1) == 0} {
                        # Is the other name a simple var subst?
                        if {[regexp {^\$([\w()]+)$}  $other -> other] || \
                            [regexp {^\${([^{}]*)}$} $other -> other]} {
                            if {[info exists knownVars(known,$other)]} {
                                if {$level == 1} {
                                    set knownVars(upvar,$other) $var
                                } elseif {$level eq "#0"} {
                                    # FIXA: level #0 for global
                                    set knownVars(upvar,$other) $var
                                    set knownVars(set,$var) 1 ;# FIXA?
                                }
                            }
                        }
                    }
                }
		incr i 2
	    }
	}
	set {
	    # Set gets a different syntax string depending on the
	    # number of arguments.
	    if {$argc == 1} {
                # Check the variable
                if {[string match ::* [lindex $argv 0]]} {
                    # Skip qualified names until we handle
                    # namespace better. FIXA
                } elseif {[markVariable [lindex $argv 0] \
                        [lindex $wordstatus 0] [lindex $wordtype 0] \
                        2 [lindex $indices 0] knownVars wtype]} {
                    errorMsg E "Unknown variable \"[lindex $argv 0]\""\
                            [lindex $indices 0]
                }
            } elseif {$argc == 2} {
                set wtype [lindex $wordtype 1]
                markVariable [lindex $argv 0] \
                        [lindex $wordstatus 0] [lindex $wordtype 0] \
                        1 [lindex $indices 0] \
                        knownVars wtype
            } else {
		WA
		set wtype ""
	    }
            lappend constantsDontCheck 0
            set type $wtype
	}
	foreach {
	    if {$argc < 3 || ($argc % 2) == 0} {
		WA
		return
	    }
	    for {set i 0} {$i < $argc - 1} {incr i 2} {
		if {[lindex $wordstatus $i] == 0} {
		    errorMsg W "Non constant variable list to foreach\
                            statement." [lindex $indices $i]
		    # FIXA, maybe abort here?
		}
		lappend constantsDontCheck $i
		foreach var [lindex $argv $i] {
		    markVariable $var 1 "" 1 $index knownVars ""
		}
	    }
            # FIXA: Experimental foreach check...
            # A special case for looping over constant lists
            set varsAdded {}
            foreach {varList valList} [lrange $argv 0 end-1] \
                    {varWS valWS} [lrange $wordstatus 0 end-1] {
                if {($varWS & 1) && ($valWS & 1)} {
                    set fVars {}
                    foreach fVar $varList {
                        set ::foreachVar($fVar) {}
                        lappend fVars apaV($fVar)
                        lappend varsAdded $fVar
                    }
                    foreach $fVars $valList {
                        foreach fVar $varList {
                            ##nagelfar variable apaV
                            lappend ::foreachVar($fVar) $apaV($fVar)
                        }
                    }
                }
            }

            if {([lindex $wordstatus end] & 1) == 0} {
                errorMsg W "No braces around body in foreach\
                        statement." $index
	    }
            set ::instrumenting([lindex $indices end]) 1
	    set type [parseBody [lindex $argv end] [lindex $indices end] \
                    knownVars]
            # Clean up
            foreach fVar $varsAdded {
                catch {unset ::foreachVar($fVar)}
            }
	}
	if {
	    if {$argc < 2} {
		WA
		return
	    }
	    # Build a syntax string that fits this if statement
	    set state expr
	    set ifsyntax {}
            foreach arg $argv ws $wordstatus index $indices {
		switch -- $state {
                    skip {
                        # This will behave bad with "if 0 then then"...
                        lappend ifsyntax X
			if {![string equal $arg then]} {
                            set state else
			}
                        continue
                    }
		    then {
			set state body
			if {[string equal $arg then]} {
			    lappend ifsyntax x
			    continue
			}
		    }
		    else {
			if {[string equal $arg elseif]} {
			    set state expr
			    lappend ifsyntax x
			    continue
			}
			set state lastbody
			if {[string equal $arg else]} {
			    lappend ifsyntax x
			    continue
			}
                        if {$::Prefs(forceElse)} {
                            errorMsg E "Badly formed if statement" $index
                            contMsg "Found argument '[trimStr $arg]' where\
                                    else/elseif was expected."
                            return
                        }
		    }
		}
		switch -- $state {
		    expr {
                        # Handle if 0 { ... } as a comment
                        if {[string is integer $arg] && $arg == 0} {
                            lappend ifsyntax x
                            set state skip
                        } else {
                            lappend ifsyntax e
                            set state then
                        }
		    }
		    lastbody {
			lappend ifsyntax c
			set state illegal
		    }
		    body {
			lappend ifsyntax c
			set state else
		    }
		    illegal {
			errorMsg E "Badly formed if statement" $index
			contMsg "Found argument '[trimStr $arg]' after\
                              supposed last body."
			return
		    }
		}
	    }
            # State should be "else" if there was no else clause or
            # "illegal" if there was one.
	    if {$state ne "else" && $state ne "illegal"} {
		errorMsg E "Badly formed if statement" $index
		contMsg "Missing one body."
		return
	    } elseif {$state eq "else"} {
                # Mark the missing else for instrumenting
                set ::instrumenting([expr {$index + [string length $arg]}]) 2
            }
#            decho "if syntax \"$ifsyntax\""
	    set ::syntax(if) $ifsyntax
	    checkCommand $cmd $index $argv $wordstatus $wordtype $indices
	}
	switch {
	    if {$argc < 2} {
		WA
		return
	    }
	    set i [llength [checkOptions $cmd $argv $wordstatus $indices]]
	    incr i
	    set left [expr {$argc - $i}]

	    if {$left < 1} {
		WA
		return
	    } elseif {$left == 1} {
		# One block. Split it into a list.
                # FIXA. Changing argv messes up the constant check.

		set arg [lindex $argv $i]
		set ws [lindex $wordstatus $i]
		set ix [lindex $indices $i]

                if {($ws & 1) == 1} {
                    set swargv [splitList $arg $ix swindices]
                    if {[llength $swargv] % 2 == 1} {
                        errorMsg E "Odd number of elements in last argument to\
                                switch." $ix
                        return
                    }
                    if {[llength $swargv] == 0} {
                        errorMsg W "Empty last argument to switch." $ix
                        return
                    }
                    set swwordst {}
                    foreach word $swargv {
                        lappend swwordst 1
                    }
                } else {
                    set swwordst {}
                    set swargv {}
                    set swindices {}
                }
	    } elseif {$left % 2 == 1} {
		WA
		return
	    } else {
		set swargv [lrange $argv $i end]
		set swwordst [lrange $wordstatus $i end]
		set swindices [lrange $indices $i end]
	    }
	    foreach {pat body} $swargv {ws1 ws2} $swwordst {i1 i2} $swindices {
		if {[string equal [string index $pat 0] "#"]} {
		    errorMsg W "Switch pattern starting with #.\
			    This could be a bad comment." $i1
		}
		if {[string equal $body -]} {
		    continue
		}
		if {($ws2 & 1) == 0} {
		    errorMsg W "No braces around code in switch\
                            statement." $i2
		}
                set ::instrumenting($i2) 1
		parseBody $body $i2 knownVars
	    }
	}
	expr { # FIXA
            # Take care of the standard case of a brace enclosed expr.
            if {$argc == 1 && ([lindex $wordstatus 0] & 1)} {
                 parseExpr [lindex $argv 0] [lindex $indices 0] knownVars
            } else {
                if {$::Prefs(warnBraceExpr)} {
                    errorMsg W "Expr without braces" [lindex $indices 0]
                }
            }
	}
	eval { # FIXA
            set noConstantCheck 1
	}
	interp {
            if {$argc < 1} {
                WA
                return
            }
            # Special handling of interp alias
            if {([lindex $wordstatus 0] & 1) && \
                    [string equal "alias" [lindex $argv 0]]} {
                if {$argc < 3} {
                    WA
                    return
                }
                # This should define a source in the current interpreter
                # with a known name.
                if {$argc >= 5 && \
                        ([lindex $wordstatus 1] & 1) && \
                        "" eq [lindex $argv 1] && \
                        ([lindex $wordstatus 2] & 1)} {
                    set newAlias [lindex $argv 2]
                    set aliasCmd {}
                    for {set t 4} {$t < $argc} {incr t} {
                        if {[lindex $wordstatus 1] & 1} {
                            lappend aliasCmd [lindex $argv $t]
                        } else {
                            lappend aliasCmd {}
                        }
                    }
                    set ::knownAliases($newAlias) $aliasCmd
                }
            }
            set type [checkCommand $cmd $index $argv $wordstatus \
                    $wordtype $indices]
            set noConstantCheck 1
	}
        package { # FIXA, take care of require
            set type [checkCommand $cmd $index $argv $wordstatus $wordtype \
                              $indices]
        }
	namespace { # FIXA, also handle import
            if {$argc < 1} {
                WA
                return
            }
            # Special handling of namespace eval
            if {([lindex $wordstatus 0] & 1) && \
                    [string match "ev*" [lindex $argv 0]]} {
                if {$argc < 3} {
                    if {!$::Nagelfar(firstpass)} { # Messages in second pass
                        WA
                    }
                    return
                }
                set arg1const [expr {[lindex $wordstatus 1] & 1}]
                set arg2const [expr {[lindex $wordstatus 2] & 1}]
                # Look for unknown parts
                if {[string is space [lindex $argv 2]]} {
                    # Empty body, do nothing
                } elseif {$arg2const && $argc == 3} {
                    if {$arg1const} {
                        set ns [lindex $argv 1]
                        if {![string match "::*" $ns]} {
                            set root [currentNamespace]
                            if {$root ne "__unknown__"} {
                                set ns ${root}::$ns
                            }
                        }
                    } else {
                        set ns __unknown__
                    }

                    pushNamespace $ns
                    parseBody [lindex $argv 2] [lindex $indices 2] knownVars
                    popNamespace
                } else {
                    if {!$::Nagelfar(firstpass)} { # Messages in second pass
                        errorMsg N "Only braced namespace evals are checked." \
                                [lindex $indices 0]
                    }
                }
            } else {
                set type [checkCommand $cmd $index $argv $wordstatus \
                                  $wordtype $indices]
            }
	}
	uplevel { # FIXA
            set noConstantCheck 1
	}
	default {
            set ns [currentNamespace]
	    if {$ns eq "" && [info exists ::syntax($cmd)]} {
		set type [checkCommand $cmd $index $argv $wordstatus \
                        $wordtype $indices]
	    } else {
                # Resolve commands in namespace
                set rescmd [lookForCommand $cmd $ns $index]
                if {[llength $rescmd] > 0 && \
                        [info exists ::syntax([lindex $rescmd 0])]} {
                    set cmd [lindex $rescmd 0]
                    # If lookForCommand returns a partial command, fill in
                    # all lists accordingly.
                    if {[llength $rescmd] > 1} {
                        set preargv {}
                        set prews {}
                        set prewt {}
                        set preindices {}
                        foreach arg [lrange $rescmd 1 end] {
                            lappend preargv $arg
                            lappend prews 1
                            lappend prewt ""
                            lappend preindices $index
                        }
                        set argv [concat $preargv $argv]
                        set wordstatus [concat $prews $wordstatus]
                        set wordtype [concat $prewt $wordtype]
                        set indices [concat $preindices $indices]
                    }
                    set type [checkCommand $cmd $index $argv $wordstatus \
                            $wordtype $indices]
                } elseif {$::Nagelfar(dbpicky)} {
                    errorMsg N "DB: Missing syntax for command $cmd" \
                            [lindex $indices 0]
                }
	    }
	}
    }

    if {$::Prefs(noVar)} {
        return
    }

    if {!$noConstantCheck} {
        # Check unmarked constants against known variables to detect missing $.
        # The constant is considered ok if within quotes.
        set i 0
        foreach ws $wordstatus var $argv {
            if {[info exists knownVars(known,$var)]} {
                if {($ws & 7) == 1 && [lsearch $constantsDontCheck $i] == -1} {
                    errorMsg W "Found constant \"$var\" which is also a\
                            variable." [lindex $indices $i]
                }
            }
            incr i
        }
    }
    return $type
}

# Split a script into individual statements
proc splitScript {script index statementsName indicesName knownVarsName} {
    upvar $statementsName statements $indicesName indices
    upvar $knownVarsName knownVars

    set statements {}
    set indices {}

    set tryline ""
    set newstatement 1
    set firstline ""
    string length $tryline

    set bracelevel 0

    foreach line [split $script \n] {
        # Here we must remember that "line" misses the \n that split ate.
        # When line is used below we add \n.
        # The extra \n generated on the last line does not matter.

        if {$bracelevel > 0} {
            # Manual brace parsing is entered when we know we are in
            # a braced block.  Return to ordinary parsing as soon
            # as a balanced brace is found.

            # Extract relevant characters
            foreach char [regexp -all -inline {\\.|{|}} $line] {
                if {$char eq "\{"} {
                    incr bracelevel
                } elseif {$char eq "\}"} {
                    incr bracelevel -1
                    if {$bracelevel <= 0} break
                }
            }
            if {$bracelevel > 0} {
                # We are still in a braced block so go on to the next line
		append tryline $line\n
		set line ""
                continue
            }
        }

        # An empty line can never cause completion, since at this stage
        # any backslash-newline has been removed.
        if {[string is space $line]} {
            if {$tryline eq ""} {
                incr index [string length $line]
                incr index
            } else {
                append tryline $line\n
            }
            continue
        }

        append line \n

	while {$line ne ""} {

            # Some extra checking on close braces to help finding
            # brace mismatches
            set closeBrace -1
            if {[string equal "\}" [string trim $line]]} {
                set closeBraceIx [expr {[string length $tryline] + $index}]
                if {$newstatement} {
                    errorMsg E "Unbalanced close brace found" $closeBraceIx
                    reportCommentBrace 0 $closeBraceIx
                }
                set closeBrace [wasIndented $closeBraceIx]
            }

	    # Move everything up to the next semicolon, newline or eof
            # to tryline

	    set i [string first ";" $line]
	    if {$i != -1} {
		append tryline [string range $line 0 $i]
                if {$newstatement} {
                    set newstatement 0
                    set firstline [string range $line 0 $i]
                }
		incr i
		set line [string range $line $i end]
                set splitSemi 1
	    } else {
		append tryline $line
                if {$newstatement} {
                    set newstatement 0
                    set firstline $line
                }
		set line ""
		set splitSemi 0
	    }
	    # If we split at a ; we must check that it really may be an end
	    if {$splitSemi} {
		# Comment lines don't end with ;
		#if {[regexp {^\s*#} $tryline]} {continue}
                if {[string equal [string index [string trimleft $tryline] 0]\
                        "#"]} continue

		# Look for \'s before the ;
		# If there is an odd number of \, the ; is ignored
		if {[string equal [string index $tryline end-1] "\\"]} {
		    set i [expr {[string length $tryline] - 2}]
		    set t $i
		    while {[string equal [string index $tryline $t] "\\"]} {
                        incr t -1
                    }
		    if {($i - $t) % 2 == 1} {continue}
		}
	    }
	    # Check if it's a complete line
	    if {[info complete $tryline]} {
                # Remove leading space, keep track of index.
		# Most lines will have no leading whitespace since
		# buildLineDb removes most of it. This takes care
		# of all remaining.
                if {[string is space -failindex i $tryline]} {
                    # Only space, discard the line
                    incr index [string length $tryline]
                    set tryline ""
                    set newstatement 1
                    continue
                } else {
                    if {$i != 0} {
                        set tryline [string range $tryline $i end]
                        incr index $i
                    }
                }
                if {[string equal [string index $tryline 0] "#"]} {
		    # Check and discard comments
		    checkComment $tryline $index knownVars
		} else {
		    if {$splitSemi} {
                        # Remove the semicolon from the statement
			lappend statements [string range $tryline 0 end-1]
		    } else {
			lappend statements $tryline
		    }
		    lappend indices $index
		}
                if {$closeBrace != -1} {
                    set tmp [wasIndented $index]
                    if {$tmp != $closeBrace} {
                        # Only do this if there is a free open brace
                        if {[regexp "\{\n" $tryline]} {
                            errorMsg N "Close brace not aligned with line\
                                    [calcLineNo $index] ($tmp $closeBrace)" \
                                    $closeBraceIx
                        }
                    }
                }
		incr index [string length $tryline]
		set tryline ""
                set newstatement 1
	    } elseif {$closeBrace == 0 && \
                    ![string match "namespace eval*" $tryline] && \
                    ![string match "if *" $tryline] && \
                    ![string match "*tcl_platform*" $tryline]} {
                # A close brace that is not indented is typically the end of
                # a global statement, like "proc".
                # If it does not end the statement, there is probably a
                # brace mismatch.
                # When inside a namespace eval block, this is probably ok.
                errorMsg N "Found non indented close brace that did not end\
                        statement." $closeBraceIx
                contMsg "This may indicate a brace mismatch."
            }
	}

        # If the line is complete except for a trailing open brace
        # we can switch to just scanning braces.
        # This could be made more general but since this is the far most
        # common case it's probably not worth complicating it.
        if {[string range $tryline end-2 end] eq " \{\n" && \
                    [info complete [string range $tryline 0 end-2]]} {
            set bracelevel 1
        }
    }
    # If tryline is non empty, it did not become complete
    if {[string length $tryline] != 0} {
        errorMsg E "Could not complete statement." $index

        # Experiment a little to give more info.
        if {[info complete $firstline\}]} {
            contMsg "One close brace would complete the first line"
            reportCommentBrace $index $index
        } elseif {[info complete $firstline\}\}]} {
            contMsg "Two close braces would complete the first line"
            reportCommentBrace $index $index
        }
        if {[info complete $firstline\"]} {
            contMsg "One double quote would complete the first line"
        }
        if {[info complete $firstline\]]} {
            contMsg "One close bracket would complete the first line"
        }

        set endIx [expr {$index + [string length $tryline] - 1}]
        set txt "the script body at line [calcLineNo $endIx]."
        if {[info complete $tryline\}]} {
            contMsg "One close brace would complete $txt"
            contMsg "Assuming completeness for further processing."
            reportCommentBrace $index $endIx
            lappend statements $tryline\}
            lappend indices $index
        } elseif {[info complete $tryline\}\}]} {
            contMsg "Two close braces would complete $txt"
            contMsg "Assuming completeness for further processing."
            reportCommentBrace $index $endIx
            lappend statements $tryline\}\}
            lappend indices $index
        }
        if {[info complete $tryline\"]} {
            contMsg "One double quote would complete $txt"
        }
        if {[info complete $tryline\]]} {
            contMsg "One close bracket would complete $txt"
        }
    }
}

# Returns the return type of the script
proc parseBody {body index knownVarsName} {
    upvar $knownVarsName knownVars

    #set ::instrumenting($index) 1

    # Cache the splitScript result to optimise 2-pass checking.
    if {[info exists ::Nagelfar(cacheBody)] && \
            [info exists ::Nagelfar(cacheBody,$body)]} {
        set statements $::Nagelfar(cacheStatements,$body)
        set indices $::Nagelfar(cacheIndices,$body)
    } else {
        splitScript $body $index statements indices knownVars
    }

    set type ""
    foreach statement $statements index $indices {
	set type [parseStatement $statement $index knownVars]
    }
    if {$::Nagelfar(firstpass)} {
        set ::Nagelfar(cacheBody) 1
        set ::Nagelfar(cacheBody,$body) 1
        set ::Nagelfar(cacheStatements,$body) $statements
        set ::Nagelfar(cacheIndices,$body) $indices
    } else {
        unset -nocomplain ::Nagelfar(cacheBody)
    }
    return $type
}

# This is called when a proc command is encountered.
proc parseProc {argv indices} {
    global knownGlobals syntax

    if {[llength $argv] != 3} {
	errorMsg E "Wrong number of arguments to proc." [lindex $indices 0]
	return
    }

    foreach {name args body} $argv {break}

    # Take care of namespace
    set cns [currentNamespace]
    set ns [namespace qualifiers $name]
    set tail [namespace tail $name]
    set storeIt 1
    if {![string match "::*" $ns]} {
        if {$cns eq "__unknown__"} {
            set ns $cns
            set storeIt 0
        } elseif {$ns != ""} {
            set ns ${cns}::$ns
        } else {
            set ns $cns
        }
    }
    set fullname ${ns}::$tail
    #decho "proc $name -> $fullname ($cns) ($ns) ($tail)"
    # Do not include the first :: in the name
    if {[string match ::* $fullname]} {
        set fullname [string range $fullname 2 end]
    }
    set name $fullname

    # Parse the arguments.
    # Initialise a knownVars array with the arguments.
    array set knownVars {}
    foreach a $args {
        set var [lindex $a 0]
        set knownVars(known,$var) 1
        set knownVars(local,$var) 1
        set knownVars(set,$var)   1
        set knownVars(type,$var)  ""
    }
    
    if {$storeIt} {
        lappend ::knownCommands $name
    }

    # Sanity check of argument names
    if {!$::Nagelfar(firstpass)} {
        # Check for non-last "args"
        set i [lsearch $args "args"]
        if {$i >= 0 && $i != [llength $args] - 1} {
            errorMsg N "Argument 'args' used before last, which can be confusing" \
                    [lindex $indices 0]
        }
        # Check for duplicates
        set l1 [lsort $args]
        set l2 [lsort -unique $args]
        if {$l1 ne $l2} {
            errorMsg N "Duplicate proc arguments" [lindex $indices 0]
        }
    }

#    decho "Note: parsing procedure $name"
    if {!$::Nagelfar(firstpass)} {
        pushNamespace $ns
        pushProc $name
        parseBody $body [lindex $indices 2] knownVars
        popProc
        popNamespace
    }
    set ::instrumenting([lindex $indices 2]) 1

    #foreach item [array names knownVars upvar,*] {
    #    puts "upvar '$item' '$knownVars($item)'"
    #}

    if {$storeIt} {
        # Build a syntax description for the procedure.
        # Parse the arguments.
        set upvar 0
        set unlim 0
        set min 0
        set newsyntax {}
        foreach a $args {
            set var [lindex $a 0]
            set type x

            # Check for any upvar in the proc
            if {[info exists knownVars(upvar,$var)]} {
                set other $knownVars(upvar,$var)
                if {[info exists knownVars(read,$other)]} {
                    set type v
                } elseif {[info exists knownVars(set,$other)]} {
                    set type n
                } else {
                    set type l
                }
                set upvar 1
            }
            if {[string equal $var "args"]} {
                set unlim 1
                set type x*
            } elseif {[llength $a] == 2} {
                append type .
            } else {
                incr min
            }
            lappend newsyntax $type
        }

        if {!$upvar} {
            if {$unlim} {
                set newsyntax [list r $min]
            } elseif {$min == [llength $args]} {
                set newsyntax $min
            } else {
                set newsyntax [list r $min [llength $args]]
            }
        }

        if {[info exists syntax($name)]} {
            #decho "$name : Prev: '$syntax($name)'  New: '$newsyntax'"
            # Check if it matches previously defined syntax
            set prevmin 0
            set prevmax 0
            set prevunlim 0
            if {[string is integer $syntax($name)]} {
                set prevmin $syntax($name)
                set prevmax $syntax($name)
            } elseif {[string match "r*" $syntax($name)]} {
                set prevmin [lindex $syntax($name) 1]
                set prevmax [lindex $syntax($name) 2]
                if {$prevmax == ""} {
                    set prevmax $prevmin
                    set prevunlim 1
                }
            } else {
                foreach token $syntax($name) {
                    set tok [string index $token 0]
                    set mod [string index $token 1]
                    set n [expr {$tok == "p" ? 2 : 1}]
                    if {$mod == ""} {
                        incr prevmin $n
                        incr prevmax $n
                    } elseif {$mod == "?"} {
                        incr prevmax $n
                    } elseif {$mod == "*"} {
                        set prevunlim 1
                    } elseif {$mod == "."} {
                        incr prevmax $n
                    }
                }
            }
            if {$prevunlim != $unlim || \
                    ($prevunlim == 0 && $prevmax != [llength $args]) \
                    || $prevmin != $min} {
                errorMsg W "Procedure \"$name\" does not match previous definition" \
                        [lindex $indices 0]
                contMsg "Previous '$syntax($name)'  New '$newsyntax'"
            } else {
                # It matched.  Does the new one seem better?
                if {[regexp {^(?:r )?\d+(?: \d+)?$} $syntax($name)]} {
                    #if {$syntax($name) != $newsyntax} {
                    #    decho "$name : Prev: '$syntax($name)'  New: '$newsyntax'"
                    #}
                    set syntax($name) $newsyntax
                }
            }
        } else {
            #decho "Syntax for '$name' : '$newsyntax'"
            set syntax($name) $newsyntax
        }
    }

    # Update known globals with those that were set in the proc.
    # I.e. anyone with set == 1 and namespace == "" should be
    # added to known globals.
    foreach item [array names knownVars namespace,*] {
        if {$knownVars($item) != ""} continue
        set var [string range $item 10 end]
	if {[info exists knownVars(set,$var)]} {
#	    decho "Set global $var in proc $name."
	    if {[lsearch $knownGlobals $var] == -1} {
		lappend knownGlobals $var
	    }
	}
    }
}

# Given an index in the original string, calculate its line number.
proc calcLineNo {ix} {
    global newlineIx

    # Shortcut for exact match, which happens when the index is first
    # in a line. This is common when called from wasIndented.
    set i [lsearch -integer -sorted $newlineIx $ix]
    if {$i >= 0} {
        return [expr {$i + 2}]
    }

    # Binary search
    if {$ix < [lindex $newlineIx 0]} {return 1}
    set first 0
    set last [expr {[llength $newlineIx] - 1}]

    while {$first < ($last - 1)} {
        set n [expr {($first + $last) / 2}]
        set ni [lindex $newlineIx $n]
        if {$ni < $ix} {
            set first $n
        } elseif {$ni > $ix} {
            set last $n
        } else {
            # Equality should have been caught in the lsearch above.
            decho "Internal error: Equal element slipped through in calcLineNo"
            return [expr {$n + 2}]
        }
    }
    return [expr {$last + 1}]
}

# Given an index in the original string, tell if that line was indented
# This should preferably be called with the index to the first char of
# the line since that case is much more efficient in calcLineNo.
proc wasIndented {i} {
    lindex $::indentInfo [calcLineNo $i]
}

# Length of initial whitespace
proc countIndent {str} {
    # Get whitespace
    set str [string range $str 0 end-[string length [string trimleft $str]]]
    # Any tabs?
    if {[string first \t $str] != -1} {
        # Only tabs in beginning?
        if {[regexp {^\t+[^\t]*$} $str]} {
            set str [string map $::Nagelfar(tabMap) $str]
        } else {
            regsub -all $::Nagelfar(tabReg) $str $::Nagelfar(tabSub) str
        }
    }
    return [string length $str]
}

# Build a database of newlines to be able to calculate line numbers.
# Also replace all escaped newlines with a space, and remove all
# whitespace from the start of lines. Later processing is greatly
# simplified if it does not need to bother with those.
# Returns the simplified script.
proc buildLineDb {str} {
    global newlineIx indentInfo

    set result ""
    set lines [split $str \n]
    if {[lindex $lines end] eq ""} {
        set lines [lrange $lines 0 end-1]
    }
    set newlineIx {}
    # Dummy element to get 1.. indexing
    set indentInfo [list {}]

    # Detect a header.  Backslash-newline is not substituted in the header,
    # and the index after the header is kept.  This is to preserve the header
    # in code coverage mode.
    # The first non-empty non-comment line ends the header.
    set ::instrumenting(header) 0
    set headerLines 1
    set previousWasEscaped 0

    # This is a trick to get "sp" and "nl" to get an internal string rep.
    # This also makes sure it will not be a shared object, which can mess up
    # the internal rep.
    # Append works a lot better that way.
    set sp [string range " " 0 0]
    set nl [string range \n 0 0]
    set lineNo 0

    foreach line $lines {
	incr lineNo
        # Count indent spaces and remove them
        set indent [countIndent $line]
	set line [string trimleft $line]
        # Check for comments.
	if {[string equal [string index $line 0] "#"]} {
	    checkPossibleComment $line $lineNo
	} elseif {$headerLines && $str ne "" && !$previousWasEscaped} {
            set headerLines 0
            set ::instrumenting(header) [string length $result]
        }

        # Count backslashes to determine if it's escaped
        set previousWasEscaped 0
        if {[string equal [string index $line end] "\\"]} {
	    set len [string length $line]
            set si [expr {$len - 2}]
            while {[string equal [string index $line $si] "\\"]} {incr si -1}
            if {($len - $si) % 2 == 0} {
                # An escaped newline
                set previousWasEscaped 1
                if {!$headerLines} {
                    append result [string range $line 0 end-1] $sp
                    lappend newlineIx [string length $result]
                    lappend indentInfo $indent
                    continue
                }
            }
        }
        # Unescaped newline
        # It's important for performance that all elements in append
        # has an internal string rep. String index takes care of $line
        append result $line $nl
        lappend newlineIx [string length $result]
        lappend indentInfo $indent
    }
    if {$::Nagelfar(gui)} {progressMax $lineNo}
    return $result
}

# Parse a global script
proc parseScript {script} {
    global knownGlobals unknownCommands knownCommands syntax

    catch {unset unknownCommands}
    set unknownCommands {}
    array set knownVars {}
    array set ::knownAliases {}
    foreach g $knownGlobals {
	set knownVars(known,$g) 1
	set knownVars(set,$g)   1
	set knownVars(namespace,$g) ""
	set knownVars(type,$g)      ""
    }
    set script [buildLineDb $script]
    set ::instrumenting(script) $script

    pushNamespace {}
    set ::Nagelfar(firstpass) 0
    if {$::Nagelfar(2pass)} {
        # First do one round with proc checking
        set ::Nagelfar(firstpass) 1
        parseBody $script 0 knownVars
        #echo "Second pass"
        set ::Nagelfar(firstpass) 0
    }
    parseBody $script 0 knownVars
    popNamespace

    # Check commands that where unknown when encountered
    # FIXA: aliases
    foreach apa $unknownCommands {
        foreach {cmd cmd1 cmd2 index} $apa break
        if {![info exists syntax($cmd1)] && \
                [lsearch $knownCommands $cmd1] == -1 && \
                ![info exists syntax($cmd2)] && \
                [lsearch $knownCommands $cmd2] == -1} {
	    # Close brace is reported elsewhere
            if {$cmd ne "\}"} {
		# Different messages depending on name
		if {[regexp {^[\w',:.]+$} $cmd]} {
		    errorMsg W "Unknown command \"$cmd\"" $index
		} else {
		    errorMsg E "Strange command \"$cmd\"" $index
		}
            }
        }
    }
    # Update known globals.
    foreach item [array names knownVars namespace,*] {
        if {$knownVars($item) != ""} continue
        set var [string range $item 10 end]
	# Check if it has been set.
	if {[info exists knownVars(set,$var)]} {
	    if {[lsearch $knownGlobals $var] == -1} {
		lappend knownGlobals $var
	    }
	}
    }
}

# Parse a file
proc parseFile {filename} {
    set ch [open $filename]
    if {[info exists ::Nagelfar(encoding)] && \
            $::Nagelfar(encoding) ne "system"} {
        fconfigure $ch -encoding $::Nagelfar(encoding)
    }
    set script [read $ch]
    close $ch

    array unset ::instrumenting

    initMsg
    parseScript $script
    flushMsg
    
    if {$::Nagelfar(instrument) && \
            [file extension $filename] ne ".syntax"} {
        # Experimental instrumenting
        dumpInstrumenting $filename
    }
}

# Experimental instrumenting
proc dumpInstrumenting {filename} {

    set tail [file tail $filename]
    set ifile ${filename}_i
    echo "Writing file $ifile" 1
    set iscript $::instrumenting(script)
    set indices {}
    foreach item [array names ::instrumenting] {
        if {[string is digit $item]} {
            lappend indices $item
        }
    }
    set init [list [list set current $tail]]
    set headerIndex $::instrumenting(header)
    foreach ix [lsort -decreasing -integer $indices] {
        if {$ix <= $headerIndex} break
        set line [calcLineNo $ix]
        set item "$tail,$line"
        set i 2
        while {[info exists done($item)]} {
            set item "$tail,$line,$i"
            incr i
        }
        set done($item) 1

        if {$::instrumenting($ix) == 2} {
            # Missing else clause
            if {[string index $iscript $ix] eq "\}"} {
                incr ix
            }
            set insert [list unset -nocomplain ::_instrument_::log($item)]
            set insert " [list else $insert]"
            set pre [string range $iscript 0 [expr {$ix - 1}]]
            set post [string range $iscript $ix end]
        } else {
            # Normal
            set insert [list unset -nocomplain ::_instrument_::log($item)]\;
            set pre [string range $iscript 0 [expr {$ix - 1}]]
            set post [string range $iscript $ix end]
        
            set c [string index $pre end]
            if {$c ne "\[" && $c ne "\{" && $c ne "\""} {
                if {[regexp {^(\s*\w+)(\s.*)$} $post -> word rest]} {
                    append pre "\{"
                    set post "$word\}$rest"
                } else {
                    echo "Not instrumenting line: $line\
                            [string range $pre end-5 end]<>[string range $post 0 5]"
                    continue
                }
            }
        }
        set iscript $pre$insert$post

        lappend init [list set log($item) 1]
    }
    set ch [open $ifile w]
    # Start with a copy of the original's header
    if {$headerIndex > 0} {
        puts $ch [string range $iscript 0 [expr {$headerIndex - 1}]]
        set iscript [string range $iscript $headerIndex end]
    }
    # Create a prolog equal in all instrumented files
    puts $ch {
        namespace eval ::_instrument_ {}
        if {[info commands ::_instrument_::source] == ""} {
            rename ::source ::_instrument_::source
            proc ::source {args} {
                set fileName [lindex $args end]
                set args [lrange $args 0 end-1]
                set newFileName $fileName
                set altFileName ${fileName}_i
                if {[file exists $altFileName]} {
                    set newFileName $altFileName
                }
                set args [linsert $args 0 ::_instrument_::source]
                lappend args $newFileName
                uplevel 1 $args
            }
            rename ::exit ::_instrument_::exit
            proc ::exit {args} {
                ::_instrument_::cleanup
                uplevel 1 [linsert $args 0 ::_instrument_::exit]
            }
            proc ::_instrument_::cleanup {} {
                variable log
                variable all
                variable dumpList
                foreach {src logFile} $dumpList {
                    set ch [open $logFile w]
                    puts $ch [list array unset ::_instrument_::log $src,*]
                    foreach item [lsort -dictionary [array names log $src,*]] {
                        puts $ch [list set ::_instrument_::log($item) 1]
                    }
                    close $ch
                }
            }
        }
    }
    # Insert file specific info
    puts $ch "# Initialise list of lines"
    puts $ch "namespace eval ::_instrument_ \{"
    puts $ch [join $init \n]
    puts $ch "\}"
    # More common prolog
    puts $ch {
        # Check if there is a stored log
        namespace eval ::_instrument_ {
            set thisScript [file normalize [file join [pwd] [info script]]]
            if {[string match "*_i" $thisScript]} {
                set thisScript [string range $thisScript 0 end-2]
            }
            set logFile    ${thisScript}_log
            if {[file exists $logFile]} {
                ::_instrument_::source $logFile
            }

            lappend dumpList $current $logFile
        }

        #instrumented source goes here
    }

    puts $ch $iscript
    close $ch
    
    # Copy permissions to instrumented file.
    catch {file attributes $ifile -permissions \
            [file attributes $filename -permissions]}
}

proc instrumentMarkup {filename} {
    set tail [file tail $filename]
    set logfile ${filename}_log
    set mfile ${filename}_m

    namespace eval ::_instrument_ {}
    source $logfile
    foreach item [array names ::_instrument_::log $tail,*] {
        if {[regexp {,(\d+),\d+$} $item -> line]} {
            set lines($line) 1
        } elseif {[regexp {,(\d+)$} $item -> line]} {
            set lines($line) 1
        }
    }
    echo "Writing file $mfile" 1
    if {[array size lines] == 0} {
        echo "All lines covered in $tail"
        file copy -force $filename $mfile
        return
    }

    set chi [open $filename r]
    set cho [open $mfile w]
    set lineNo 1
    while {[gets $chi line] >= 0} {
        if {[info exists lines($lineNo)]} {
            append line "*"
        }
        puts $cho $line
        incr lineNo
    }
    close $chi
    close $cho
}

# Add a message filter
proc addFilter {pat {reapply 0}} {
    if {[lsearch -exact $::Nagelfar(filter) $pat] < 0} {
        lappend ::Nagelfar(filter) $pat
    }
    if {$reapply} {
        set w $::Nagelfar(resultWin)
        $w configure -state normal
        set ln 1
        while {1} {
            set line [$w get $ln.0 $ln.end]
            if {[string match $pat $line]} {
                $w delete $ln.0 $ln.end+1c
            } else {
                incr ln
            }
            if {[$w index end] <= $ln} break
        }
        $w configure -state disabled
    }
}


# FIXA: Move safe reading to package
##nagelfar syntax _ipsource x
##nagelfar syntax _ipexists l
##nagelfar syntax _ipset    v
##nagelfar syntax _iparray  s v
##nagelfar subcmd _iparray  exists get

# Load syntax database using safe interpreter
proc loadDatabases {} {
    if {[interp exists loadinterp]} {
        interp delete loadinterp
    }
    interp create -safe loadinterp
    interp expose loadinterp source
    interp alias {} _ipsource loadinterp source
    interp alias {} _ipexists loadinterp info exists
    interp alias {} _ipset    loadinterp set
    interp alias {} _iparray  loadinterp array

    foreach f $::Nagelfar(db) {
        # FIXA: catch?
        _ipsource $f
    }

    if {[_ipexists ::knownGlobals]} {
        set ::knownGlobals [_ipset ::knownGlobals]
    } else {
        set ::knownGlobals {}
    }
    if {[_ipexists ::knownCommands]} {
        set ::knownCommands [_ipset ::knownCommands]
    } else {
        set ::knownCommands {}
    }
    if {[_ipexists ::dbInfo]} {
        set ::Nagelfar(dbInfo) [join [_ipset ::dbInfo] \n]
    } else {
        set ::Nagelfar(dbInfo) {}
    }
    if {[_ipexists ::dbTclVersion]} {
        set ::Nagelfar(dbTclVersion) [_ipset ::dbTclVersion]
    } else {
        set ::Nagelfar(dbTclVersion) [package present Tcl]
    }
    # {*} expansion requires that Nagelfar is run in 8.5 since the checks
    # for it does not work otherwise.
    # It also naturally requires an 8.5 database to indicate that it is
    # checking 8.5 scripts
    set ::Nagelfar(allowExpand) 0
    set ::Nagelfar(allowOldExpand) 0
    if {[package vcompare $::Nagelfar(dbTclVersion) 8.5] >= 0 && \
            [package vcompare $::tcl_version 8.5] >= 0} {
        ##nagelfar ignore
        if {![catch {list {expand}{hej}}]} {
            set ::Nagelfar(allowExpand) 1
            set ::Nagelfar(allowOldExpand) 1
        ##nagelfar ignore
        } elseif {![catch {list {*}{hej}}]} {
            set ::Nagelfar(allowExpand) 1
        }
    }

    catch {unset ::syntax}
    catch {unset ::return}
    catch {unset ::subCmd}
    catch {unset ::option}
    if {[_iparray exists ::syntax]} {
        array set ::syntax [_iparray get ::syntax]
    }
    if {[_iparray exists ::return]} {
        array set ::return [_iparray get ::return]
    }
    if {[_iparray exists ::subCmd]} {
        array set ::subCmd [_iparray get ::subCmd]
    }
    if {[_iparray exists ::option]} {
        array set ::option [_iparray get ::option]
    }

    interp delete loadinterp

    if {$::Prefs(strictAppend)} {
        set ::syntax(lappend) [string map {n v} $::syntax(lappend)]
        set ::syntax(append) [string map {n v} $::syntax(append)]
    }
}

# Execute the checks
proc doCheck {} {
    if {[llength $::Nagelfar(db)] == 0} {
        if {$::Nagelfar(gui)} {
            tk_messageBox -title "Nagelfar Error" -type ok -icon error \
                    -message "No syntax database file selected"
            return
        } else {
            puts stderr "No syntax database file found"
            exit 1
        }
    }

    set int [info exists ::Nagelfar(checkEdit)]

    if {!$int && [llength $::Nagelfar(files)] == 0} {
        errEcho "No files to check"
        return
    }

    if {$::Nagelfar(gui)} {
        busyCursor
    }

    if {!$int} {
        set ::Nagelfar(editFile) ""
    }
    if {[info exists ::Nagelfar(resultWin)]} {
        $::Nagelfar(resultWin) configure -state normal
        $::Nagelfar(resultWin) delete 1.0 end
    }

    # Load syntax databases
    loadDatabases

    # In header generation, store info before reading
    if {$::Nagelfar(header) ne ""} {
        set h_oldsyntax [array names ::syntax]
        set h_oldsubCmd [array names ::subCmd]
        set h_oldoption [array names ::option]
        set h_oldreturn [array names ::return]
    }

    # Do the checking

    set ::currentFile ""
    if {$int} {
        initMsg
        parseScript $::Nagelfar(checkEdit)
        flushMsg
    } else {
        foreach f $::Nagelfar(files) {
            if {$::Nagelfar(gui) || [llength $::Nagelfar(files)] > 1} {
                set ::currentFile $f
            }
            set syntaxfile [file rootname $f].syntax
            if {[file exists $syntaxfile]} {
                if {!$::Nagelfar(quiet)} {
                    echo "Parsing file $syntaxfile" 1
                }
                parseFile $syntaxfile
            }
            if {$f == $syntaxfile} continue
            if {[file isfile $f] && [file readable $f]} {
                if {!$::Nagelfar(quiet)} {
                    echo "Checking file $f" 1
                }
                parseFile $f
            } else {
                errEcho "Could not find file '$f'"
            }
        }
    }
    # Generate header
    if {$::Nagelfar(header) ne ""} {
        foreach item $h_oldsyntax { unset ::syntax($item) }
        foreach item $h_oldsubCmd { unset ::subCmd($item) }
        foreach item $h_oldoption { unset ::option($item) }
        foreach item $h_oldreturn { unset ::return($item) }
        
        if {[catch {set ch [open $::Nagelfar(header) w]}]} {
            puts stderr "Could not create file \"$::Nagelfar(header)\""
        } else {
            echo "Writing \"$::Nagelfar(header)\"" 1
            foreach item [lsort -dictionary [array names ::syntax]] {
                puts $ch "\#\#nagelfar [list syntax $item] $::syntax($item)"
            }
            foreach item [lsort -dictionary [array names ::subCmd]] {
                puts $ch "\#\#nagelfar [list subcmd $item] $::subCmd($item)"
            }
            foreach item [lsort -dictionary [array names ::option]] {
                puts $ch "\#\#nagelfar [list option $item] $::option($item)"
            }
            foreach item [lsort -dictionary [array names ::return]] {
                puts $ch "\#\#nagelfar [list return $item] $::return($item)"
            }
            close $ch
        }
    }
    if {$::Nagelfar(gui)} {
        echo "Done" 1
        normalCursor
        progressUpdate -1
    }
}
