#!/bin/sh
#----------------------------------------------------------------------
#  nagelfar.tcl, a syntax checker for Tcl.
#  Copyright (c) 1999-2004, Peter Spjuth  (peter.spjuth@space.se)
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
set version "Version 1.0b2 2004-02-10"

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
# 0 contains substitutions
# 1 constant
# 2 enclosed in braces, or constant in quotes

# Moved out message handling to make it more flexible
proc echo {str} {
    if {[info exists ::Nagelfar(resultWin)]} {
        $::Nagelfar(resultWin) configure -state normal
        $::Nagelfar(resultWin) insert end $str\n
        $::Nagelfar(resultWin) configure -state disabled
    } else {
        puts $str
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

# A profiling thingy
proc timestamp {str} {
    global _timestamp_
    set apa [clock clicks]
    if {[info exists _timestamp_]} {
        puts stderr $str:$apa:[expr {$apa - $_timestamp_}]
    } else {
        puts stderr $str:$apa
    }
    set _timestamp_ $apa
}

# A tool to collect profiling data
##syntax profile x c
proc profile {str script} {
    global profiledata
    if {![info exists profiledata($str)]} {
        set profiledata($str)   0
        set profiledata($str,n) 0
    }
    set apa [clock clicks]
    set res [uplevel 1 $script]
    incr profiledata($str) [expr {[clock clicks] - $apa}]
    incr profiledata($str,n)
    return $res
}

proc dumpProfileData {} {
    global profiledata
    set maxl 0
    foreach name [array names profiledata] {
	if {[string length $name] > $maxl} {
	    set maxl [string length $name]
	}
    }
    foreach name [lsort -dictionary [array names profiledata]] {
	puts stdout [format "%-*s = %s" $maxl $name $profiledata($name)]
    }
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
    if {[string match "##syntax *" $str]} {
        set ::syntax([lindex $str 1]) [lrange $str 2 end]
    }
    if {[string match "##variable *" $str]} {
        set var [string trim [string range $str 11 end]]
        markVariable $var 1 1 $index knownVars
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

# Move "i" forward to the first non whitespace char
proc skipWS {str len iName} {
    upvar $iName i

    set j [string length [string trimleft [string range $str $i end]]]
    set i [expr {$len - $j}]
}

# Scan the string until the end of one word is found.
# When entered, i points to the start of the word.
# When returning, i points to the last char of the word.
proc scanWord {str len index iName} {
    upvar $iName i

    set si $i
    set c [string index $str $i]

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
                set i $len
                return
            }
            set word [string range $str $si $i]
            if {[info complete $word]} {
                # Check for following whitespace
                set j [expr {$i + 1}]
                if {$j == $len || [string is space [string index $str $j]]} {
                    return
                }
                errorMsg E "Extra chars after closing $charType." \
                        [expr {$index + $i}]
                contMsg "Opening $charType of above was on line %L." \
                        [expr {$index + $si}]
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
        if {[info complete [string range $str $si $i]]} {
            incr i -1
            return
        }
    }

    # Theoretically, no incomplete string should come to this function,
    # but some precaution is never bad.
    if {![info complete [string range $str $si end]]} {
        decho "Internal error in scanWord: String not complete.\
                Line [calcLineNo [expr {$index + $si}]]."
        decho $str
        return -code break
    }
    incr i -1
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
    skipWS $statement $len i
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
        scanWord $statement $len $index i
        lappend words [string range $statement $si $i]
        incr i
        skipWS $statement $len i
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


    set maxa [expr {[llength $argv] - $startI}]
    if {$pair && ($maxa % 2) == 1} {
        incr maxa -1
    }
    if {$maxa > $max} {
        set max $maxa
    }
    if {$maxa == 0} {
        return {}
    }
    set check [info exists option($cmd)]
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
	if {[string equal [string index $arg 0] "-"]} {
	    incr used
            lappend replaceSyn x
	    set skip $pair
	    if {$ws != 0  && $check} {
                set ix [lsearch $option($cmd) $arg]
		if {$ix == -1} {
                    # Check ambiguity.
                    set match [lsearch -all -inline -glob $option($cmd) $arg*]
                    if {[llength $match] == 0} {
                        errorMsg E "Bad option $arg to $cmd" $index
                    } elseif {[llength $match] > 1} {
                        errorMsg E "Ambigous option for $cmd,\
                                $arg -> [join $match /]" $index
                    } else {
                        errorMsg W "Shortened option for $cmd,\
                                $arg -> [lindex $match 0]" $index
                    }
		} else {
                    set item "$cmd [lindex $option($cmd) $ix]"
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
	} else {
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
    set state ws

    for {set i 0} {$i < $len} {incr i} {
	set c [string index $str $i]
	switch -- $state {
	    ws { # Whitespace
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
			    set state ws
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
			    set state ws
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
				set state ws
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
	    if {[string is word $c]} continue
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
                    [expr {$index + $pi}] knownVars]
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
        return
    }

    if {[string match ::* $var]} {
	# Skip qualified names until we handle namespace better. FIXA
	return
    }
    if {![info exists knownVars(known,$var)]} {
	errorMsg E "Unknown variable \"$var\"" $index
    }
    if {![info exists knownVars(set,$var)]} {
        set knownVars(read,$var) 1
        if {[info exists knownVars(local,$var)]} {
            errorMsg E "Unknown variable \"$var\"" $index
        }
    }
    # Make use of markVariable. FIXA
    # If it's a constant array index, maybe it should be checked? FIXA
}

# Check for substitutions in a word
# Check any variables referenced, and parse any commands within brackets.
# Returns 1 if the string is constant, i.e. no substitutions
# Returns 0 if any substitutions are present
proc parseSubst {str index knownVarsName} {
    upvar $knownVarsName knownVars

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
    for {set i 0} {$i < $len} {incr i} {
        set c [string index $str $i]
        if {[string equal $c "\\"]} {
            set escape [expr {!$escape}]
        } elseif {!$escape} {
	    if {[string equal $c "\$"]} {
		incr i
		parseVar $str $len $index i knownVars
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
		parseBody [string range $str $si $i] [expr {$index + $si}] \
                        knownVars
		incr i
		set result 0
	    } elseif {[string equal $c "\]"] && $i == ($len - 1)} {
                # Note unescaped bracket at end of word since it's
                # likely to mean it should not be there.
                errorMsg N "Unescaped end bracket" [expr {$index + $i}]
            }
        } else {
            set escape 0
        }
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
                    } elseif {[string equal $c "\]"] && $i == ($len - 1)} {
                        # Note unescaped bracket at end of word since it's
                        # likely to mean it should not be there.
                        errorMsg N "Unescaped end bracket" [expr {$index + $i}]
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
    # Check for #
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

proc checkForCommentL {words wordstatus indices} {
    foreach word $words ws $wordstatus i $indices {
        if {$ws == 2} {
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
proc checkCommand {cmd index argv wordstatus indices {firsti 0}} {
    upvar "constantsDontCheck" constantsDontCheck "knownVars" knownVars

    set argc [llength $argv]
    set syn $::syntax($cmd)
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
            return
        }
        set syn $newsyn
    }

    if {[string is integer -strict $syn]} {
	if {($argc - $firsti) != $syn} {
	    WA
	}
        checkForCommentL $argv $wordstatus $indices
	return
    } elseif {[string equal [lindex $syn 0] "r"]} {
	if {($argc - $firsti) < [lindex $syn 1]} {
	    WA
	} elseif {[llength $syn] >= 3 && ($argc - $firsti) > [lindex $syn 2]} {
	    WA
	}
        checkForCommentL $argv $wordstatus $indices
	return
    }

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
		if {[lindex $wordstatus $i] == 2 && $tok != "X"} {
                    checkForComment [lindex $argv $i] [lindex $indices $i]
                }
		if {![string equal $mod "?"] || $i < $argc} {
		    incr i
		}
	    }
            E -
	    e { # An expression
		if {![string equal $mod ""]} {
		    echo "Modifier \"$mod\" is not supported for \"$tok\" in\
                            syntax for $cmd."
		}
		if {[lindex $wordstatus $i] == 0} {
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
                } elseif {[lindex $wordstatus $i] == 2} {
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
		if {[lindex $wordstatus $i] == 0} {
                    # No braces around non constant code.
                    # Special case: [list ...]
                    set arg [lindex $argv $i]
                    if {[string match {\[list*} $arg]} {
                        #echo "(List code)"
                    } else {
                        errorMsg W "No braces around code in $cmd\
                                statement." [lindex $indices $i]
                    }
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
		if {[lindex $wordstatus $i] == 0} {
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
                                    errorMsg E "Ambigous subcommand for $cmd,\
                                            $arg -> [join $match /]" \
                                            [lindex $indices $i]
                                } elseif {$::Prefs(warnShortSub)} {
                                    # Report shortened subcmd?
                                    errorMsg W "Shortened subcommand for $cmd,\
                                            $arg ->\
                                            [lindex $match 0]" \
                                            [lindex $indices $i]
                                }
                                set arg [lindex $::subCmd($cmd) $ix]
                            }
			}
		    }
		    # Are there any syntax definition for this subcommand?
		    set sub "$cmd $arg"
		    if {[info exists ::syntax($sub)]} {
			checkCommand $sub $index $argv $wordstatus $indices 1
			set i $argc
			break
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
		    set ei $argc
		}
		while {$i < $ei} {
		    if {[string equal $tok "v"]} {
			# Check the variable
                        if {[string match ::* [lindex $argv $i]]} {
                            # Skip qualified names until we handle
                            # namespace better. FIXA
                        } elseif {[markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] 2 $index knownVars]} {
			    errorMsg E "Unknown variable \"[lindex $argv $i]\""\
                                    $index
			}
		    } elseif {[string equal $tok "n"]} {
			markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] 1 $index knownVars
		    } else {
			markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] 0 $index knownVars
		    }

		    lappend constantsDontCheck $i
		    incr i
		}
	    }
	    o {
		set max 0
		if {![string equal $mod "*"]} {
		    set max 1
		}
		set oSyn [checkOptions $cmd $argv $wordstatus $indices $i $max]
                set used [llength $oSyn]
		if {$used == 0 && ($mod == "" || $mod == ".")} {
		    errorMsg E "Expected an option as argument $i to \"$cmd\"" \
                            [lindex $indices $i]
		    return
		}
                
                if {[lsearch -not $oSyn "x"] >= 0} {
                    # Feed the syntax back into the check loop
                    set syn [concat $oSyn $syn]
                } else {
                    incr i $used
                }
	    }
	    p {
		set max 0
		if {![string equal $mod "*"]} {
		    set max 2
		}
                # FIXA: like o
		set used [checkOptions $cmd $argv $wordstatus $indices $i \
                        $max 1]
                set used [llength $used]
		if {$used == 0 && ($mod == "" || $mod == ".")} {
		    errorMsg E "Expected an option as argument $i to \"$cmd\"" \
			    [lindex $indices $i]
		    return
		}
		incr i $used
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
}

# Central function to handle known variable names.
# If check is 2, check if it is known, return 1 if unknown
# If check is 1, mark the variable as known and set
# If check is 0, mark the variable as known
proc markVariable {var ws check index knownVarsName} {
    upvar $knownVarsName knownVars
    
    if {$::Prefs(noVar)} {
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
	if {$varBaseWs == 0 && [regexp {^(::)?(\w+(::)?)+$} $varBase]} {
	    set varBaseWs 1
	}
	set varArray 1
    }

    # If the base contains substitutions it can't be checked.
    if {$varBaseWs == 0} {
        # Experimental foreach check FIXA
        if {[string match {$*} $var]} {
            set name [string range $var 1 end]
            if {[info exists ::foreachVar($name)]} {
                # Mark them as known instead
                foreach name $::foreachVar($name) {
                    markVariable $name 1 $check $index knownVars
                }
                #return 1
            }
        }
        errorMsg N "Suspicious variable name \"$var\"" $index
	return 0
    }

    if {$check == 2} {
	if {![info exists knownVars(known,$varBase)]} {
	    return 1
	}
	if {$varArray && $varIndexWs != 0 && \
                [info exists knownVars(local,$varBase)]} {
	    if {![info exists knownVars(known,$var)]} {
		return 1
	    }
	}
	return 0
    } else {
	if {![info exists knownVars(known,$varBase)]} {
            set knownVars(known,$varBase) 1
            set knownVars(local,$varBase) 1
        }
        if {$check == 1} {
            set knownVars(set,$varBase) 1
        }
        # If the array index is constant, mark the whole name
	if {$varArray && $varIndexWs != 0} {
	    if {![info exists knownVars(known,$var)]} {
		set knownVars(known,$var) 1
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
proc lookForCommand {cmd ns index} {
    # Get both the namespace and global possibility
    if {[string match "::*" $cmd]} {
        set cmd1 [string range $cmd 2 end]
        set cmd2 ""
    } else {
        set cmd1 "${ns}::$cmd"
        if {[string match "::*" $cmd1]} {
            set cmd1 [string range $cmd1 2 end]
        }
        set cmd2 $cmd
    }

    if {[lsearch $::knownCommands $cmd] >= 0} {
        return
    }
    if {[lsearch $::knownCommands $cmd1] >= 0} {
        return
    }
    if {$cmd2 != "" && [lsearch $::knownCommands $cmd2] >= 0} {
        return
    }

    lappend ::unknownCommands [list $cmd $cmd1 $cmd2 $index]
}

# Parse one statement and check the syntax of the command
proc parseStatement {statement index knownVarsName} {
    upvar $knownVarsName knownVars
    set words [splitStatement $statement $index indices]
    if {[llength $words] == 0} {return}
    if {$::Nagelfar(onlyproc)} {
        if {[lindex $words 0] != "proc"} {
            return
        }
    }

    set words2 {}
    set wordstatus {}
    set indices2 {}
    foreach word $words index $indices {
        set char [string index $word 0]
        if {[string equal $char "\{"]} {
            lappend words2 [string range $word 1 end-1]
            lappend wordstatus 2
	    incr index
        } else {
	    set ws 1
            if {[string equal $char "\""]} {
                set word [string range $word 1 end-1]
		incr index
		set ws 2
            }
	    lappend words2 $word
	    set ws [expr {[parseSubst $word $index knownVars] ? $ws : 0}]
            lappend wordstatus $ws
        }
	lappend indices2 $index
    }


    # If the command contains substitutions we can not determine
    # which command it is, so we skip it.
    # FIXA. A command with a variable may be a widget command.
    if {[lindex $wordstatus 0] == 0} {
        return
    }

    set cmd [lindex $words2 0]
    set index [lindex $indices2 0]
    set argv [lrange $words2 1 end]
    set wordstatus [lrange $wordstatus 1 end]
    set indices [lrange $indices2 1 end]
    set argc [llength $argv]

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
		WA
		return
	    }
	    # Skip the proc if any part of it is not constant
            # FIXA: Maybe accept substitutions as part of namespace?
	    if {[lsearch $wordstatus 0] >= 0} {
		errorMsg N "Non constant argument to proc \"[lindex $argv 0]\".\
                        Skipping." $index
		return
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
	    # FIXA, update the globals database
	    foreach var $argv ws $wordstatus {
		if {$ws} {
                    set knownVars(known,$var) 1
                    set knownVars(namespace,$var) ""
		} else {
		    errorMsg N "Non constant argument to $cmd: $var" $index
		}
	    }
            set noConstantCheck 1
	}
	variable {
	    # FIXA, namespaces?
	    # FIXA, qualified names?
	    set i 0
	    foreach {var val} $argv {ws1 ws2} $wordstatus {
		regexp {::([^:]+)$} $var -> var
		if {$ws1} {
                    set knownVars(known,$var) 1
                    #set knownVars(namespace,$var) FIXA???
		    if {$i < $argc - 1} {
                        set knownVars(set,$var) 1
		    }
		    lappend constantsDontCheck $i
		} else {
		    errorMsg N "Non constant argument to $cmd: $var" $index
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
            if {[lindex $wordstatus 0]} {
                # Is it a level ?
                if {[regexp {^[\#0-9]} $level]} {
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
                if {$wsV == 0} {
                    # The variable name contains substitutions
                    errorMsg N "Suspicious upvar variable \"$var\"" $index
                } else {
                    set knownVars(known,$var) 1
                    lappend constantsDontCheck $i
                    if {$wsO == 0} {
                        # Is the other name a simple var subst?
                        if {[regexp {^\$[\w()]+} $other]} {
                            set other [string range $other 1 end]
                            if {[info exists knownVars(known,$other)]} {
                                # FIXA: level #0 for global
                                if {$level == 1} {
                                    set knownVars(upvar,$other) $var
                                }
                            }
                        }
                    }
                }
		incr i 2
	    }
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
		    markVariable $var 1 1 $index knownVars
		}
	    }
            # FIXA: Experimental foreach check...
            # A special case for looping over constant lists
            set varsAdded {}
            foreach {varList valList} [lrange $argv 0 end-1] \
                    {varWS valWS} [lrange $wordstatus 0 end-1] {
                if {$varWS != 0 && $valWS != 0} {
                    set fVars {}
                    foreach fVar $varList {
                        set ::foreachVar($fVar) {}
                        lappend fVars apaV($fVar)
                        lappend varsAdded $fVar
                    }
                    foreach $fVars $valList {
                        foreach fVar $varList {
                            ##variable apaV
                            lappend ::foreachVar($fVar) $apaV($fVar)
                        }
                    }
                }
            }
            
            if {[lindex $wordstatus end] == 0} {
                errorMsg W "No braces around body in foreach\
                        statement." $index
	    }
	    parseBody [lindex $argv end] [lindex $indices end] knownVars
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
                        if {[string equal $arg "0"]} {
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
	    if {![string equal $state "else"] \
                    && ![string equal $state "illegal"]} {
		errorMsg E "Badly formed if statement" $index
		contMsg "Missing one body."
		return
	    }
#            decho "if syntax \"$ifsyntax\""
	    set ::syntax(if) $ifsyntax
	    checkCommand $cmd $index $argv $wordstatus $indices
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
		if {$ws2 == 0} {
		    errorMsg W "No braces around code in switch\
                            statement." $i2
		}
		parseBody $body $i2 knownVars
	    }
	}
	expr { # FIXA
            # Take care of the standard case of a brace enclosed expr.
            if {$argc == 1 && [lindex $wordstatus 0] != 0} {
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
	interp { # FIXA
            set noConstantCheck 1
	}
        package { # FIXA, take care of require
            checkCommand $cmd $index $argv $wordstatus $indices
        }
	namespace { # FIXA, also handle import
            if {$argc < 1} {
                WA
                return
            }
            if {[lindex $wordstatus 0] != 0 && \
                    [string match "ev*" [lindex $argv 0]]} {
                if {$argc < 3} {
                    WA
                    return
                }
                # Look for unknown parts
                if {[lsearch $wordstatus 0] >= 0} {
                    errorMsg N "Only braced namespace evals are checked." \
                            [lindex $indices 0]
                } else {
                    set ns [lindex $argv 1]
                    if {![string match "::*" $ns]} {
                        set ns [currentNamespace]::$ns
                    }
                    pushNamespace $ns
                    parseBody [lindex $argv 2] [lindex $indices 2] knownVars
                    popNamespace
                }
            } else {
                checkCommand $cmd $index $argv $wordstatus $indices
            }
	}
	uplevel { # FIXA
            set noConstantCheck 1
	}
	default {
	    if {[info exists ::syntax($cmd)]} {
		checkCommand $cmd $index $argv $wordstatus $indices
	    } else {
                lookForCommand $cmd [currentNamespace] $index
	    }
	}
    }

    if {$::Prefs(noVar)} {
        return
    }

    if {$noConstantCheck} {
        return
    }

    # Check unmarked constants against known variables to detect missing $.
    # The constant is considered ok if within quotes (i.e. ws=2).
    set i 0
    foreach ws $wordstatus var $argv {
        if {[info exists knownVars(known,$var)]} {
            if {$ws == 1 && [lsearch $constantsDontCheck $i] == -1} {
                errorMsg W "Found constant \"$var\" which is also a\
                        variable." [lindex $indices $i]
            }
        }
        incr i
    }
}

# Split a script into individual statements
proc splitScript {script index statementsName indicesName knownVarsName} {
    upvar $statementsName statements $indicesName indices
    upvar $knownVarsName knownVars

    set statements {}
    set indices {}
    set lines [split $script \n]
    set tryline ""
    set newstatement 1
    set firstline ""
    string length $tryline

    foreach line $lines {
        # Restore the \n that split removed
	append line \n
	while {![string equal $line ""]} {

            # Some extra checking on close braces to help finding
            # brace mismatches
            set closeBrace -1
            if {[string equal "\}" [string trim $line]]} {
                set closeBraceIx [expr {[string length $tryline] + $index}]
                if {$newstatement} {
                    errorMsg E "Close brace first in statement." $closeBraceIx
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
			lappend statements [string range $tryline 0 end-1]
		    } else {
			lappend statements $tryline
		    }
		    lappend indices $index
		}
                if {$closeBrace != -1} {
                    set tmp [wasIndented $index]
                    if {$tmp != $closeBrace} {
                        errorMsg N "Close brace not aligned with line\
                                [calcLineNo $index] ($tmp $closeBrace)" \
                                $closeBraceIx
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

proc parseBody {body index knownVarsName} {
    upvar $knownVarsName knownVars

    # Cache the splitScript result to optimise 2-pass checking.
    if {[info exists ::Nagelfar(cacheBody)] && \
            [string equal $::Nagelfar(cacheBody) $body]} {
        set statements $::Nagelfar(cacheStatements)
        set indices $::Nagelfar(cacheIndices)
    } else {
        splitScript $body $index statements indices knownVars
    }

    foreach statement $statements index $indices {
	parseStatement $statement $index knownVars
    }
    if {$::Nagelfar(onlyproc)} {
        set ::Nagelfar(cacheBody) $body
        set ::Nagelfar(cacheStatements) $statements
        set ::Nagelfar(cacheIndices) $indices
    } else {
        unset -nocomplain ::Nagelfar(cacheBody)
    }
}

# This is called when a proc command is encountered.
proc parseProc {argv indices} {
    global knownCommands knownGlobals syntax

    if {[llength $argv] != 3} {
	errorMsg E "Wrong number of arguments to proc." [lindex $indices 0]
	return
    }

    foreach {name args body} $argv {break}

    # Take care of namespace
    set cns [currentNamespace]
    set ns [namespace qualifiers $name]
    set tail [namespace tail $name]
    if {![string match "::*" $ns]} {
        if {$ns != ""} {
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
        set knownVars(known,[lindex $a 0]) 1
        set knownVars(local,[lindex $a 0]) 1
        set knownVars(set,[lindex $a 0]) 1
    }

    lappend knownCommands $name

#    decho "Note: parsing procedure $name"
    if {!$::Nagelfar(onlyproc)} {
        pushNamespace $ns
        parseBody $body [lindex $indices 2] knownVars
        popNamespace
    }

    #foreach item [array names knownVars upvar,*] {
    #    puts "upvar '$item' '$knownVars($item)'"
    #}

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
    global indentInfo
    lindex $indentInfo [expr {[calcLineNo $i] - 1}]
}

# Length of initial whitespace
proc countIndent {str} {
    set str [string range $str 0 end-[string length [string trimleft $str]]]
    regsub -all { {0,7}\t} $str "        " str
    return [string length $str]
}

# Build a database of newlines to be able to calculate line numbers.
# Also replace all escaped newlines with a space, and remove all
# whitespace from the start of lines. Later processing is greatly
# simplified if it does not need to bother with those.
proc buildLineDb {str} {
    global newlineIx indentInfo

    set result ""
    set lines [split $str \n]
    set newlineIx {}
    set indentInfo {}
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
	}
        # Count backslashes to determine if it's escaped
        if {[string equal [string index $line end] "\\"]} {
	    set len [string length $line]
            set si [expr {$len - 2}]
            while {[string equal [string index $line $si] "\\"]} {incr si -1}
            if {($len - $si) % 2 == 0} {
                # An escaped newline
                append result [string range $line 0 end-1] $sp
                lappend newlineIx [string length $result]
                lappend indentInfo $indent
                continue
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
    foreach g $knownGlobals {
	set knownVars(known,$g) 1
	set knownVars(set,$g) 1
	set knownVars(namespace,$g) ""
    }
    set script [buildLineDb $script]

    pushNamespace {}
    set ::Nagelfar(onlyproc) 0
    if {$::Nagelfar(2pass)} {
        # First do one round with proc checking
        set ::Nagelfar(onlyproc) 1
        parseBody $script 0 knownVars
        #echo "Second pass"
        set ::Nagelfar(onlyproc) 0
    }
    parseBody $script 0 knownVars
    popNamespace

    # Check commands that where unknown when encountered
    foreach apa $unknownCommands {
        foreach {cmd cmd1 cmd2 index} $apa break
        if {![info exists syntax($cmd1)] && \
                [lsearch $knownCommands $cmd1] == -1 && \
                ![info exists syntax($cmd2)] && \
                [lsearch $knownCommands $cmd2] == -1} {
            errorMsg W "Unknown command \"$cmd\"" $index
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

    initMsg
    parseScript $script
    flushMsg
}

# Add a message filter
proc addFilter {pat} {
    if {[lsearch -exact $::Nagelfar(filter) $pat] < 0} {
        lappend ::Nagelfar(filter) $pat
    }
}

proc usage {} {
    puts $::version
    puts {Usage: nagelfar [options] scriptfile ...
 -help             : Show usage.
 -gui              : Start with GUI even when files are specified.
 -s <dbfile>       : Include a database file. (More than one is allowed.)
 -encoding <enc>   : Read script with this encoding.
 -filter <p>       : Any message that matches the glob pattern is suppressed.
 -severity <level> : Set severity level filter to N/W/E (default N).
 -novar            : Disable variable checking.
 -WexprN           : Sets expression warning level to N.
   2 (def)         = Warn about any unbraced expression.
   1               = Don't warn on single commands. "if [apa] {...}" is ok.
 -WsubN            : Sets subcommand warning level to N.
   1 (def)         = Warn about shortened subcommands.
 -WelseN           : Enforce else keyword. Default 1.}
    exit
}

#####################################
# Registry section
#####################################

proc makeRegistryFrame {w label key newvalue} {
    set old {}
    catch {set old [registry get $key {}]}

    set l [labelframe $w -text $label -padx 4 -pady 4]

    label $l.key1 -text "Key:"
    label $l.key2 -text $key
    label $l.old1 -text "Old value:"
    label $l.old2 -text $old
    label $l.new1 -text "New value:"
    label $l.new2 -text $newvalue

    button $l.change -text "Change" -width 10 -command \
            "[list registry set $key {} $newvalue] ; \
             [list $l.change configure -state disabled]"
    button $l.delete -text "Delete" -width 10 -command \
            "[list registry delete $key] ; \
             [list $l.delete configure -state disabled]"
    if {[string equal $newvalue $old]} {
        $l.change configure -state disabled
    }
    if {[string equal "" $old]} {
        $l.delete configure -state disabled
    }
    grid $l.key1 $l.key2 -     -sticky "w" -padx 4 -pady 4
    grid $l.old1 $l.old2 -     -sticky "w" -padx 4 -pady 4
    grid $l.new1 $l.new2 -     -sticky "w" -padx 4 -pady 4
    grid $l.delete - $l.change -sticky "w" -padx 4 -pady 4
    grid $l.change -sticky "e"
    grid columnconfigure $l 2 -weight 1
}

proc makeRegistryWin {} {
    global thisScript

    # Locate executable for this program
    set exe [info nameofexecutable]
    if {[regexp {^(.*wish)\d+\.exe$} $exe -> pre]} {
        set alt $pre.exe
        if {[file exists $alt]} {
            set a [tk_messageBox -icon question -title "Which Wish" -message \
                    "Would you prefer to use the executable\n\
                    \"$alt\"\ninstead of\n\
                    \"$exe\"\nin the registry settings?" -type yesno]
            if {$a eq "yes"} {
                set exe $alt
            }
        }
    }

    set top .reg
    destroy $top
    toplevel $top
    wm title $top "Register Nagelfar"

    # Registry keys

    set key {HKEY_CLASSES_ROOT\.tcl\shell\Check\command}
    set old {}
    catch {set old [registry get {HKEY_CLASSES_ROOT\.tcl} {}]}
    if {$old != ""} {
        set key "HKEY_CLASSES_ROOT\\$old\\shell\\Check\\command"
    }

    # Are we in a starkit?
    if {[info exists ::starkit::topdir]} {
        # In a starpack ?
        set exe [file normalize $exe]
        if {[string equal [file normalize $::starkit::topdir] $exe]} {
            set myexe [list $exe]
        } else {
            set myexe [list $exe $::starkit::topdir]
        }
    } else {
        if {[regexp {wish\d+\.exe} $exe]} {
            set exe [file join [file dirname $exe] wish.exe]
            if {[file exists $exe]} {
                set myexe [list $exe]
            }
        }
        set myexe [list $exe $thisScript]
    }

    set valbase {}
    foreach item $myexe {
        lappend valbase \"[file nativename $item]\"
    }
    set valbase [join $valbase]

    set new "$valbase -gui \"%1\""
    makeRegistryFrame $top.d "Check" $key $new

    pack $top.d -side "top" -fill x -padx 4 -pady 4

    button $top.close -text "Close" -width 10 -command [list destroy $top] \
            -default active
    pack $top.close -side bottom -pady 4
    bind $top <Key-Return> [list destroy $top]
    bind $top <Key-Escape> [list destroy $top]
}

###########
# GUI stuff
###########

proc busyCursor {} {
    if {![info exists ::oldcursor]} {
        set ::oldcursor  [. cget -cursor]
        set ::oldcursor2 [$::Nagelfar(resultWin) cget -cursor]
    }

    . config -cursor watch
    $::Nagelfar(resultWin) config -cursor watch
}

proc normalCursor {} {
    . config -cursor $::oldcursor
    $::Nagelfar(resultWin) config -cursor $::oldcursor2
}

proc exitApp {} {
    exit
}

# Browse for and add a syntax database file
proc addDbFile {} {
    set apa [tk_getOpenFile -title "Select db file"]
    if {$apa == ""} return

    lappend ::Nagelfar(db) $apa
    lappend ::Nagelfar(allDb) $apa
    lappend ::Nagelfar(allDbView) $apa
    updateDbSelection 1
}

# File drop using TkDnd
proc fileDropDb {files} {
    foreach file $files {
        set file [fileRelative [pwd] $file]
        lappend ::Nagelfar(db) $file
        lappend ::Nagelfar(allDb) $file
        lappend ::Nagelfar(allDbView) $file
    }
    updateDbSelection 1
}

# Browse for and add a file to check.
proc addFile {} {
    set apa [tk_getOpenFile -title "Select file to check" \
            -defaultextension .tcl \
            -filetypes {{{Tcl File} {.tcl}} {{All Files} {.*}}}]
    if {$apa == ""} return

    lappend ::Nagelfar(files) [fileRelative [pwd] $apa]
}

# Remove a file from the list to check
proc removeFile {} {
    set ixs [lsort -decreasing -integer [$::Nagelfar(fileWin) curselection]]
    foreach ix $ixs {
        set ::Nagelfar(files) [lreplace $::Nagelfar(files) $ix $ix]
    }
}

# File drop using TkDnd
proc fileDropFile {files} {
    foreach file $files {
        lappend ::Nagelfar(files) [fileRelative [pwd] $file]
    }
}

# FIXA: Move safe reading to package
##syntax _ipsource x
##syntax _ipexists l
##syntax _ipset    v
##syntax _iparray  s v

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
        tk_messageBox -title "Nagelfar Error" -type ok -icon error \
                -message "No files to check"
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

    # Load syntax database using safe interpreter

    interp create -safe loadinterp
    interp expose loadinterp source
    interp alias {} _ipsource loadinterp source
    interp alias {} _ipexists loadinterp info exists
    interp alias {} _ipset    loadinterp set
    interp alias {} _iparray  loadinterp array

    foreach f $::Nagelfar(db) {
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

    catch {unset ::syntax}
    catch {unset ::subCmd}
    catch {unset ::option}
    if {[_iparray exists ::syntax]} {
        array set ::syntax [_iparray get ::syntax]
    }
    if {[_iparray exists ::subCmd]} {
        array set ::subCmd [_iparray get ::subCmd]
    }
    if {[_iparray exists ::option]} {
        array set ::option [_iparray get ::option]
    }

    interp delete loadinterp

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
                echo "Parsing file $syntaxfile"
                parseFile $syntaxfile
            }
            if {$f == $syntaxfile} continue
            if {[file isfile $f] && [file readable $f]} {
                echo "Checking file $f"
                parseFile $f
            } else {
                if {$::Nagelfar(gui)} {
                    tk_messageBox -title "Nagelfar Error" -type ok -icon error \
                            -message "Could not find file '$f'"
                } else {
                    puts stderr "Could not find file '$f'"
                }
            }
        }
    }
    if {$::Nagelfar(gui)} {
        echo "Done"
        normalCursor
        progressUpdate -1
    }
}

# This shows the file and the line from an error in the result window.
proc showError {{lineNo {}}} {
    set w $::Nagelfar(resultWin) 
    if {$lineNo == ""} {
        set lineNo [lindex [split [$w index current] .] 0]
    }

    set line [$w get $lineNo.0 $lineNo.end]

    if {[regexp {^(.*): Line\s+(\d+):} $line -> fileName fileLine]} {
        editFile $fileName $fileLine
    } elseif {[regexp {^Line\s+(\d+):} $line -> fileLine]} {
        editFile "" $fileLine
    }
}

proc resultPopup {x y X Y} {
    set w $::Nagelfar(resultWin) 
    
    set lineNo [lindex [split [$w index @$x,$y] .] 0]
    set line [$w get $lineNo.0 $lineNo.end]

    destroy .popup
    menu .popup -tearoff 0

    if {[regexp {^(.*): Line\s+(\d+):} $line -> fileName fileLine]} {
        .popup add command -label "Show File" \
                -command [list editFile $fileName $fileLine]
    }
    if {[regexp {^(.*): Line\s+\d+:\s*(.*)$} $line -> pre post]} {
        .popup add command -label "Filter this message" \
                -command [list addFilter "*$pre*$post*"]
    }

    tk_popup .popup $X $Y
}

# Update the selection in the db listbox to or from the db list.
proc updateDbSelection {{fromVar 0}} {
    if {$fromVar} {
        $::Nagelfar(dbWin) selection clear 0 end
        foreach f $::Nagelfar(db) {
            set i [lsearch $::Nagelfar(allDb) $f]
            if {$i >= 0} {
                $::Nagelfar(dbWin) selection set $i
            }
        }
        return
    }

    set ::Nagelfar(db) {}
    foreach ix [$::Nagelfar(dbWin) curselection] {
        lappend ::Nagelfar(db) [lindex $::Nagelfar(allDb) $ix]
    }
}

if {[catch {package require snit}]} {
    namespace eval snit {
        proc widget {args} {}
    }
}
::snit::widget ScrollX {
    option -direction both
    option -auto 0

    delegate method * to child
    delegate option * to child

    constructor {class args} {
        set child [$class $win.s]
        $self configurelist $args
        grid $win.s -row 0 -column 0 -sticky news
        grid columnconfigure $win 0 -weight 1
        grid rowconfigure    $win 0 -weight 1

        # Move border properties to frame
        set bw [$win.s cget -borderwidth]
        set relief [$win.s cget -relief]
        $win configure -relief $relief -borderwidth $bw
        $win.s configure -borderwidth 0
    }

    method child {} {
        return $child
    }

    method SetScrollbar {sb from to} {
        $sb set $from $to
        if {$options(-auto) && $from == 0.0 && $top == 1.0} {
            grid remove $sb
        } else {
            grid $sb
        }
    }

    onconfigure -direction {value} {
        switch -- $value {
            both {
                set scrollx 1
                set scrolly 1
            }
            x {
                set scrollx 1
                set scrolly 0
            }
            y {
                set scrollx 0
                set scrolly 1
            }
            default {
                return -code error "Bad -direction \"$value\""
            }
        }
        set options(-direction) $value
        destroy $win.sbx $win.sby
        if {$scrollx} {
            $win.s configure -xscrollcommand [mymethod SetScrollbar $win.sbx]
            scrollbar $win.sbx -orient horizontal -command [list $win.s xview]
            grid $win.sbx -row 1 -sticky we
        } else {
            $win.s configure -xscrollcommand {}
        }
        if {$scrolly} {
            $win.s configure -yscrollcommand [mymethod SetScrollbar $win.sby]
            scrollbar $win.sby -orient vertical -command [list $win.s yview]
            grid $win.sby -row 0 -column 1 -sticky ns
        } else {
            $win.s configure -yscrollcommand {}
        }
        
    }

}

# A little helper to make a scrolled window
# It returns the name of the scrolled window
proc Scroll {dir class w args} {
    switch -- $dir {
        both {
            set scrollx 1
            set scrolly 1
        }
        x {
            set scrollx 1
            set scrolly 0
        }
        y {
            set scrollx 0
            set scrolly 1
        }
        default {
            return -code error "Bad scrolldirection \"$dir\""
        }
    }

    frame $w
    eval [list $class $w.s] $args

    # Move border properties to frame
    set bw [$w.s cget -borderwidth]
    set relief [$w.s cget -relief]
    $w configure -relief $relief -borderwidth $bw
    $w.s configure -borderwidth 0

    grid $w.s -sticky news

    if {$scrollx} {
        $w.s configure -xscrollcommand [list $w.sbx set]
        scrollbar $w.sbx -orient horizontal -command [list $w.s xview]
        grid $w.sbx -row 1 -sticky we
    }
    if {$scrolly} {
        $w.s configure -yscrollcommand [list $w.sby set]
        scrollbar $w.sby -orient vertical -command [list $w.s yview]
        grid $w.sby -row 0 -column 1 -sticky ns
    }
    grid columnconfigure $w 0 -weight 1
    grid rowconfigure    $w 0 -weight 1

    return $w.s
}

# Set the progress
proc progressUpdate {n} {
    if {$n < 0} {
        $::Nagelfar(progressWin) configure -relief flat
    } else {
        $::Nagelfar(progressWin) configure -relief solid
    }
    if {$n <= 0} {
        place $::Nagelfar(progressWin).f -x -100 -relx 0 -y 0 -rely 0 \
                -relheight 1.0 -relwidth 0.0
    } else {
        set frac [expr {double($n) / $::Nagelfar(progressMax)}]

        place $::Nagelfar(progressWin).f -x 0 -relx 0 -y 0 -rely 0 \
                -relheight 1.0 -relwidth $frac
    }
    update idletasks
}

# Set the 100 % level of the progress bar
proc progressMax {n} {
    set ::Nagelfar(progressMax) $n
    progressUpdate 0
}

# Create a simple progress bar
proc progressBar {w} {
    set ::Nagelfar(progressWin) $w

    frame $w -bd 1 -relief solid -padx 2 -pady 2 -width 100 -height 20
    frame $w.f -background blue

    progressMax 100
    progressUpdate -1
}

# A thing to easily get to debug mode
proc backDoor {a} {
    append ::Nagelfar(backdoor) $a
    set ::Nagelfar(backdoor) [string range $::Nagelfar(backdoor) end-9 end]
    if {$::Nagelfar(backdoor) eq "PeterDebug"} {
        set ::debug 1
        catch {console show}
        set ::Nagelfar(backdoor) ""
    }
}

# Create main window
proc makeWin {} {
    defaultGuiOptions

    catch {font create ResultFont -family courier \
            -size [lindex $::Prefs(resultFont) 1]}

    eval destroy [winfo children .]
    wm protocol . WM_DELETE_WINDOW exitApp
    wm title . "Nagelfar: Tcl Syntax Checker"
    wm withdraw .

    # Syntax database section

    frame .fs
    label .fs.l -text "Syntax database files"
    button .fs.b -text "Add" -width 10 -command addDbFile
    set lb [Scroll y listbox .fs.lb \
                    -listvariable ::Nagelfar(allDbView) \
                    -height 4 -width 40 -selectmode single]
    set ::Nagelfar(dbWin) $lb
    bind $lb <<ListboxSelect>> updateDbSelection

    grid .fs.l  .fs.b -sticky w -padx 2 -pady 2
    grid .fs.lb -     -sticky news
    grid columnconfigure .fs 0 -weight 1
    grid rowconfigure .fs 1 -weight 1
    
    
    # File section

    frame .ff
    label .ff.l -text "Tcl files to check"
    button .ff.b -text "Add" -width 10 -command addFile
    set lb [Scroll y listbox .ff.lb \
                    -listvariable ::Nagelfar(files) \
                    -height 4 -width 40]
    set ::Nagelfar(fileWin) $lb
    bind $lb <Key-Delete> "removeFile"
    bind $lb <Button-1> [list focus $lb]

    grid .ff.l  .ff.b -sticky w -padx 2 -pady 2
    grid .ff.lb -     -sticky news
    grid columnconfigure .ff 0 -weight 1
    grid rowconfigure .ff 1 -weight 1

    # Set up file dropping in listboxes if TkDnd is available
    if {![catch {package require tkdnd}]} {
        dnd bindtarget . text/uri-list <Drop> {fileDropFile %D}
        #dnd bindtarget $::Nagelfar(fileWin) text/uri-list <Drop> {fileDropFile %D}
        dnd bindtarget $::Nagelfar(dbWin) text/uri-list <Drop> {fileDropDb %D}
    }

    # Result section

    frame .fr
    progressBar .fr.pr
    button .fr.b -text "Check" -underline 0 -width 10 -command "doCheck"
    bind . <Alt-Key-c> doCheck
    bind . <Alt-Key-C> doCheck
    if {$::debug == 0} {
        bind . <Key> "backDoor %A"
    }

    set ::Nagelfar(resultWin) [Scroll both \
            text .fr.t -width 100 -height 25 -wrap none -font ResultFont]

    grid .fr.b .fr.pr -sticky w -padx 2 -pady {0 2}
    grid .fr.t -      -sticky news
    grid columnconfigure .fr 0 -weight 1
    grid rowconfigure    .fr 1 -weight 1

    $::Nagelfar(resultWin) tag configure error -foreground red
    bind $::Nagelfar(resultWin) <Double-Button-1> "showError ; break"
    bind $::Nagelfar(resultWin) <Button-3> "resultPopup %x %y %X %Y ; break"

    # Use the panedwindow in 8.4
    panedwindow .pw -orient vertical
    lower .pw
    frame .pw.f
    grid .fs x .ff -in .pw.f -sticky news 
    grid columnconfigure .pw.f {0 2} -weight 1 -uniform a
    grid columnconfigure .pw.f 1 -minsize 4
    grid rowconfigure .pw.f 0 -weight 1

    # Make sure the frames have calculated their size before
    # adding them to the pane
    # This update can be excluded in 8.4.4+
    update idletasks
    .pw add .pw.f -sticky news
    .pw add .fr   -sticky news
    pack .pw -fill both -expand 1
    

    # Menus

    menu .m
    . configure -menu .m

    .m add cascade -label "File" -underline 0 -menu .m.mf
    menu .m.mf
    .m.mf add command -label "Add File" -underline 4 -command addFile
    .m.mf add command -label "Add Database" -underline 4 -command addDbFile
    .m.mf add separator
    .m.mf add command -label "Quit" -underline 0 -command exitApp

    .m add cascade -label "Options" -underline 0 -menu .m.mo
    menu .m.mo

    .m.mo add cascade -label "Result Window Font" -menu .m.mo.mo
    menu .m.mo.mo
    .m.mo.mo add radiobutton -label "Small" \
	    -variable ::Prefs(resultFont) -value "Courier 8" \
	    -command {font configure ResultFont -size 8}
    .m.mo.mo add radiobutton -label "Medium" \
	    -variable ::Prefs(resultFont) -value "Courier 10" \
	    -command {font configure ResultFont -size 10}
    .m.mo.mo add radiobutton -label "Large" \
	    -variable ::Prefs(resultFont) -value "Courier 12" \
	    -command {font configure ResultFont -size 12}

    .m.mo add cascade -label "Severity level" -menu .m.mo.ms
    menu .m.mo.ms
    .m.mo.ms add radiobutton -label "Show All (N)" \
            -variable ::Prefs(severity) -value N
    .m.mo.ms add radiobutton -label {Show Warnings (W)} \
            -variable ::Prefs(severity) -value W
    .m.mo.ms add radiobutton -label {Show Errors (E)} \
            -variable ::Prefs(severity) -value E

    .m.mo add checkbutton -label "Warn about shortened subcommands" \
            -variable ::Prefs(warnShortSub)
    .m.mo add cascade -label "Braced expressions" -menu .m.mo.mb
    menu .m.mo.mb
    .m.mo.mb add radiobutton -label "Allow unbraced" \
            -variable ::Prefs(warnBraceExpr) -value 0
    .m.mo.mb add radiobutton -label {Allow 'if [cmd] {xxx}'} \
            -variable ::Prefs(warnBraceExpr) -value 1
    .m.mo.mb add radiobutton -label "Warn on any unbraced" \
            -variable ::Prefs(warnBraceExpr) -value 2
    .m.mo add checkbutton -label "Enforce else keyword" \
            -variable ::Prefs(forceElse)

    .m.mo add cascade -label "Script encoding" -menu .m.mo.me
    menu .m.mo.me
    .m.mo.me add radiobutton -label "Ascii" \
            -variable ::Nagelfar(encoding) -value ascii
    .m.mo.me add radiobutton -label "Iso8859-1" \
            -variable ::Nagelfar(encoding) -value iso8859-1
    .m.mo.me add radiobutton -label "System ([encoding system])" \
            -variable ::Nagelfar(encoding) -value system

    # Tools menu

    .m add cascade -label "Tools" -underline 0 -menu .m.mt
    menu .m.mt
    .m.mt add command -label "Edit Window" -underline 0 \
            -command {editFile "" 0}
    if {$::tcl_platform(platform) eq "windows"} {
        if {![catch {package require registry}]} {
            .m.mt add separator
            .m.mt add command -label "Setup Registry" -underline 6 \
                    -command makeRegistryWin
        }
    }

    # Debug menu

    if {$::debug == 1} {
        .m add cascade -label "Debug" -underline 0 -menu .m.md
        menu .m.md
        if {$::tcl_platform(platform) == "windows"} {
            .m.md add checkbutton -label Console -variable consolestate \
                    -onvalue show -offvalue hide \
                    -command {console $consolestate}
            .m.md add separator
        }
        .m.md add command -label "Reread Source" -command {source $thisScript}
        .m.md add separator
        .m.md add command -label "Redraw Window" -command {makeWin}
        #.m.md add separator
        #.m.md add command -label "Normal Cursor" -command {normalCursor}
    }

    # Help menu is last

    .m add cascade -label "Help" -underline 0 -menu .m.help
    menu .m.help
    foreach label {Messages {Syntax Databases}} \
            file {messages.txt syntaxdatabases.txt} {
        .m.help add command -label $label -command [list makeDocWin $file]
    }
    .m.help add separator
    .m.help add command -label About -command makeAboutWin

    wm deiconify .
}

#############################
# A simple file viewer/editor
#############################

proc editFile {filename lineNo} {
    if {[winfo exists .fv]} {
        wm deiconify .fv
        raise .fv
        set w $::Nagelfar(editWin)
    } else {
        toplevel .fv
        wm title .fv "Nagelfar Editor"

        set w [Scroll both text .fv.t \
                       -width 80 -height 25 -font $::Prefs(editFileFont)]
        set ::Nagelfar(editWin) $w
        frame .fv.f
        grid .fv.t -sticky news
        grid .fv.f -sticky we
        grid columnconfigure .fv 0 -weight 1
        grid rowconfigure .fv 0 -weight 1

        menu .fv.m
        .fv configure -menu .fv.m
        .fv.m add cascade -label "File" -underline 0 -menu .fv.m.mf
        menu .fv.m.mf
        .fv.m.mf add command -label "Save"  -underline 0 -command "saveFile"
        .fv.m.mf add separator
        .fv.m.mf add command -label "Close"  -underline 0 -command "closeFile"

        .fv.m add cascade -label "Edit" -underline 0 -menu .fv.m.me
        menu .fv.m.me
        .fv.m.me add command -label "Clear/Paste" -underline 6 \
                -command "clearAndPaste"
        .fv.m.me add command -label "Check" -underline 0 \
                -command "checkEditWin"

        .fv.m add cascade -label "Font" -underline 3 -menu .fv.m.mo
        menu .fv.m.mo
        set cmd "[list $w] configure -font \$::Prefs(editFileFont)"
        foreach lab {Small Medium Large} size {8 10 12} {
            .fv.m.mo add radiobutton -label $lab  -underline 0 \
                    -variable ::Prefs(editFileFont) \
                    -value [list Courier $size] \
                    -command $cmd
        }

        label .fv.f.ln -width 5 -anchor e -textvariable ::Nagelfar(lineNo)
        pack .fv.f.ln -side right -padx 3

        bind $w <Any-Key> {
            after idle {
                set ::Nagelfar(lineNo) \
                        [lindex [split [$::Nagelfar(editWin) index insert] .] 0]
            }
        }
        bind $w <Any-Button> [bind $w <Any-Key>]

        wm protocol .fv WM_DELETE_WINDOW closeFile
        $w tag configure hl -background yellow
        if {[info exists ::Nagelfar(editFileGeom)]} {
            wm geometry .fv $::Nagelfar(editFileGeom)
        } else {
            after idle {after 1 {
                set ::Nagelfar(editFileOrigGeom) [wm geometry .fv]
            }}
        }
    }

    if {$filename != "" && \
            (![info exists ::Nagelfar(editFile)] || \
            $filename != $::Nagelfar(editFile))} {
        $w delete 1.0 end
        set ::Nagelfar(editFile) $filename
        wm title .fv [file tail $filename]

        # Try to figure out eol style
        set ch [open $filename r]
        fconfigure $ch -translation binary
        set data [read $ch 400]
        close $ch
        
        set crCnt [expr {[llength [split $data \r]] - 1}]
        set lfCnt [expr {[llength [split $data \n]] - 1}]
        if {$crCnt == 0 && $lfCnt > 0} {
            set ::Nagelfar(editFileTranslation) lf
        } elseif {$crCnt > 0 && $crCnt == $lfCnt} {
            set ::Nagelfar(editFileTranslation) crlf
        } elseif {$lfCnt == 0 && $crCnt > 0} {
            set ::Nagelfar(editFileTranslation) cr
        } else {
            set ::Nagelfar(editFileTranslation) auto
        }
            
        #puts "EOL $::Nagelfar(editFileTranslation)"

        set ch [open $filename r]
        set data [read $ch]
        close $ch
        $w insert end $data
    }
   
    $w tag remove hl 1.0 end
    $w tag add hl $lineNo.0 $lineNo.end
    $w mark set insert $lineNo.0
    focus $w
    set ::Nagelfar(lineNo) $lineNo
    update
    $w see insert
    #after 1 {after idle {$::Nagelfar(editWin) see insert}}
}

proc saveFile {} {
    if {[tk_messageBox -parent .fv -title "Save File" -type okcancel \
            -icon question \
            -message "Save file\n$::Nagelfar(editFile)"] != "ok"} {
        return
    }
    set ch [open $::Nagelfar(editFile) w]
    fconfigure $ch -translation $::Nagelfar(editFileTranslation)
    puts -nonewline $ch [.fv.t get 1.0 end-1char]
    close $ch
}

proc closeFile {} {
    if {[info exists ::Nagelfar(editFileGeom)] || \
            ([info exists ::Nagelfar(editFileOrigGeom)] && \
             $::Nagelfar(editFileOrigGeom) != [wm geometry .fv])} {
        set ::Nagelfar(editFileGeom) [wm geometry .fv]
    }

    destroy .fv
    set ::Nagelfar(editFile) ""
}

proc clearAndPaste {} {
    set w $::Nagelfar(editWin)
    $w delete 1.0 end
    focus $w
    
    if {$::tcl_platform(platform) == "windows"} {
        event generate $w <<Paste>>
    } else {
        $w insert 1.0 [selection get]
    }
}

proc checkEditWin {} {
    set w $::Nagelfar(editWin)
    
    set script [$w get 1.0 end]
    set ::Nagelfar(checkEdit) $script
    doCheck
    unset ::Nagelfar(checkEdit)
}

######
# Help
######

proc helpWin {w title} {
    destroy $w

    toplevel $w
    wm title $w $title
    bind $w <Key-Return> "destroy $w"
    bind $w <Key-Escape> "destroy $w"
    frame $w.f
    button $w.b -text "Close" -command "destroy $w" -width 10 \
            -default active
    pack $w.b -side bottom -pady 3
    pack $w.f -side top -expand y -fill both
    focus $w
    return $w.f
}

proc makeAboutWin {} {
    global version

    set w [helpWin .ab "About Nagelfar"]


    text $w.t -width 45 -height 7 -wrap none -relief flat \
            -bg [$w cget -bg]
    pack $w.t -side top -expand y -fill both

    $w.t insert end "A syntax checker for Tcl\n\n"
    $w.t insert end "$version\n\n"
    $w.t insert end "Made by Peter Spjuth\n"
    $w.t insert end "E-Mail: peter.spjuth@space.se\n"
    $w.t insert end "\nURL: http://spjuth.pointclark.net/Nagelfar.html\n"
    $w.t insert end "\nTcl version: [info patchlevel]"
    set d [package provide tkdnd]
    if {$d != ""} {
        $w.t insert end "\nTkDnd version: $d"
    }
    set last [lindex [split [$w.t index end] "."] 0]
    $w.t configure -height $last
    $w.t configure -state disabled
}

proc makeDocWin {fileName} {
    set w [helpWin .doc "Nagelfar Help"]
    set t [Scroll both \
                   text $w.t -width 80 -height 25 -wrap none -font ResultFont]
    pack $w.t -side top -expand 1 -fill both

    if {![file exists $::thisDir/doc/$fileName]} {
        $t insert end "ERROR: Could not find doc file "
        $t insert end \"$fileName\"
        return
    }
    set ch [open $::thisDir/doc/$fileName r]
    set data [read $ch]
    close $ch

    $t insert end $data
    focus $t
    $t configure -state disabled
}

#########
# Options
#########

# FIXA This is not used yet
proc saveOptions {} {
    set ch [open "~/.nagelfarrc" w]

    foreach i [array names ::Prefs] {
        puts $ch [list set ::Prefs($i) $::Prefs($i)]
    }
    close $ch
}

proc getOptions {} {
    array set ::Prefs {
        warnBraceExpr 2
        warnShortSub 1
        forceElse 1
        noVar 0
        severity N
        editFileFont {Courier 10}
        resultFont {Courier 10}
    }

    if {[file exists "~/.nagelfarrc"]} {
        interp create -safe loadinterp
        interp expose loadinterp source
        interp eval loadinterp source "~/.nagelfarrc"
        array set ::Prefs [interp eval loadinterp array get ::Prefs]
        interp delete loadinterp
    }
}

# Generate a file path relative to a dir
proc fileRelative {dir file} {
    set dirpath [file split $dir]
    set filepath [file split $file]
    set newpath {}
    
    set dl [llength $dirpath]
    set fl [llength $filepath]
    for {set t 0} {$t < $dl && $t < $fl} {incr t} {
        set f [lindex $filepath $t]
        set d [lindex $dirpath $t]
        if {![string equal $f $d]} break
    }
    # Return file if too unequal
    if {$t <= 2 || ($dl - $t) > 3} {
        return $file
    }
    for {set u $t} {$u < $dl} {incr u} {
        lappend newpath ".."
    }
    return [eval file join $newpath [lrange $filepath $t end]]
}

proc defaultGuiOptions {} {
    catch {package require griffin}

    if {[tk windowingsystem]=="x11"} {
        option add *Menu.activeBorderWidth 1
        option add *Menu.borderWidth 1       
        
        option add *Menu.tearOff 0
        option add *Listbox.exportSelection 0
        option add *Listbox.borderWidth 1
        option add *Listbox.highlightThickness 1
        option add *Font "Helvetica -12"
    }

    if {$::tcl_platform(platform) == "windows"} {
        option add *Panedwindow.sashRelief flat
        option add *Panedwindow.sashWidth 4
        option add *Panedwindow.sashPad 0
    }
}

################################
# End of procs, global code here
################################

# Global code is only run first time to allow re-sourcing
if {![info exists gurka]} {
    set gurka 1
    set ::Nagelfar(db) {}
    set ::Nagelfar(files) {}
    set ::Nagelfar(gui) 0
    set ::Nagelfar(filter) {}
    set ::Nagelfar(2pass) 0
    set ::Nagelfar(encoding) system
    getOptions

    # Locate default syntax database(s)
    set ::Nagelfar(allDb) {}
    set ::Nagelfar(allDbView) {}
    set apa {}
    lappend apa [file join [pwd] syntaxdb.tcl]
    eval lappend apa [glob -nocomplain [file join [pwd] syntaxdb*.tcl]]

    lappend apa [file join $thisDir syntaxdb.tcl]
    eval lappend apa [glob -nocomplain [file join $thisDir syntaxdb*.tcl]]

    foreach file $apa {
        if {[file isfile $file] && [file readable $file] && \
                [lsearch $::Nagelfar(allDb) $file] == -1} {
            lappend ::Nagelfar(allDb) $file
            if {[file dirname $file] == $::thisDir} {
                lappend ::Nagelfar(allDbView) "[file tail $file] (app)"
            } else {
                lappend ::Nagelfar(allDbView) [fileRelative [pwd] $file]
            }
        }
    }
    
    # Parse command line options
    for {set i 0} {$i < $argc} {incr i} {
        set arg [lindex $argv $i]
        switch -glob -- $arg {
            --h* -
            -h* {
                usage
            }
            -s {
                incr i
                set arg [lindex $argv $i]
                if {[file isfile $arg] && [file readable $arg]} {
                    lappend ::Nagelfar(db) $arg
                    lappend ::Nagelfar(allDb) $arg
                    lappend ::Nagelfar(allDbView) $arg
                } else {
                    puts stderr "Cannot read \"$arg\""
                }
            }
            -encoding {
                incr i
                set enc [lindex $argv $i]
                if {$enc eq ""} {set enc system}
                if {[lsearch -exact [encoding names] $enc] < 0} {
                    puts stderr "Bad encoding name: \"$enc\""
                    set enc system
                }
                set ::Nagelfar(encoding) $enc
            }
            -2pass {
                set ::Nagelfar(2pass) 1
            }
            -gui {
                set ::Nagelfar(gui) 1
            }
            -novar {
                set ::Prefs(noVar) 1
            }
            -Wexpr* {
                set ::Prefs(warnBraceExpr) [string range $arg 6 end]
            }
            -Wsub* {
                set ::Prefs(warnShortSub) [string range $arg 5 end]
            }
            -Welse* {
                set ::Prefs(forceElse) [string range $arg 6 end]
            }
            -filter {
                incr i
                addFilter [lindex $argv $i]
            }
            -severity {
                incr i
                set ::Prefs(severity) [lindex $argv $i]
                if {![regexp {^[EWN]$} $::Prefs(severity)]} {
                    puts "Bad severity level '$::Prefs(severity)',\
                            should be E/W/N."
                    exit
                }
            }
            -* {
                puts "Unknown option $arg."
                usage
            }
            default {
                lappend ::Nagelfar(files) $arg
            }
        }
    }

    # Use default database if none were given
    if {[llength $::Nagelfar(db)] == 0} {
        if {[llength $::Nagelfar(allDb)] != 0} {
            lappend ::Nagelfar(db) [lindex $::Nagelfar(allDb) 0]
        }
    }

    # If there is no file specified, try invoking a GUI
    if {$::Nagelfar(gui) || [llength $::Nagelfar(files)] == 0} {
        if {[catch {package require Tk}]} {
            if {$::Nagelfar(gui)} {
                puts stderr "Failed to start GUI"
                exit 1
            } else {
                puts stderr "No files specified"
                exit 1
            }
        }
        set ::Nagelfar(gui) 1
        makeWin
        vwait forever
        exit
    }

    doCheck
    exit
}
