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
set version "Version 1.0.2+ 2004-09-10"

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
# 1 constant
# 2 braced
# 4 quoted
# 8 {expand}-ed

# Moved out message handling to make it more flexible
proc echo {str} {
    if {[info exists ::Nagelfar(resultWin)]} {
        $::Nagelfar(resultWin) configure -state normal
        $::Nagelfar(resultWin) insert end $str\n
        $::Nagelfar(resultWin) configure -state disabled
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
##nagelfar syntax profile x c
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

    # The forms ##syntax and ##variable are deprecated
    if {[string match "##syntax *" $str]} {
        set ::syntax([lindex $str 1]) [lrange $str 2 end]
    }
    if {[string match "##variable *" $str]} {
        set var [string trim [string range $str 11 end]]
        markVariable $var 1 "" 1 $index knownVars ""
    }

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
                addFilter "*Line *$line:*$first [join $rest]*"
            }
            default {
                errorMsg N "Bad type in ##nagelfar comment" $index
                return
            }
        }
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

    if {$::Nagelfar(allowExpand) && $c eq "\{"} {
        if {[string range $str $i [expr {$i + 7}]] eq "{expand}"} {
            set ni [expr {$i + 8}]
            set nc [string index $str $ni]
            if {![string is space $nc]} {
                # Non-space detected, it is expansion
                set c $nc
                set i $ni
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

    # How many is the limit imposed by the number of argument?
    set maxa [expr {[llength $argv] - $startI}]
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
                        errorMsg E "Ambigous option for $cmd,\
                                $arg -> [join $match /]" $index
                        set item ""
                    } else {
                        errorMsg W "Shortened option for $cmd,\
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
	return ""
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

    set minargs 0
    foreach token $syn {
        if {[string length $token] <= 1} {
            incr minargs
        }
    }
    set anyOptional [expr {($argc - $firsti) > $minargs}]

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
		    set ei $argc
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
                if {$mod eq "" || $anyOptional} {
                    set max 0
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
	    }
	    p {
                if {$mod eq "" || $anyOptional} {
                    set max 0
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
            set knownVars(known,$varBase) 1
            set knownVars(local,$varBase) 1
            set knownVars(type,$varBase)  $type
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
    } else {
        set cmd1 "${ns}::$cmd"
        if {[string match "::*" $cmd1]} {
            set cmd1 [string range $cmd1 2 end]
        }
        set cmd2 $cmd
    }

    if {[info exists ::knownAliases($cmd1)]} {
        return $::knownAliases($cmd1)
    }
    if {[info exists ::knownAliases($cmd2)]} {
        return $::knownAliases($cmd2)
    }

    if {[lsearch $::knownCommands $cmd] >= 0} {
        return [list $cmd]
    }
    if {[lsearch $::knownCommands $cmd1] >= 0} {
        return [list $cmd1]
    }
    if {$cmd2 != "" && [lsearch $::knownCommands $cmd2] >= 0} {
        return [list $cmd2]
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
        if {[string length $word] > 8 && [string match "{expand}*" $word]} {
            set ws 8
            set word [string range $word 8 end]
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
		if {($ws2 & 1) == 0} {
		    errorMsg W "No braces around code in switch\
                            statement." $i2
		}
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
                # Look for unknown parts
                if {[string is space [lindex $argv 2]]} {
                    # Empty body, do nothing
                } elseif {!([lindex $wordstatus 1] & 1) || \
                        !([lindex $wordstatus 2] & 1)} {
                    if {!$::Nagelfar(firstpass)} { # Messages in second pass
                        errorMsg N "Only braced namespace evals are checked." \
                                [lindex $indices 0]
                    }
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
                set type [checkCommand $cmd $index $argv $wordstatus \
                                  $wordtype $indices]
            }
	}
	uplevel { # FIXA
            set noConstantCheck 1
	}
	default {
	    if {[info exists ::syntax($cmd)]} {
		set type [checkCommand $cmd $index $argv $wordstatus \
                        $wordtype $indices]
	    } else {
                # Resolve commands in namespace
                set rescmd [lookForCommand $cmd [currentNamespace] $index]
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
        set var [lindex $a 0]
        set knownVars(known,$var) 1
        set knownVars(local,$var) 1
        set knownVars(set,$var)   1
        set knownVars(type,$var)  ""
    }

    lappend ::knownCommands $name

#    decho "Note: parsing procedure $name"
    if {!$::Nagelfar(firstpass)} {
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
    if {[lindex $lines end] eq ""} {
        set lines [lrange $lines 0 end-1]
    }
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
    array set ::knownAliases {}
    foreach g $knownGlobals {
	set knownVars(known,$g) 1
	set knownVars(set,$g)   1
	set knownVars(namespace,$g) ""
	set knownVars(type,$g)      ""
    }
    set script [buildLineDb $script]

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
            if {$cmd ne "\}"} {
                errorMsg W "Unknown command \"$cmd\"" $index
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
    set apa [tk_getOpenFile -title "Select file(s) to check" \
            -defaultextension .tcl -multiple 1 \
            -filetypes {{{Tcl File} {.tcl}} {{All Files} {.*}}}]
    if {[llength $apa] == 0} return

    foreach file $apa {
        lappend ::Nagelfar(files) [fileRelative [pwd] $file]
    }
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
##nagelfar syntax _ipsource x
##nagelfar syntax _ipexists l
##nagelfar syntax _ipset    v
##nagelfar syntax _iparray  s v
##nagelfar subcmd _iparray  exists get

# Load syntax database using safe interpreter
proc loadDatabases {} {
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
    # {expand} requires that Nagelfar is run in 8.5 since the checks for
    # it does not work otherwise.
    # It also naturally requires an 8.5 database to indicate that it is
    # checking 8.5 scripts
    if {[package vcompare $::Nagelfar(dbTclVersion) 8.5] >= 0 && \
            [package vcompare [package present Tcl] 8.5] >= 0} {
        set ::Nagelfar(allowExpand) 1
    } else {
        set ::Nagelfar(allowExpand) 0
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

    # Load syntax databases
    loadDatabases

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
    .m.mt add command -label "Browse Database" -underline 0 \
            -command makeDbBrowserWin
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
    foreach label {README Messages {Syntax Databases} {Inline Comments} {Call By Name} {Syntax Tokens}} \
            file {README.txt messages.txt syntaxdatabases.txt inlinecomments.txt call-by-name.txt syntaxtokens.txt} {
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

#############################
# Database browser
#############################

proc makeDbBrowserWin {} {
    if {[winfo exists .db]} {
        wm deiconify .db
        raise .db
        set w $::Nagelfar(dbBrowserWin)
    } else {
        toplevel .db
        wm title .db "Nagelfar Database"

        set w [Scroll y text .db.t -wrap word \
                       -width 80 -height 15 -font $::Prefs(editFileFont)]
        set ::Nagelfar(dbBrowserWin) $w
        $w tag configure all -lmargin2 2c
        set f [frame .db.f -padx 3 -pady 3]
        grid .db.f -sticky we
        grid .db.t -sticky news
        grid columnconfigure .db 0 -weight 1
        grid rowconfigure .db 1 -weight 1

        label $f.l -text "Command"
        entry $f.e -textvariable ::Nagelfar(dbBrowserCommand) -width 15
        button $f.b -text "Search" -command dbBrowserSearch -default active

        grid $f.l $f.e $f.b -sticky ew -padx 3
        grid columnconfigure $f 1 -weight 1

        bind .db <Key-Return> dbBrowserSearch
    }
}

proc dbBrowserSearch {} {
    set cmd $::Nagelfar(dbBrowserCommand)
    set w $::Nagelfar(dbBrowserWin)

    loadDatabases
    $w delete 1.0 end

    # Must be at least one word char in the pattern
    set pat $cmd*
    if {![regexp {\w} $pat]} {
        set pat ""
    }

    foreach item [lsort -dictionary [array names ::syntax $pat]] {
        $w insert end [list \#\#nagelfar syntax $item]
        $w insert end " "
        $w insert end $::syntax($item)\n
    }
    foreach item [lsort -dictionary [array names ::subCmd $pat]] {
        $w insert end [list \#\#nagelfar subcmd $item]
        $w insert end " "
        $w insert end $::subCmd($item)\n
    }
    foreach item [lsort -dictionary [array names ::option $pat]] {
        $w insert end [list \#\#nagelfar option $item]
        $w insert end " "
        $w insert end $::option($item)\n
    }
    foreach item [lsort -dictionary [array names ::return $pat]] {
        $w insert end [list \#\#nagelfar return $item]
        $w insert end " "
        $w insert end $::return($item)\n
    }

    if {[$w index end] eq "2.0"} {
        $w insert end "No match!"
    }
    $w tag add all 1.0 end
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
    catch {loadDatabases}
    if {[info exists ::Nagelfar(dbInfo)] &&  $::Nagelfar(dbInfo) != ""} {
        $w.t insert end "\nSyntax database: $::Nagelfar(dbInfo)"
    }
    set last [lindex [split [$w.t index end] "."] 0]
    $w.t configure -height $last
    $w.t configure -state disabled
}

# Insert a text file into a text widget.
# Any XML-style tags in the file are used as tags in the text window.
proc insertTaggedText {w file} {
    set ch [open $file r]
    set data [read $ch]
    close $ch

    set tags {}
    while {$data != ""} {
        if {[regexp {^([^<]*)<(/?)([^>]+)>(.*)$} $data -> pre sl tag post]} {
            $w insert end [subst -nocommands -novariables $pre] $tags
            set i [lsearch $tags $tag]
            if {$sl != ""} {
                # Remove tag
                if {$i >= 0} {
                    set tags [lreplace $tags $i $i]
                }
            } else {
                # Add tag
                lappend tags $tag
            }
            set data $post
        } else {
            $w insert end [subst -nocommands -novariables $data] $tags
            set data ""
        }
    }
}

proc makeDocWin {fileName} {
    set w [helpWin .doc "Nagelfar Help"]
    set t [Scroll both \
                   text $w.t -width 80 -height 25 -wrap none -font ResultFont]
    pack $w.t -side top -expand 1 -fill both

    # Set up tags
    $t tag configure ul -underline 1

    if {![file exists $::thisDir/doc/$fileName]} {
        $t insert end "ERROR: Could not find doc file "
        $t insert end \"$fileName\"
        return
    }
    insertTaggedText $t $::thisDir/doc/$fileName

    #focus $t
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
    set ::Nagelfar(2pass) 1
    set ::Nagelfar(encoding) system
    set ::Nagelfar(dbpicky) 0

    if {[info exists _nagelfar_test]} return

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
            -dbpicky { # A debug thing to help make a more complete database
                set ::Nagelfar(dbpicky) 1
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
