#!/bin/sh
#----------------------------------------------------------------------
#  Syntax.tcl, a syntax checker for Tcl.
#  Copyright (c) 1999-2002, Peter Spjuth  (peter.spjuth@space.se)
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

set debug 1
set version "Version 0.3+ 2002-11-14"
set thisScript [file join [pwd] [info script]]
set thisDir    [file dirname $thisScript]
set ::Syntax(tcl84) [expr {[info tclversion] >= 8.4}]

# Follow any link
while {[file type $thisScript] == "link"} {
    set tmplink [file readlink $thisScript]
    set thisDir [file dirname [file join $thisDir $tmplink]]
    unset tmplink
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
# 0 contains substitutions
# 1 constant
# 2 enclosed in braces, or constant in quotes

# Moved out message handling to make it more flexible
proc echo {str} {
    if {[info exists ::Syntax(resultWin)]} {
        $::Syntax(resultWin) insert end $str\n
    } else {
        puts $str
    }
    update
}

# Debug output
proc decho {str} {
    if {[info exists ::Syntax(resultWin)]} {
        $::Syntax(resultWin) insert end $str\n error
    } else {
        puts stderr $str
    }
    update
}

# Standard error message.
proc errorMsg {msg i} {
    if {[info exists ::Syntax(currentMessage)] && \
            $::Syntax(currentMessage) != ""} {
        lappend ::Syntax(messages) [list $::Syntax(currentMessageLine) \
                $::Syntax(currentMessage)]
    }

    set pre ""
    if {$::currentFile != ""} {
        set pre "$::currentFile: "
    }
    set line [calcLineNo $i]
    set pre "${pre}Line [format %3d $line]: "
    set ::Syntax(indent) [string repeat " " [string length $pre]]
    set ::Syntax(currentMessage) $pre$msg
    set ::Syntax(currentMessageLine) $line
}

# Continued message. Used to give extra info after an error.
proc contMsg {msg {i {}}} {
    append ::Syntax(currentMessage) "\n" $::Syntax(indent)
    if {$i != ""} {
        regsub -all {%L} $msg [calcLineNo $i] msg
    }
    append ::Syntax(currentMessage) $msg
}

#
proc initMsg {} {
    set ::Syntax(messages) {}
    set ::Syntax(currentMessage) ""
}

# Called after a file has been parsed, to flush messages
proc flushMsg {} {
    if {[info exists ::Syntax(currentMessage)] && \
            $::Syntax(currentMessage) != ""} {
        lappend ::Syntax(messages) [list $::Syntax(currentMessageLine) \
                $::Syntax(currentMessage)]
    }
    set msgs [lsort -integer -index 0 $::Syntax(messages)]
    foreach msg $msgs {
        echo [lindex $msg 1]
    }
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
    uplevel 1 $script
    incr profiledata($str) [expr {[clock clicks] - $apa}]
    incr profiledata($str,n)
}

# Experimental test for comments with unmatched braces.
proc checkPossibleComment {str lineNo} {

    set n1 [llength [split $str \{]]
    set n2 [llength [split $str \}]]
    if {$n1 != $n2} {
	echo "Experimental comment check: Unbalanced brace in comment. Line $lineNo."
    }
}

# This is called when a comment is encountered.
# It allows syntax information to be stored in comments
proc checkComment {str index} {
    if {[string match "##syntax *" $str]} {
	set ::syntax([lindex $str 1]) [lrange $str 2 end]
    }
    # FIXA
    # I would also like some check for unmatched braces.
    # That can't be done here since this is called after the
    # script is split, and the split will fail on an unmatched brace
    # Maybe splitScript can take care of it? Or buildLineDb?
}

# Handle a stack of current namespaces.
proc currentNamespace {} {
    lindex $::Syntax(namespaces) end
}

proc pushNamespace {ns} {
    lappend ::Syntax(namespaces) $ns
}

proc popNamespace {} {
    set ::Syntax(namespaces) [lrange $::Syntax(namespaces) 0 end-1]
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
        set closeChar [string range apa 1 0]
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
                errorMsg "Extra chars after closing $charType." \
                        [expr {$index + $i}]
                contMsg "Opening $charType of above was on line %L." \
                        [expr {$index + $si}]
                # Switch over to scanning for whitespace
                incr i
		break
            }
	}
    }
    # Regexp to locate unescaped whitespace
    set RE {(^|[^\\])(\\\\)*\s}

    for {} {$i < $len} {incr i} {
	# Search for unescaped whitespace
        set tmp [string range $str $i end]
        if {[regexp -indices $RE $tmp match]} {
            incr i [lindex $match 1]
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
# Returns the number of arguments "used".
# If 'pair' is set, all options should take a value.
proc checkOptions {cmd argv wordstatus indices {startI 0} {max 0} {pair 0}} {
    global option

    set check [info exists option($cmd)]
    set i 0
    set used 0
    set skip 0
    # Since in most cases startI is 0, I believe foreach is faster.
    foreach arg $argv ws $wordstatus index $indices {
	if {$skip} {
	    set skip 0
	    incr used
	    continue
	}
	if {$i < $startI} {
	    incr i
	    continue
	}
	if {[string equal [string index $arg 0] "-"]} {
	    incr used
	    set skip $pair
	    if {$ws != 0  && $check} {
                set ix [lsearch $option($cmd) $arg]
		if {$ix == -1} {
                    set ix [lsearch -glob $option($cmd) $arg*]
                    if {$ix == -1} {
                        errorMsg "Bad option $arg to $cmd" $index
                    } else {
                        errorMsg "Shortened option for $cmd,\
                                $arg ->\
                                [lindex $option($cmd) $ix]" \
                                [lindex $indices $i]
                    }
		}
                if {$ix != -1 && \
                     [info exists "option($cmd [lindex $option($cmd) $ix])"]} {
                    set skip 1
                }
	    }
	    if {[string equal $arg "--"]} {
		break
	    }
	} else {
	    break
	}
	if {$max != 0 && $used >= $max} {
	    break
	}
    }
    return $used
}

##syntax splitList x x n
# Make a list of a string. This is easy, just treat it as a list.
# But we must keep track of indices, so our own parsing is needed too.
proc splitList {str index iName} {
    upvar $iName indices

    # Make a copy to perform list operations on
    set lstr [string range $str 0 end]

    set indices {}
    if {[catch {set n [llength $lstr]}]} {
	errorMsg "Bad list" $index
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
	errorMsg "Length mismatch in splitList." $index
        echo "nindices: [llength $indices]  nwords: $n"
#        echo :$str:
        foreach l $lstr ix $indices {
            echo :$ix:[string range $l 0 10]:
        }
    }
    return $lstr
}

##syntax parseVar x x x n n
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
	    errorMsg "Could not find closing brace in variable reference." \
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
		errorMsg "Could not find closing parenthesis in variable\
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
    if {![info exists knownVars($var)]} {
	errorMsg "Unknown variable \"$var\"" $index
    }
    # Make use of markVariable. FIXA
    # If it's a constant array index, maybe it should be checked? FIXA
}

##syntax parseSubst x x n
# Check for substitutions in a word
# Check any variables referenced, and parse any commands within brackets.
# Returns 1 if the string is constant, i.e. no substitutions
# Returns 0 if any substitutions are present
proc parseSubst {str index knownVarsName} {
    upvar $knownVarsName knownVars

    # First do a quick check for $ or [
    if {[string first \$ $str] == -1 && [string first \[ $str] == -1} {
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
		    errorMsg "URGA:$si\n:$str:\n:[string range $str $si end]:" $index
		}
		incr si
		incr i -1
		parseBody [string range $str $si $i] [expr {$index + $si}] \
                        knownVars
		incr i
		set result 0
	    }
        } else {
            set escape 0
        }
    }
    return $result
}

##syntax parseExpr x x n
# Parse an expression
proc parseExpr {str index knownVarsName} {
    upvar $knownVarsName knownVars
    # Just check for variables and commands
    # This could maybe check more
    parseSubst $str $index knownVars
}

# A "macro" for checkCommand to print common error message
proc WA {} {
    upvar cmd cmd index index argc argc argv argv indices indices
    errorMsg "Wrong number of arguments ($argc) to \"$cmd\"" $index

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
# This is called from parseStatement, and was moved out to be able
# to call it recursively.
# 'firsti' says at which index in argv et.al. the arguments begin.
proc checkCommand {cmd index argv wordstatus indices {firsti 0}} {
    upvar constantsDontCheck constantsDontCheck knownVars knownVars

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
	return
    } elseif {[string equal [lindex $syn 0] "r"]} {
	if {($argc - $firsti) < [lindex $syn 1]} {
	    WA
	} elseif {[llength $syn] >= 3 && ($argc - $firsti) > [lindex $syn 2]} {
	    WA
	}
	return
    }

    set i $firsti
    foreach token $syn {
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
	    x {
		# x* matches anything up to the end.
		if {[string equal $mod "*"]} {
		    set i $argc
		    break
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
                        errorMsg "Warning: No braces around expression in\
                                $cmd statement." [lindex $indices $i]
                    } elseif {$::Prefs(warnBraceExpr)} {
                        # Allow pure command substitution if warnBraceExpr == 1
                        if {$::Prefs(warnBraceExpr) == 2 || \
                                [string index [lindex $argv $i] 0] != "\[" || \
                                [string index [lindex $argv $i] end] != "\]" } {
                            errorMsg "Warning: No braces around expression in\
                                    $cmd statement." [lindex $indices $i]
                        }
                    }
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
                        echo "(List code)"
                    } else {
                        errorMsg "Warning: No braces around code in $cmd\
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
		if {[lindex $wordstatus $i] == 0} {
		    errorMsg "Non static subcommand to \"$cmd\"" \
                            [lindex $indices $i]
		} else {
		    set arg [lindex $argv $i]
		    if {[info exists ::subCmd($cmd)]} {
			if {[lsearch $::subCmd($cmd) $arg] == -1} {
                            set ix [lsearch -glob $::subCmd($cmd) $arg*]
                            if {$ix == -1} {
                                errorMsg "Unknown subcommand \"$arg\" to \"$cmd\""\
                                        [lindex $indices $i]
                            } else {
                                # Check ambiguity.
                                # Just do it if 8.4 is available since
                                # its simple.
                                set match [list [lindex $::subCmd($cmd) $ix]]
                                if {$::Syntax(tcl84)} {
                                    set match [lsearch -all -inline -glob \
                                            $::subCmd($cmd) $arg*]
                                }
                                if {[llength $match] > 1} {
                                    errorMsg "Ambigous subcommand for $cmd,\
                                            $arg -> [join $match /]" \
                                            [lindex $indices $i]
                                } elseif {$::Prefs(warnShortSub)} {
                                    # Report shortened subcmd?
                                    errorMsg "Shortened subcommand for $cmd,\
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
		lappend constantsDontCheck $i
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
			    errorMsg "Unknown variable \"[lindex $argv $i]\""\
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
		set used [checkOptions $cmd $argv $wordstatus $indices $i $max]
		if {$used == 0 && ($mod == "" || $mod == ".")} {
		    errorMsg "Expected an option as argument $i to \"$cmd\"" \
                            [lindex $indices $i]
		    return
		}
		incr i $used
	    }
	    p {
		set max 0
		if {![string equal $mod "*"]} {
		    set max 2
		}
		set used [checkOptions $cmd $argv $wordstatus $indices $i \
                        $max 1]
		if {$used == 0 && ($mod == "" || $mod == ".")} {
		    errorMsg "Expected an option as argument $i to \"$cmd\"" \
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

##syntax markVariable x x x x n
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
	if {$check != 2} {
	    errorMsg "Suspicious variable name \"$var\"" $index
	}
	return 1;
    }

    if {$check == 2} {
	if {![info exists knownVars($varBase)]} {
	    return 1
	}
	if {$varArray && $varIndexWs != 0} {
	    if {![info exists knownVars($var)]} {
		return 1
	    }
	}
	return 0
    } else {
	set bit [expr {$check == 1 ? 4 : 0}]
	if {[info exists knownVars($varBase)]} {
	    set knownVars($varBase) [expr {$knownVars($varBase) | $bit}]
	} else {
	    set knownVars($varBase) [expr {1 | $bit}]
	}
	if {$varArray && $varIndexWs != 0} {
	    if {[info exists knownVars($var)]} {
		set knownVars($var) [expr {$knownVars($var) | $bit}]
	    } else {
		set knownVars($var) $knownVars($varBase)
	    }
	}
    }
}


proc lookForCommand {cmd ns index} {
    # Get both the namespace and global possibility
    if {[string match "::*" $cmd]} {
        set cmd1 $cmd
        set cmd2 ""
    } else {
        set cmd1 "${ns}::$cmd"
        set cmd2 "::$cmd"
    }

    if {[lsearch $::knownCommands $cmd] >= 0 || \
            [lsearch $::knownProcs $cmd] >= 0} {
        return
    }
    if {[lsearch $::knownCommands $cmd1] >= 0 || \
            [lsearch $::knownProcs $cmd1] >= 0} {
        return
    }
    if {$cmd2 != "" && ([lsearch $::knownCommands $cmd2] >= 0 || \
            [lsearch $::knownProcs $cmd2] >= 0)} {
        return
    }

    lappend ::unknownCommands [list $cmd $cmd1 $cmd2 $index]
}

##syntax parseStatement x x n
# Parse one statement and check the syntax of the command
proc parseStatement {statement index knownVarsName} {
    upvar $knownVarsName knownVars
    set words [splitStatement $statement $index indices]
    if {[llength $words] == 0} {return}

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


    # If the command contains substitutions, then we can not determine
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
	    set anyUnknown [expr !( [join $wordstatus *])]
	    if {$anyUnknown} {
		errorMsg "Non constant argument to proc \"[lindex $argv 0]\".\
                        Skipping." $index
		return
	    }
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
		    # 2 means it is a global
		    set knownVars($var) 2
		} else {
		    errorMsg "Non constant argument to $cmd: $var" $index
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
		    if {$i >= $argc - 1} {
			set knownVars($var) 1
		    } else {
			set knownVars($var) 5
		    }
		    lappend constantsDontCheck $i
		} else {
		    errorMsg "Non constant argument to $cmd: $var" $index
		}
		incr i 2
	    }
	}
	upvar {
	    if {$argc % 2 == 1} {
		set tmp [lrange $argv 1 end]
		set i 2
	    } else {
		set tmp $argv
		set i 1
	    }
	    foreach {other var} $tmp {
		set knownVars($var) 1
		lappend constantsDontCheck $i
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
		    errorMsg "Warning: Non constant variable list to foreach\
                            statement." [lindex $indices $i]
		    # FIXA, maybe abort here?
		}
		lappend constantsDontCheck $i
		foreach var [lindex $argv $i] {
		    markVariable $var 1 0 $index knownVars
		}
	    }
	    if {[lindex $wordstatus end] == 0} {
		errorMsg "Warning: No braces around body in foreach\
                        statement." $index
	    }
	    parseBody [lindex $argv end] [lindex $indices end] knownVars
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
                            errorMsg "Badly formed if statement" $index
                            contMsg "Found argument '[trimStr $arg]' where\
                                    else/elseif was expected."
                            return
                        }
		    }
		}
		switch -- $state {
		    expr {
			lappend ifsyntax e
			set state then
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
			errorMsg "Badly formed if statement" $index
			contMsg "Found argument '[trimStr $arg]' after\
                              supposed last body."
			return
		    }
		}
	    }
	    if {![string equal $state "else"] \
                    && ![string equal $state "illegal"]} {
		errorMsg "Badly formed if statement" $index
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
	    set i [checkOptions $cmd $argv $wordstatus $indices]
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
		    errorMsg "Odd number of elements in last argument to\
                            switch." $ix
		    return
		}
		if {[llength $swargv] == 0} {
		    errorMsg "Empty last argument to switch." $ix
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
		    errorMsg "Warning: Switch pattern starting with #.\
			    This could be a bad comment." $i1
		}
		if {[string equal $body -]} {
		    continue
		}
		if {$ws2 == 0} {
		    errorMsg "Warning: No braces around code in switch\
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
                    errorMsg "Warning: Expr without braces" [lindex $indices 0]
                }
            }
	}
	eval { # FIXA
            set noConstantCheck 1
	}
	interp { # FIXA
            set noConstantCheck 1
	}
	namespace { # FIXA
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
                set anyUnknown [expr !( [join $wordstatus *])]
                if {$anyUnknown} {
                    errorMsg "Only braced namespace evals are checked." \
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
        if {[info exists knownVars($var)]} {
            if {$ws == 1 && [lsearch $constantsDontCheck $i] == -1} {
                errorMsg "Found constant \"$var\" which is also a\
                        variable." [lindex $indices $i]
            }
        }
        incr i
    }
}

##syntax splitScript x x n n
# Split a script into individual statements
proc splitScript {script index statementsName indicesName} {
    upvar $statementsName statements $indicesName indices

    set statements {}
    set indices {}
    set lines [split $script \n]
    set tryline ""
    set newstatement 1
    set firstline ""
    string length $tryline

    foreach line $lines {
	append line \n
	while {![string equal $line ""]} {

            # Some extra checking on close braces to help finding
            # brace mismatches
            set closeBrace -1
            if {[regexp "^\}\\s*$" $line]} {
                set closeBraceIx [expr {[string length $tryline] + $index}]
                if {$newstatement} {
                    errorMsg "Close brace first in statement." $closeBraceIx
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
		    checkComment $tryline $index
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
                        errorMsg "Close brace not aligned with line\
                                [calcLineNo $index] ($tmp $closeBrace)" \
                                $closeBraceIx
                    }
                }
		incr index [string length $tryline]
		set tryline ""
                set newstatement 1
	    } elseif {!$closeBrace && \
                    ![string match "namespace eval*" $tryline]} {
                # A close brace that is not indented is typically the end of
                # a global statement, like "proc".
                # If it does not end the statement, there are probably a
                # brace mismatch.
                # When inside a namespace eval block, this is probably ok.
                errorMsg "Found non indented close brace that did not end\
                        statement." $closeBraceIx
                contMsg "This may indicate a brace mismatch."
            }
	}
    }
    # If tryline is non empty, it did not become complete
    if {[string length $tryline] != 0} {
        errorMsg "Could not complete statement." $index

        # Experiment a little to give more info.
        if {[info complete $firstline\}]} {
            contMsg "One close brace would complete the first line"
        } elseif {[info complete $firstline\}\}]} {
            contMsg "Two close braces would complete the first line"
        }
        if {[info complete $firstline\"]} {
            contMsg "One double quote would complete the first line"
        }
        if {[info complete $firstline\]]} {
            contMsg "One close bracket would complete the first line"
        }

        set txt "the script body at line\
                [calcLineNo [expr {$index + [string length $tryline] - 1}]]."
        if {[info complete $tryline\}]} {
            contMsg "One close brace would complete $txt"
            contMsg "Assuming completeness for further processing."
            lappend statements $tryline\}
            lappend indices $index
        } elseif {[info complete $tryline\}\}]} {
            contMsg "Two close braces would complete $txt"
            contMsg "Assuming completeness for further processing."
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

##syntax parseBody x x n
proc parseBody {body index knownVarsName} {
    upvar $knownVarsName knownVars

    splitScript $body $index statements indices

    foreach statement $statements index $indices {
	parseStatement $statement $index knownVars
    }
}

# This is called when a proc command is encountered.
proc parseProc {argv indices} {
    global knownProcs knownCommands knownGlobals syntax

    if {[llength $argv] != 3} {
	errorMsg "Wrong number of arguments to proc." [lindex $indices 0]
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
    #puts "proc $name -> $fullname ($cns) ($ns) ($tail)"
    set name $fullname

    # Parse the arguments.
    # Initialise a knownVars array with the arguments.
    # Build a syntax description for the procedure.
    set min 0
    set unlim 0
    array set knownVars {}
    foreach a $args {
	if {[llength $a] != 1} {
	    set knownVars([lindex $a 0]) 5
	    continue
	}
	if {[string equal $a "args"]} {
	    set unlim 1
	} else {
            incr min
        }
	set knownVars($a) 5
    }
    if {![info exists syntax($name)]} {
	if {$unlim} {
	    set syntax($name) [list r $min]
	} elseif {$min == [llength $args]} {
	    set syntax($name) $min
	} else {
	    set syntax($name) [list r $min [llength $args]]
	}
    }
    lappend knownProcs $name
    lappend knownCommands $name

#    decho "Note: parsing procedure $name"
    pushNamespace $ns
    parseBody $body [lindex $indices 2] knownVars
    popNamespace

    # Update known globals with those that were set in the proc.
    # Values in knownVars:
    # 1: local
    # 2: global
    # 4: Has been set.
    # I.e. anyone marked 6 should be added to known globals.
    foreach var [array names knownVars] {
	if {$knownVars($var) == 6} {
#	    decho "Set global $var in proc $name."
	    if {[lsearch $knownGlobals $var] == -1} {
		lappend knownGlobals $var
	    }
	}
    }
}

# Given an index in the original string, calculate its line number.
proc calcLineNo {i} {
    global newlineIx

    set n 1
    foreach ni $newlineIx {
        if {$ni > $i} {
            return $n
        }
        incr n
    }
    return [llength $newlineIx]
}

# Given an index in the original string, tell if that line was indented
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
    return $result
}

# Parse a global script
proc parseScript {script} {
    global knownGlobals unknownCommands knownProcs syntax

    catch {unset unknownCommands}
    set unknownCommands {}
    array set knownVars {}
    foreach g $knownGlobals {
	set knownVars($g) 6
    }
    set script [buildLineDb $script]
    pushNamespace {}
    parseBody $script 0 knownVars
    popNamespace

    # Check commands that where unknown when encountered
    foreach apa $unknownCommands {
        foreach {cmd cmd1 cmd2 index} $apa break
        if {![info exists syntax($cmd1)] && \
                [lsearch $knownProcs $cmd1] == -1 && \
                ![info exists syntax($cmd2)] && \
                [lsearch $knownProcs $cmd2] == -1} {
            errorMsg "Unknown command \"$cmd\"" $index
        }
    }
    #Update known globals.
    foreach var [array names knownVars] {
	# Check if it has been set.
	if {($knownVars($var) & 4) == 4} {
	    if {[lsearch $knownGlobals $var] == -1} {
		lappend knownGlobals $var
	    }
	}
    }
}

# Parse a file
proc parseFile {filename} {
    set ch [open $filename]
    set script [read $ch]
    close $ch

    initMsg
    parseScript $script
    flushMsg
}

proc usage {} {
    puts $::version
    puts {Usage: syntax.tcl [options] [-s dbfile] scriptfile ...}
    puts { -h        : Show usage.}
    puts { -gui      : Start with GUI even when files are specified.}
    puts { -s dbfile : Include a database file. (More than one is allowed.)}
    puts { -WexprN   : Sets expression warning level.}
    puts {   2 (def) = Warn about any unbraced expression.}
    puts {   1       = Don't warn on single commands. "if [apa] {...}" is ok.}
    puts { -WsubN    : Sets subcommand warning level.}
    puts {   1 (def) = Warn about shortened subcommands.}
    exit
}

###########
# GUI stuff
###########

proc exitApp {} {
    exit
}

# Browse for and add a syntax database file
proc addDbFile {} {
    set apa [tk_getOpenFile -title "Select db file"]
    if {$apa == ""} return

    lappend ::Syntax(db) $apa
    lappend ::Syntax(allDb) $apa
    updateDbSelection 1
}

# File drop using TkDnd
proc fileDropDb {files} {
    foreach file $files {
        lappend ::Syntax(db) $file
        lappend ::Syntax(allDb) $file
    }
    updateDbSelection 1
}

# Browse for and add a file to check.
proc addFile {} {
    set apa [tk_getOpenFile -title "Select file to check" \
            -defaultextension .tcl \
            -filetypes {{{Tcl File} {.tcl}} {{All Files} {.*}}}]
    if {$apa == ""} return

    lappend ::Syntax(files) $apa
}

# Remove a file from the list to check
proc removeFile {} {
    set ixs [lsort -decreasing -integer [.ff.lb curselection]]
    foreach ix $ixs {
        set ::Syntax(files) [lreplace $::Syntax(files) $ix $ix]
    }
}

# File drop using TkDnd
proc fileDropFile {files} {
    foreach file $files {
        lappend ::Syntax(files) $file
    }
}

# Execute the checks
proc doCheck {} {
    if {[llength $::Syntax(db)] == 0} {
        if {$::Syntax(gui)} {
            tk_messageBox -title "Syntax Error" -type ok -icon error \
                    -message "No syntax database file selected"
            return
        } else {
            puts stderr "No syntax database file found"
            exit 1
        }
    }

    if {[llength $::Syntax(files)] == 0} {
        tk_messageBox -title "Syntax Error" -type ok -icon error \
                -message "No files to check"
        return
    }

    set ::Syntax(editFile) ""
    if {[info exists ::Syntax(resultWin)]} {
        $::Syntax(resultWin) delete 1.0 end
    }

    # Clear the database first
    set ::knownGlobals {}
    set ::knownCommands {}
    set ::knownProcs {}
    catch {unset ::syntax}
    catch {unset ::subCmd}
    catch {unset ::options}

    foreach f $::Syntax(db) {
        uplevel #0 [list source $f]
    }

    set ::currentFile ""
    foreach f $::Syntax(files) {
        if {$::Syntax(gui) || [llength $::Syntax(files)] > 1} {
            set ::currentFile $f
        }
        set syntaxfile [file rootname $f].syntax
        if {[file exists $syntaxfile]} {
            echo "Parsing file $syntaxfile"
            parseFile $syntaxfile
        }
        if {[file exists $f]} {
            echo "Checking file $f"
            parseFile $f
        } else {
            if {$::Syntax(gui)} {
                tk_messageBox -title "Syntax Error" -type ok -icon error \
                        -message "Could not find file $f"
            } else {
                puts stderr "Could not find file $f"
            }
        }
    }
}

# This shows the file and the line from an error in the result window.
proc showError {{lineNo {}}} {
    set w $::Syntax(resultWin) 
    if {$lineNo == ""} {
        set lineNo [lindex [split [$w index current] .] 0]
    }

    set line [$w get $lineNo.0 $lineNo.end]

    if {[regexp {^(.*): Line\s+(\d+):} $line -> fileName fileLine]} {
        editFile $fileName $fileLine
    }
}

# Update the selection in the db listbox to or from the db list.
proc updateDbSelection {{fromVar 0}} {
    if {$fromVar} {
        .fs.lb selection clear 0 end
        foreach f $::Syntax(db) {
            set i [lsearch $::Syntax(allDb) $f]
            if {$i >= 0} {
                .fs.lb selection set $i
            }
        }
        return
    }

    set ::Syntax(db) {}
    foreach ix [.fs.lb curselection] {
        lappend ::Syntax(db) [lindex $::Syntax(allDb) $ix]
    }
}

# Create main window
proc makeWin {} {
    option add *Menu.tearOff 0
    option add *Listbox.exportSelection 0

    eval destroy [winfo children .]
    wm protocol . WM_DELETE_WINDOW exitApp
    wm title . "Tcl Syntax Checker"
    wm withdraw .

    # Syntax file section

    frame .fs
    label .fs.l -text "Syntax database files"
    button .fs.b -text "Add" -command addDbFile
    listbox .fs.lb -yscrollcommand ".fs.sby set" -listvariable ::Syntax(allDb) \
            -height 4 -width 40 -selectmode extended
    updateDbSelection 1
    bind .fs.lb <<ListboxSelect>> updateDbSelection
    scrollbar .fs.sby -orient vertical -command ".fs.lb yview"

    grid .fs.l -sticky w
    grid .fs.b -sticky w
    grid .fs.lb .fs.sby -sticky news
    grid columnconfigure .fs 0 -weight 1
    grid rowconfigure .fs 2 -weight 1
    
    
    # File section

    frame .ff
    label .ff.l -text "Tcl files to check"
    button .ff.b -text "Add" -command addFile
    listbox .ff.lb -yscrollcommand ".ff.sby set" \
            -listvariable ::Syntax(files) \
            -height 4 -width 40
    scrollbar .ff.sby -orient vertical -command ".ff.lb yview"
    bind .ff.lb <Key-Delete> "removeFile"
    bind .ff.lb <Button-1> "focus .ff.lb"

    grid .ff.l -sticky w
    grid .ff.b -sticky w
    grid .ff.lb .ff.sby -sticky news
    grid columnconfigure .ff 0 -weight 1
    grid rowconfigure .ff 2 -weight 1

    # Set up file dropping in listboxes if TkDnd is available
    if {![catch {package require tkdnd}]} {
        dnd bindtarget .fs.lb text/uri-list <Drop> {fileDropDb %D}
        dnd bindtarget .ff.lb text/uri-list <Drop> {fileDropFile %D}
    }

    # Result section

    set ::Syntax(resultWin) .fr.t
    frame .fr
    button .fr.b -text "Check" -command "doCheck"
    text .fr.t -width 100 -height 25 -wrap none -font "Courier 8" \
            -xscrollcommand ".fr.sbx set" \
            -yscrollcommand ".fr.sby set"
    scrollbar .fr.sbx -orient horizontal -command ".fr.t xview"
    scrollbar .fr.sby -orient vertical -command ".fr.t yview"

    grid .fr.b         -sticky w
    grid .fr.t .fr.sby -sticky news
    grid .fr.sbx       -sticky we
    grid columnconfigure .fr 0 -weight 1
    grid rowconfigure .fr 1 -weight 1

    .fr.t tag configure error -foreground red
    bind .fr.t <Double-Button-1> "showError ; break"

    # Use the panedwindow in 8.4
    if {[package present Tk] >= 8.4} {
        panedwindow .pw -orient vertical
        lower .pw
        frame .pw.f
        grid .fs .ff -in .pw.f -sticky news 
        grid columnconfigure .pw.f {0 1} -weight 1 -uniform a
        grid rowconfigure .pw.f 0 -weight 1

        # Make sure the frames have calculated their size before
        # adding them to the pane
        update idletasks
        .pw add .pw.f -sticky news
        .pw add .fr   -sticky news
        pack .pw -fill both -expand 1
    } else {
        grid .fs .ff -sticky news
        grid .fr -   -sticky news
        grid columnconfigure . {0 1} -weight 1
        grid rowconfigure . 1 -weight 1
    }

    # Menus

    menu .m
    . configure -menu .m

    .m add cascade -label "File" -menu .m.mf
    menu .m.mf
    .m.mf add command -label "Add File" -command addFile
    .m.mf add command -label "Add Database" -command addDbFile
    .m.mf add separator
    .m.mf add command -label "Quit" -command exitApp

    .m add cascade -label "Options" -menu .m.mo
    menu .m.mo
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

    # Debug menu

    if {$::debug == 1} {
        .m add cascade -label "Debug" -menu .m.md
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

    .m add cascade -label "Help" -menu .m.mh
    menu .m.mh
    .m.mh add command -label About -command makeAboutWin


    wm deiconify .
}

#############################
# A simple file viewer/editor
#############################

proc editFile {filename lineNo} {
    if {[winfo exists .fv]} {
        wm deiconify .fv
        raise .fv
    } else {
        toplevel .fv
	if {![info exists ::Syntax(editFileFont)]} {
	    set ::Syntax(editFileFont) "Courier -12"
	}

        text .fv.t -width 80 -height 25 -font $::Syntax(editFileFont) \
                -xscrollcommand ".fv.sbx set" \
                -yscrollcommand ".fv.sby set"
        scrollbar .fv.sbx -orient horizontal -command ".fv.t xview"
        scrollbar .fv.sby -orient vertical   -command ".fv.t yview"
        frame .fv.f
        grid .fv.t .fv.sby -sticky news
        grid .fv.sbx       -sticky we
        grid .fv.f -       -sticky we
        grid columnconfigure .fv 0 -weight 1
        grid rowconfigure .fv 0 -weight 1

        menu .fv.m
        .fv configure -menu .fv.m
        .fv.m add cascade -label "File" -menu .fv.m.mf
        menu .fv.m.mf
        .fv.m.mf add command -label "Save" -command "saveFile"
        .fv.m.mf add separator
        .fv.m.mf add command -label "Close" -command "closeFile"

        .fv.m add cascade -label "Font" -menu .fv.m.mo
        menu .fv.m.mo
	.fv.m.mo add radiobutton -label "Courier -10" \
	    -variable ::Syntax(editFileFont) -value "Courier -10" \
	    -command {.fv.t configure -font $::Syntax(editFileFont)}
	.fv.m.mo add radiobutton -label "Courier -12" \
	    -variable ::Syntax(editFileFont) -value "Courier -12" \
	    -command {.fv.t configure -font $::Syntax(editFileFont)}
	.fv.m.mo add radiobutton -label "Courier -14" \
	    -variable ::Syntax(editFileFont) -value "Courier -14" \
	    -command {.fv.t configure -font $::Syntax(editFileFont)}

        label .fv.f.ln -width 5 -textvariable ::Syntax(lineNo)
        pack .fv.f.ln -side right

        bind .fv.t <Any-Key> {
            after idle {
                set ::Syntax(lineNo) [lindex [split [.fv.t index insert] .] 0]
            }
        }
        wm protocol .fv WM_DELETE_WINDOW closeFile
        .fv.t tag configure hl -background yellow
        if {[info exists ::Syntax(editFileGeom)]} {
            wm geometry .fv $::Syntax(editFileGeom)
        } else {
            after idle {after 1 {
                set ::Syntax(editFileOrigGeom) [wm geometry .fv]
            }}
        }
    }

    if {![info exists ::Syntax(editFile)] || \
            $filename != $::Syntax(editFile)} {
        .fv.t delete 1.0 end
        set ::Syntax(editFile) $filename
        wm title .fv [file tail $filename]

        # Try to figure out eol style
        set ch [open $filename r]
        fconfigure $ch -translation binary
        set data [read $ch 400]
        close $ch
        
        set crCnt [expr {[llength [split $data \r]] - 1}]
        set lfCnt [expr {[llength [split $data \n]] - 1}]
        if {$crCnt == 0 && $lfCnt > 0} {
            set ::Syntax(editFileTranslation) lf
        } elseif {$crCnt > 0 && $crCnt == $lfCnt} {
            set ::Syntax(editFileTranslation) crlf
        } elseif {$lfCnt == 0 && $crCnt > 0} {
            set ::Syntax(editFileTranslation) cr
        } else {
            set ::Syntax(editFileTranslation) auto
        }
            
        #puts "EOL $::Syntax(editFileTranslation)"

        set ch [open $filename r]
        set data [read $ch]
        close $ch
        .fv.t insert end $data
    }
   
    .fv.t tag remove hl 1.0 end
    .fv.t tag add hl $lineNo.0 $lineNo.end
    .fv.t mark set insert $lineNo.0
    focus .fv.t
    set ::Syntax(lineNo) $lineNo
    #update idletasks
    after 1 {after idle {.fv.t see insert}}
}

proc saveFile {} {
    if {[tk_messageBox -parent .fv -title "Save File" -type okcancel \
            -icon question \
            -message "Save file\n$::Syntax(editFile)"] != "ok"} {
        return
    }
    set ch [open $::Syntax(editFile) w]
    fconfigure $ch -translation $::Syntax(editFileTranslation)
    puts -nonewline $ch [.fv.t get 1.0 end-1char]
    close $ch
}

proc closeFile {} {
    if {[info exists ::Syntax(editFileGeom)] || \
            ([info exists ::Syntax(editFileOrigGeom)] && \
             $::Syntax(editFileOrigGeom) != [wm geometry .fv])} {
        set ::Syntax(editFileGeom) [wm geometry .fv]
    }

    destroy .fv
    set ::Syntax(editFile) ""
}


######
# Help
######

proc makeAboutWin {} {
    global version
    destroy .ab

    toplevel .ab
    wm title .ab "About syntax checker"
    text .ab.t -width 45 -height 7 -wrap word
    button .ab.b -text "Close" -command "destroy .ab"
    pack .ab.b -side bottom
    pack .ab.t -side top -expand y -fill both

    .ab.t insert end "A syntax checker for Tcl\n\n"
    .ab.t insert end "$version\n\n"
    .ab.t insert end "Made by Peter Spjuth\n"
    .ab.t insert end "E-Mail: peter.spjuth@space.se"
}

#########
# Options
#########

proc saveOptions {} {
    set ch [open "~/.syntaxrc" w]

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
    }

    if {[file exists "~/.syntaxrc"]} {
        source "~/.syntaxrc"
    }
}

################################
# End of procs, global code here
################################

# Global code is only run first time to allow re-sourcing
if {![info exists gurka]} {
    set gurka 1
    set ::Syntax(db) {}
    set ::Syntax(files) {}
    set ::Syntax(gui) 0
    getOptions

    # Locate default syntax database(s)
    set ::Syntax(allDb) {}
    set apa {}
    lappend apa [file join [pwd] syntaxdb.tcl]
    eval lappend apa [glob -nocomplain [file join [pwd] syntaxdb*.tcl]]
    while {$thisDir != "." && [file tail $thisDir] == "."} {
	set thisDir [file dirname $thisDir]
    }
    lappend apa [file join $thisDir syntaxdb.tcl]
    eval lappend apa [glob -nocomplain [file join $thisDir syntaxdb*.tcl]]

    foreach file $apa {
        if {[file exists $file] && [lsearch $::Syntax(allDb) $file] == -1} {
            lappend ::Syntax(allDb) $file
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
                lappend ::Syntax(db) [lindex $argv $i]
                lappend ::Syntax(allDb) [lindex $argv $i]
            }
            -gui {
                set ::Syntax(gui) 1
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
            -* {
                puts "Unknown option $arg."
                usage
            }
            default {
                lappend ::Syntax(files) $arg
            }
        }
    }

    # Use default database if none were given
    if {[llength $::Syntax(db)] == 0} {
        if {[llength $::Syntax(allDb)] != 0} {
            lappend ::Syntax(db) [lindex $::Syntax(allDb) 0]
        }
    }

    # If there is no file specified, try invoking a GUI
    if {$::Syntax(gui) || [llength $::Syntax(files)] == 0} {
        if {[catch {package require Tk}]} {
            if {$::Syntax(gui)} {
                puts stderr "Failed to start GUI"
                exit 1
            } else {
                puts stderr "No files specified"
                exit 1
            }
        }
        set ::Syntax(gui) 1
        makeWin
        vwait forever
        exit
    }

    doCheck
    exit
}
