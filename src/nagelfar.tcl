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

# Collect default syntax data
# This must be done first, before we start to pollute.
set defknownGlobals  [info globals]
set defknownCommands [info commands]
set defknownProcs    [info procs]

set version "Version 0.1 2002-08-21"
set thisScript [file join [pwd] [info script]]
set thisDir    [file dirname $thisScript]
if {[file type $thisScript] == "link"} {
    set tmplink [file readlink $thisScript]
    set thisDir [file dirname [file join $thisDir $tmplink]]
    unset tmplink
}

# TODO: Handle widgets -command options, bind code and other callbacks
#       Handle e.g. -textvariable
#       Handle namespaces and qualified vars
#       Handle multiple files. Remember variables.
#       Everything marked FIXA
#       Give this program a silly name. (maybe Psyche, Peter's SYntax CHEcker)
#       Optimise. Always optimise.
#       Tidy up messages. Tidy up code structure. Things are getting messy.

# Arguments to many procedures:
# index     : Index of the start of a string or command.
# cmd       : Command
# argv      : List of arguments
# wordstatus: List of status for the words in argv
# indices   : List of indices where every word in argv starts
# knownVars : An array that keeps track of variables known in this scope

# Moved out message handling to make it more flexible
proc echo {str} {
    puts $str
    update
}

# Debug output
proc decho {str} {
    puts stderr $str
    update
}

proc errorMsg {msg i} {
    global currentFile

    set pre ""
    if {$currentFile != ""} {
        set pre "$currentFile: "
    }

    echo "${pre}Line [calcLineNo $i]: $msg"
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
	echo "Test: Unbalanced brace in comment. Line $lineNo."
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

##syntax skipWS x x v
# Move "i" forward to the first non whitespace char
proc skipWS {str len iName} {
    upvar $iName i

    set j [string length [string trimleft [string range $str $i end]]]
    set i [expr {$len - $j}]
}

##syntax scanWord x x x v
# Scan the string until the end of one word is found.
# When entered, i points to the start of the word.
# When returning, i points to the last char of the word.
proc scanWord {str len index iName} {
    upvar $iName i

    set si $i
    set c [string index $str $i]

    if {[string equal $c "\{"]} {
        set closeChar \}
    } elseif {[string equal $c "\""]} {
        set closeChar \"
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
                errorMsg "Extra chars after closing brace or quote." \
                        [expr {$index + $i}]
                errorMsg " Opening brace/quote of above was on this line."\
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

##syntax splitStatement x x n
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
    set words
}

# FIXA Options may be non constant.

# Look for options in a command's arguments.
# Check them against the list in the option database, if any.
# Returns the number of arguments "used".
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
		if {[lsearch $option($cmd) $arg] == -1} {
		    errorMsg "Bad option $arg to $cmd" $index
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

    if {$var == ""} {
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

# A "macro" to print common error message
proc WA {} {
    upvar cmd cmd index index argc argc argv argv indices indices
    errorMsg "Wrong number of arguments ($argc) to \"$cmd\"" $index

    set t 1
    set line [calcLineNo $index]
    foreach ix $indices {
        set aline [calcLineNo $ix]
        if {$aline != $line} {
            echo " Argument $t at line $aline"
        }
        incr t
    }
}

# Check a command that have a syntax defined in the database
# This is called from parseStatement, and was moved out to be able
# to call it recursively.
proc checkCommand {cmd index argv wordstatus indices {firsti 0}} {
    upvar constantsDontCheck constantsDontCheck knownVars knownVars
    global Prefs
    set argc [llength $argv]
    set syn $::syntax($cmd)
#    decho "Checking $cmd against syntax $syn"

    if {[string is integer $syn]} {
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
    foreach token $::syntax($cmd) {
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
	    e { # An expression
		if {![string equal $mod ""]} {
		    echo "Modifier \"$mod\" is not supported for \"e\" in\
                            syntax for $cmd."
		}
		if {[lindex $wordstatus $i] == 0 && $Prefs(warnBraceExpr)} {
                    # Allow pure command substitution if warnBraceExpr == 1
                    if {$Prefs(warnBraceExpr) == 2 || \
                            [string index [lindex $argv $i] 0] != "\[" || \
                            [string index [lindex $argv $i] end] != "\]" } {
                        errorMsg "Warning: No braces around expression in\
                                $cmd statement." [lindex $indices $i]
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
                                #Check ambiguity? FIXA
                                #Report shortened subcmd?
                                if {$Prefs(warnShortSub)} {
                                    errorMsg "Shortened subcommand for $cmd,\
                                            $arg ->\
                                            [lindex $::subCmd($cmd) $ix]" \
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
			if {[markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] 2 knownVars]} {
			    errorMsg "Unknown variable \"[lindex $argv $i]\""\
                                    $index
			}
		    } elseif {[string equal $tok "n"]} {
			markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] 1 knownVars
		    } else {
			markVariable [lindex $argv $i] \
                                [lindex $wordstatus $i] 0 knownVars
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

##syntax markVariable x x x n
# Central function to handle known variable names.
# If check is 2, check if it is known, return 1 if unknown
# If check is 1, mark the variable as known and set
# If check is 0, mark the variable as known
proc markVariable {var ws check knownVarsName} {
    upvar $knownVarsName knownVars
    
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
	if {$varBaseWs == 0 && [regexp {^(::)?(\w+(::)?)+$} $varBase]} {
	    set varBaseWs 1
	}
	set varArray 1
    }

    if {$check == 2} {
	if {$varBaseWs != 0} {
	    if {![info exists knownVars($varBase)]} {
		return 1
	    }
	    if {$varArray && $varIndexWs != 0} {
		if {![info exists knownVars($var)]} {
		    return 1
		}
	    }
	}
	return 0
    } else {
	if {$varBaseWs != 0} {
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
        set c [string index $word 0]
        if {[string equal $c "\{"]} {
            lappend words2 [string range $word 1 end-1]
            lappend wordstatus 2
	    incr index
        } else {
            if {[string equal $c "\""]} {
                set word [string range $word 1 end-1]
		incr index
            }
	    lappend words2 $word
            lappend wordstatus [parseSubst $word $index knownVars]
        }
	lappend indices2 $index
    }
    
    # Interpretation of wordstatus:
    # 0 contains substitutions
    # 1 constant
    # 2 enclosed in braces

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
	    lappend constantsDontCheck all
	}
	.* { # FIXA, kolla kod i ev. -command. Aven widgetkommandon ska kollas.
	     # Kanske i checkOptions ?
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
	    lappend constantsDontCheck all
	}
	variable {
	    # FIXA, namespaces?
	    set i 0
	    foreach {var val} $argv {ws1 ws2} $wordstatus {
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
	set {
	    # Set gets a different syntax string depending on the
	    # number of arguments. 
	    # If more commands needs this, maybe a general way to
	    # express it in the syntax db should be implemented. FIXA?
	    if {$argc == 1} {
                set ::syntax(set) "v"
            } elseif {$argc == 2} {
                set ::syntax(set) "n x"
            } else {
		WA
		return
	    }
	    checkCommand $cmd $index $argv $wordstatus $indices
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
		    markVariable $var 1 0 knownVars
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
			echo "  Found arguments after supposed last body."
			return
		    }
		}
	    }
	    if {![string equal $state "else"] \
                    && ![string equal $state "illegal"]} {
		errorMsg "Badly formed if statement" $index
		echo "  Missing one body."
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
		set swwordstatus {}
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
            #Take care of the standard case of a brace enclosed expr.
            if {$argc == 1 && [lindex $wordstatus 0] == 2} {
                 parseExpr [lindex $argv 0] [lindex $indices 0] knownVars
            } else {
                if {$::Prefs(warnBraceExpr)} {
                    errorMsg "Warning: Expr without braces" [lindex $indices 0]
                }
            }
	}
	eval { # FIXA
            lappend constantsDontCheck all
	}
	interp { # FIXA
            lappend constantsDontCheck all
	}
	namespace { # FIXA
            lappend constantsDontCheck all
	}
	uplevel { # FIXA
            lappend constantsDontCheck all
	}
	default {
	    if {[info exists ::syntax($cmd)]} {
		checkCommand $cmd $index $argv $wordstatus $indices
	    } elseif {[lsearch $::knownCommands $cmd] == -1 && \
                    [lsearch $::knownProcs $cmd] == -1} {
                
#                decho "Uncheck: $cmd argc:$argc line:[calcLineNo $index]"
                # Store the unknown commands for later check.
                set ::unknownCommands($cmd) $index
	    }
	}
    }

    # Check unmarked constants against known variables to detect missing $.
    # FIXA: Maybe if the constant is within quotes the warning should
    # be turned off.
    if {[lsearch $constantsDontCheck all] == -1} {
	set i 0
	foreach ws $wordstatus var $argv {
	    if {$ws == 1 && [lsearch $constantsDontCheck $i] == -1} {
		if {[info exists knownVars($var)]} {
		    errorMsg "Found constant \"$var\" which is also a\
                            variable." [lindex $indices $i]
		}
	    }
	    incr i
	}
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
                        statement. This may indicate a brace mismatch." \
                        $closeBraceIx
            }
	}
    }
    # If tryline is non empty, it did not become complete
    if {[string length $tryline] != 0} {
        errorMsg "Could not complete statement." $index

        # Experiment a little to give more info.
        foreach line [list $firstline $tryline] \
                txt [list "the first line." "the script body."] {
            if {[info complete $line\}]} {
                echo "  One close brace would complete $txt"
            }
            if {[info complete $line\"]} {
                echo "  One double quote would complete $txt"
            }
            if {[info complete $line\]]} {
                echo "  One close bracket would complete $txt"
            }
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

    # FIXA, take care of namespace
    set ns [namespace qualifiers $name]
    set name [namespace tail $name]

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
    parseBody $body [lindex $indices 2] knownVars

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
        if {$ni > $i} break
        incr n
    }
    set n
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
    set result
}

# Parse a global script
proc parseScript {script} {
    global knownGlobals unknownCommands knownProcs syntax

    catch {unset unknownCommands}
    array set unknownCommands {}
    array set knownVars {}
    foreach g $knownGlobals {
	set knownVars($g) 6
    }
    set script [buildLineDb $script]
    parseBody $script 0 knownVars

    # Check commands that where unknown when encountered
    foreach cmd [array names unknownCommands] {
        if {![info exists syntax($cmd)] && \
                [lsearch $knownProcs $cmd] == -1} {
            errorMsg "Unknown command \"$cmd\"" $unknownCommands($cmd)
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

    parseScript $script
}

proc usage {} {
    puts $::version
    puts {Usage: syntax.tcl [options] [-s dbfile] scriptfile ...}
    puts { -h        : Show usage.}
    puts { -s dbfile : Include a database file. (More than one is allowed.)}
    puts { -WexprN   : Sets expression warning level.}
    puts {   2 (def) = Warn about any unbraced expression.}
    puts {   1       = Don't warn on single commands. "if [apa] {...}" is ok.}
    puts { -WsubN    : Sets subcommand warning level.}
    puts {   1 (def) = Warn about shortened subcommands.}
    exit
}

# End of procs, global code here

# Locate default syntax database
set defaultDb ""
foreach file [list [file join [pwd] syntaxdb.tcl] \
        [file join $thisDir syntaxdb.tcl]] {
    if {[file exists $file]} {
        set defaultDb $file
        break
    }
}

# Things to run only the first time.
if {![info exists gurka]} {
    set gurka 1
    array set Prefs {
        warnBraceExpr 2
        warnShortSub 1
    }
    if {$argc >= 1} {
        set dbfiles {}
        set files {}
        for {set i 0} {$i < $argc} {incr i} {
            set arg [lindex $argv $i]
            switch -glob -- $arg {
                -h* {
                    usage
                }
                -s {
                    incr i
                    lappend dbfiles [lindex $argv $i]
                }
                -Wexpr* {
                    set Prefs(warnBraceExpr) [string range $arg 6 end]
                }
                -Wsub* {
                    set Prefs(warnShortSub) [string range $arg 5 end]
                }
                -* {
                    puts "Unknown option $arg."
                    usage
                }
                default {
                    lappend files $arg
                }
            }
        }
	if {[llength $dbfiles] == 0} {
            if {$defaultDb == ""} {
                puts "No syntax database file."
                set knownGlobals  $defknownGlobals
                set knownCommands $defknownCommands
                set knownProcs    $defknownProcs
            } else {
                source $defaultDb
            } 
        } else {
	    foreach f $dbfiles {
		source $f
	    }
	}
        foreach f $files {
            set ::currentFile $f
            puts "Checking file $f"
            parseFile $f
        }
        exit
    } else {
        #We have no arguments. Just try to load the syntaxDb.
        if {$defaultDb == ""} {
            puts "No syntax database file."
            set knownGlobals  $defknownGlobals
            set knownCommands $defknownCommands
            set knownProcs    $defknownProcs
        } else {
            source $defaultDb
        }
    }
}
