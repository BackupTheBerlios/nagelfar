source syntaxdb.tcl

#TODO: Handle command options and bind code
#      Handle namespaces and qualified vars

#Arguments used to many procedures:
#lineNo    : Line number of the start of a string or command.
#cmd       : Command
#argv      : List of arguments
#wordstatus: List of status for the words in argv
#lines     : List of lines where every word in argv starts
#knownVars : An array that keeps track of variables known in this scope

proc echo {str} {
    puts $str
    update
}

#Allow syntax information in comments
proc checkComment {str lineNo} {
    set str [string trim $str]

    if {[string match "##syntax *" $str]} {
	set apa [split $str]
	set ::syntax([lindex $apa 1]) [lrange $apa 2 end]
    }
}

##syntax skipWS x x n n
#Skip whitespace
#Afterwards, i points to the first non whitespace char.
proc skipWS {str len iVar lineNoVar} {
    upvar $iVar i $lineNoVar lineNo

    for {} {$i < $len} {incr i} {
	set c [string index $str $i]
	if {$c == "\n"} {
	    incr lineNo
	} elseif {$c == "\\"} {
            #A backslash followed by newline is whitespace
            set c2 [string index $str [expr {$i + 1}]]
            if {$c2 != "\n"} {
                return
            }
            incr i
        } elseif {![string is space $c]} {
            return
        }   
    }
    return
}

##syntax scanWord x x n n
#Scan the string until the end of one word is found.
#When entered, i points to the start of the word.
#When returning, i points to the last char of the word.
proc scanWord {str len iVar lineNoVar} {
    upvar $iVar i $lineNoVar lineNo

    set si $i
    set ei $i
    set c [string index $str $i]
    if {$c == "\{"} {
        set closeChar \}
    } elseif {$c == "\""} {
        set closeChar \"
    } else {
        set closeChar ""
    }

    set escape 0
    for {} {$i < $len} {incr i} {
        set c [string index $str $i]
        if {$c == "\n"} {
            incr lineNo
	}
        if {$c == "\\"} {
            set escape [expr {!$escape}]
        } else {
	    #Take care of an escaped newline. Treat it as space
	    if {$escape && $c == "\n"} {
		incr i -2
		if {[info complete [string range $str $si $i]]} {
		    if {$closeChar != "" && $ei != $i} {
			echo "Extra chars after closing brace or quote.\
				Line $lineNo."
		    }
		    return
		}
		set escape 0
		incr i 2
		continue
	    }
	    if {!$escape} {
		if {$c == $closeChar} {
		    if {[info complete [string range $str $si $i]]} {
			set ei $i
		    }
		} elseif {[string is space $c]} {
		    if {[info complete [string range $str $si $i]]} {
			incr i -1
			if {$closeChar != "" && $ei != $i} {
			    echo "Extra chars after closing brace or quote.\
				    Line $lineNo."
			}
			return
		    }
		}
	    } else {
		set escape 0
	    }
	}
    }

    #Theoretically, no incomplete string should come to this function,
    #but some precaution is never bad.
    if {![info complete [string range $str $si end]]} {
        echo "Error in scanWord: String not complete."
        echo $str
	return -code break
    }
    incr i -1
    if {$closeChar != "" && $ei != $i} {
	echo "Extra chars after closing brace or quote. Line $lineNo."
    }
    return
}

#Obsolete function
proc matchBrace {str len iVar} {
    upvar $iVar i

    set origi $i
    set nb 0
    set escape 0

    for {} {$i < $len} {incr i} {
        set c [string index $str $i]
        switch -- $c \\ {
            set escape [expr {!$escape}]
        } \{ {
            if {$escape} {
                set escape 0 
            } else {
                incr nb
            }
        } \} {
            if {$escape} {
                set escape 0
            } else {
                incr nb -1
                if {$nb == 0} {
                    return
                }
            }
        }
    }
    echo "Error in matchBrace. Reached end of string."
    echo [string range $str $i end]
    return -code break
}

##syntax splitStatement x x n
#Split a statement into words.
#Returns a list of the words, and puts a list with the line number
#for each word in lVar.
proc splitStatement {statement lineNo lVar} {
    upvar $lVar lines
    set lines {}

    set len [string length $statement]
    if {$len == 0} {
	return {}
    }
    set words {}
    set i 0
    skipWS $statement $len i lineNo
    if {[string index $statement $i] == "#"} {
	return {}
    }
    while {$i < $len} {
        set si $i
	lappend lines $lineNo
        scanWord $statement $len i lineNo
        set word [string range $statement $si $i]
        lappend words $word
        incr i
        skipWS $statement $len i lineNo
    }
    return $words
}

#Look for options in a command's arguments. 
#Check them against the list in the option database, if any.
#Returns the number of arguments "used".
proc checkOptions {cmd argv wordstatus lines {startI 0} {max 0} {pair 0}} {
    global option

    set check [info exists option($cmd)]
    set i 0
    set used 0
    set skip 0
    #Since in most cases startI is 0, I believe foreach is faster.
    foreach arg $argv ws $wordstatus lineNo $lines {
	if {$skip} {
	    set skip 0
	    incr used
	    continue 
	}
	if {$i < $startI} {
	    incr i
	    continue
	}
	if {[string index $arg 0] == "-"} {
	    incr used
	    set skip $pair
	    if {$ws != 0  && $check} {
		if {[lsearch $option($cmd) $arg] == -1} {
		    echo "Bad option $arg to $cmd in line $lineNo"
		}
	    }
	    if {$arg == "--"} {
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

##syntax parseVar x x x n n
#Parse a variable name, check for existance
proc parseVar {str len lineNo iVar knownVarsVar} {
    upvar $iVar i $knownVarsVar knownVars
    set si $i
    set c [string index $str $si]
    if {$c == "\{"} {
	incr si
	set ei [string first "\}" $str $si]
	if {$ei == -1} {
	    #This should not happen. FIXA
	}
	set i [expr {$ei + 1}]
	incr ei -1
	set var [string range $str $si $ei]
	set vararr 0
	if {[string index $str $ei] == ")"} {
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
	    #:: is ok.
	    if {$c == ":"} {
		set c [string index $str [expr {$ei + 1}]]
		if {$c == ":"} {
		    incr ei
		    continue
		}
	    }
	    break
	}
	if {[string index $str $ei] == "("} {
	    #Locate the end of the array index
	    set pi $ei
	    while {[set ei [string first ")" $str $ei]] != -1} {
		if {[info complete [string range $str $pi $ei]]} {
		    break
		}
		incr ei
	    }
	    if {$ei == -1} {
		#Something is wrong FIXA
	    }
	    set i [expr {$ei + 1}]
	    incr pi -1
	    set var [string range $str $si $pi]
	    incr pi 2
	    incr ei -1
	    set varindex [string range $str $pi $ei]
	    set vararr 1
	    set varindexconst [parseSubst $varindex $lineNo knownVars]
	} else {
	    set i $ei
	    incr ei -1
	    set var [string range $str $si $ei]
	    set vararr 0
	}
    }

    #By now:
    #var is the variable name
    #vararr is 1 if it is an array
    #varindex is the array index
    #varindexconst is 1 if the array index is a constant

    if {[string match ::* $var]} {
	#Skip qualified names until we handle namespace better. FIXA
	return
    }
    if {![info exists knownVars($var)]} {
	echo "xUnknown variable $var in line $lineNo"
    }
    #If it's a constant array index, maybe it should be checked? FIXA
}

##syntax parseSubst x x n
#Check for substitutions in a word
#Check any variables referenced, and parse any commands within brackets.
#Returns 1 if the string is constant, i.e. no substitutions
#Returns 0 if any substitutions are present
proc parseSubst {str lineNo knownVarsVar} {
    upvar $knownVarsVar knownVars

    #First do a quick check for $ or [
    if {[string first \$ $str] == -1 && [string first \[ $str] == -1} {
	return 1
    }

    set result 1
    set len [string length $str]
    set escape 0
    for {set i 0} {$i < $len} {incr i} {
        set c [string index $str $i]
        if {$c == "\\"} {
            set escape [expr {!$escape}]
        } elseif {!$escape} {
	    if {$c == "\$"} {
		incr i
		parseVar $str $len $lineNo i knownVars
		set result 0
	    } elseif {$c == "\["} {
		set si $i
		for {} {$i < $len} {incr i} {
		    if {[info complete [string range $str $si $i]]} {
			break
		    }
		}
		if {$i == $len} {
		    echo "URGA"
		}
		incr si
		incr i -1
		parseBody [string range $str $si $i] $lineNo knownVars
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
#Parse an expression
proc parseExpr {str lineNo knownVarsVar} {
    upvar $knownVarsVar knownVars
    #Kolla efter variabler och kommandon
    parseSubst $str $lineNo knownVars
}

#A "macro" to print common error message
proc WA {} {
    upvar cmd cmd lineNo lineNo argc argc
    echo "Wrong number of arguments ($argc) to \"$cmd\" in line $lineNo"
}

#Check a command that have a syntax defined in the database
#This is called from parseStatement, and was moved out to be able
#to call it recursively.
proc checkCommand {cmd lineNo argv wordstatus lines {firsti 0}} {
    upvar constantsDontCheck constantsDontCheck knownVars knownVars
    set argc [llength $argv]
    set syn $::syntax($cmd)
    #echo "Checking $cmd against syntax $syn"

    if {[string is integer $syn]} {
	if {$argc != $syn} {
	    WA
	}
	return
    } elseif {[lindex $syn 0] == "r"} {
	if {$argc < [lindex $syn 1]} {
	    WA
	} elseif {[llength $syn] >= 3 && $argc > [lindex $syn 2]} {
	    WA
	}
	return
    }
    
    set i $firsti
    foreach token $::syntax($cmd) {
	set tok [string index $token 0]
	set mod [string index $token 1]
	#Basic checks for modifiers
	switch -- $mod {
	    "" { #No modifier, and out of arguments, is an error
		if {$i >= $argc} {
		    set i -1
		    break
		}
	    }
	    "*" - "." { #No more arguments is ok.
		if {$i >= $argc} {
		    set i $argc
		    break
		}
	    }
	}
	switch -- $tok {
	    x {
		#x* matches anything up to the end.
		if {$mod == "*"} {
		    set i $argc
		    break
		}
		if {$mod != "?" || $i < $argc} {
		    incr i
		}
	    }
	    e { #An expression
		if {$mod != ""} {
		    echo "Modifier \"$mod\" is not supported for \"e\" in syntax for $cmd."
		}
		if {[lindex $wordstatus $i] != 2} {
		    echo "Warning: No braces around expression."
		    echo "  $cmd statement in line [lindex $lines $i]"
		}
		parseExpr [lindex $argv $i] [lindex $lines $i] knownVars
		incr i
	    }
	    c { #A code block
		if {$mod != ""} {
		    echo "Modifier \"$mod\" is not supported for \"c\" in syntax for $cmd."
		}
		if {[lindex $wordstatus 0] != 2} {
		    echo "Warning: No braces around code."
		    echo "  $cmd statement in line [lindex $lines $i]"
		}
		parseBody [lindex $argv $i] [lindex $lines $i] knownVars
		incr i
	    }
	    s { #A subcommand
		if {$mod != ""} {
		    echo "Modifier \"$mod\" is not supported for \"s\" in syntax for $cmd."
		}
		if {[lindex $wordstatus $i] == 0} {
		    echo "Non static subcommand to \"$cmd\" in line [lindex $lines $i]"
		} else {
		    set arg [lindex $argv $i]
		    if {[info exists ::subCmd($cmd)]} {
			if {[lsearch $::subCmd($cmd) $arg] == -1} {
			    echo "Unknown subcommand \"$arg\" to \"$cmd\" in line [lindex $lines $i]"
			}
		    }
		    #Are there any syntax definition for this subcommand?
		    set sub "$cmd $arg"
		    if {[info exists ::syntax($sub)]} {
			checkCommand $sub $lineNo $argv $wordstatus $lines 1
			set i $argc
			break
		    }
		}
		lappend constantsDontCheck $i
		incr i
	    }
	    l -
	    v -
	    n { #A call by name
		if {$mod == "?"} {
		    if {$i >= $argc} {
			set i $argc
			break
		    }
		}
		set ei [expr {$i + 1}]
		if {$mod == "*"} {
		    set ei $argc
		}
		while {$i < $ei} {
		    if {$tok == "v"} {
			#Check the variable
			if {[markVariable [lindex $argv $i] [lindex $wordstatus $i] 1 knownVars]} {
			    echo "yUnknown variable [lindex $argv $i] in line $lineNo"
			}
		    } elseif {$tok == "n"} {
			markVariable [lindex $argv $i] [lindex $wordstatus $i] 0 knownVars
		    }
		    
		    lappend constantsDontCheck $i
		    incr i
		}
	    }
	    o {
		set max 0
		if {$mod != "*"} {
		    set max 1
		}
		set used [checkOptions $cmd $argv $wordstatus $lines $i $max]
		if {$used == 0 && ($mod == "" || $mod == ".")} {
		    echo "Expected an option as argument $i to \"$cmd\"\
			    in line [lindex $lines $i]"
		    return
		}
		incr i $used
	    }
	    gurko {
		#FIXA koll mot databas. Anvand checkOptions?
		while {1} {
		    set arg [lindex $argv $i]
		    if {[string index $arg 0] == "-"} {
			incr i
			if {$mod != "*"} break
		    } else {
			if {$mod == "" || $mod == "."} {
			    echo "Expected an option as argument $i to \"$cmd\" in line [lindex $lines $i]"
			    return
			}
			break
		    }
		}
	    }
	    p {
		set max 0
		if {$mod != "*"} {
		    set max 2
		}
		set used [checkOptions $cmd $argv $wordstatus $lines $i $max 1]
		if {$used == 0 && ($mod == "" || $mod == ".")} {
		    echo "Expected an option as argument $i to \"$cmd\"\
			    in line [lindex $lines $i]"
		    return
		}
		incr i $used
	    }
	    gurkp {
		#FIXA koll mot databas
		while {1} {
		    if {$i >= $argc - 1} {
			if {$mod == ""} {
			    set i -1
			    break
			}
		    }
		    set arg [lindex $argv $i]
		    if {[string index $arg 0] == "-"} {
			incr i 2
			if {$mod != "*"} break
		    } else {
			if {$mod == "" || $mod == "."} {
			    echo "Expected an option as argument $i to \"$cmd\" in line [lindex $lines $i]"
			    return
			}
			break
		    }
		}
	    }
	    default {
		echo "Unsupported token $token in syntax for $cmd"
	    }
	}
    }
    #Have we used up all arguments?
    if {$i != $argc} {
	WA
    }
}

#Mark a variable as known
##syntax markVariable x x x n
proc markVariable {var ws check knownVarsVar} {
    upvar $knownVarsVar knownVars
    
    set varBase $var
    set varArray 0
    set varIndex ""
    set varBaseWs $ws
    set varIndexWs $ws

    #is it an array?
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

    if {$check} {
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
	    set knownVars($varBase) 1
	    if {$varArray && $varIndexWs != 0} {
		set knownVars($var) 1
	    }
	}
    }
}

##syntax parseStatement x x n
#Parse one statement
#This is the "big one"
proc parseStatement {statement lineNo knownVarsVar} {
    upvar $knownVarsVar knownVars
    set words [splitStatement $statement $lineNo lines]
    if {[llength $words] == 0} {return}

    set words2 {}
    set wordstatus {}
    foreach word $words {
        set c [string index $word 0]
        if {$c == "\{"} {
            lappend words2 [string range $word 1 end-1]
            lappend wordstatus 2
        } else {
            if {$c == "\""} {
                set word [string range $word 1 end-1]
            }
	    lappend words2 $word
            lappend wordstatus [parseSubst $word $lineNo knownVars]
        }
    }
    
    #Interpretation of wordstatus:
    # 0 contains substitutions
    # 1 constant
    # 2 enclosed in braces

    #If the command contains substitutions, then we can not determine 
    #which command it is, so we skip it.
    if {[lindex $wordstatus 0] == 0} {
        return
    }
    
    set cmd [lindex $words2 0]
    set argv [lrange $words2 1 end]
    set wordstatus [lrange $wordstatus 1 end]
    set lines [lrange $lines 1 end]
    set argc [llength $argv]
    set anyUnknown [expr !( [join $wordstatus +] +0)]

    #The parsing below can pass information to the constants checker
    #This list primarily consists of args that are supposed to be variable
    #names without a $ in front.
    set constantsDontCheck {}
    
    switch -glob -- $cmd {
	proc {
	    if {$argc != 3} {
		WA
		return
	    }
	    #Skip the proc if any part of it is not constant
	    if {$anyUnknown} {
		echo "Non constant argument to proc \"[lindex $argv 0]\" in line $lineNo. Skipping."
		return
	    }
	    parseProc $argv $lineNo
	    lappend constantsDontCheck all
	}
	.* { #FIXA, kolla kod i ev. -command. Aven widgetkommandon ska kollas.
	     #Kanske i checkOptions ?
	    return
	}
	bind { #FIXA, check the code
	    return
	}
	global {
	    #FIXA, update the globals database
	    foreach var $argv ws $wordstatus {
		if {$ws} {
		    set knownVars($var) 1
		} else {
		    echo "Non constant argument to $cmd in line $lineNo: $var"
		}
	    }
	    lappend constantsDontCheck all
	}
	variable {
	    #FIXA, namespaces?
	    set i 0
	    foreach {var val} $argv {ws1 ws2} $wordstatus {
		if {$ws1} {
		    set knownVars($var) 1
		    lappend constantsDontCheck $i
		} else {
		    echo "Non constant argument to $cmd in line $lineNo: $var"
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
	    if {$argc < 1 || $argc > 2} {
		WA
		return
	    }
	    if {$argc == 1} {
		if {[markVariable [lindex $argv 0] [lindex $wordstatus 0] 1 knownVars]} {
		    echo "yUnknown variable [lindex $argv 0] in line $lineNo"
		}
	    } else {
		markVariable [lindex $argv 0] [lindex $wordstatus 0] 0 knownVars
	    }
	    lappend constantsDontCheck 0
	}
	gurkappend - gurklappend {
	    if {$argc < 1} {
		WA
		return
	    }
	    if {[lindex $wordstatus 0] != 0} {
		set var [lindex $argv 0]
		set knownVars($var) 1
		lappend constantsDontCheck 0
	    }
	}
	gurkincr {
	    if {$argc < 1 || $argc > 2} {
		WA
		return
	    }
	    if {[lindex $wordstatus 0] != 0} {
		set var [lindex $argv 0]
		lappend constantsDontCheck 0
		if {![info exists knownVars($var)]} {
		    echo "zUnknown variable $var in line $lineNo"
		}
	    }
	}
	gurkwhile {
	    if {$argc != 2} {
		WA
		return
	    }
	    if {[lindex $wordstatus 0] == 0} {
		echo "Warning: No braces around condition."
		echo "  While statement in line $lineNo"
	    }
	    parseExpr [lindex $argv 0] [lindex $lines 0] knownVars
	    if {[lindex $wordstatus 1] == 0} {
		echo "Warning: No braces around body."
		echo "  While statement in line $lineNo"
	    }
	    parseBody [lindex $argv 1] [lindex $lines 1] knownVars
	}
	foreach {
	    if {$argc < 3 || ($argc % 2) == 0} {
		WA
		return
	    }
	    for {set i 0} {$i < $argc - 1} {incr i 2} {
		if {[lindex $wordstatus $i] == 0} {
		    echo "Warning: Non constant variable list."
		    echo "  Foreach statement in line [lindex $lines $i]"
		    #FIXA, maybe abort here?
		}
		lappend constantsDontCheck $i
		#FIXA, check for an array
		foreach var [lindex $argv $i] {
		    set knownVars($var) 1
		}
	    }
	    if {[lindex $wordstatus end] == 0} {
		echo "Warning: No braces around body."
		echo "  Foreach statement in line $lineNo"
	    }
	    parseBody [lindex $argv end] [lindex $lines end] knownVars
	}
	gurkfor {
	    if {$argc != 4} {
		WA
		return
	    }
	    if {[lindex $wordstatus 0] != 2} {
		echo "Warning: No braces around init code."
		echo "  For statement in line $lineNo"
	    }
	    parseBody [lindex $argv 0] [lindex $lines 0] knownVars
	    if {[lindex $wordstatus 1] != 2} {
		echo "Warning: No braces around expression."
		echo "  For statement in line $lineNo"
	    }
	    parseExpr [lindex $argv 1] [lindex $lines 1] knownVars
	    if {[lindex $wordstatus 3] == 0} {
		echo "Warning: No braces around body."
		echo "  For statement in line $lineNo"
	    }
	    parseBody [lindex $argv 3] [lindex $lines 3] knownVars
	    if {[lindex $wordstatus 2] != 2} {
		echo "Warning: No braces around next code."
		echo "  For statement in line $lineNo"
	    }
	    parseBody [lindex $argv 2] [lindex $lines 2] knownVars
	}
	if {
	    if {$argc < 2} {
		WA
		return
	    }
	    set state expr
	    set ifsyntax {}
	    foreach arg $argv ws $wordstatus lineNo $lines {
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
			echo "Badly formed if statement in line $lineNo."
			echo "  Found arguments after supposed last body."
			return
		    }
		}
	    }
	    if {$state != "else" && $state != "illegal"} {
		echo "Badly formed if statement in line $lineNo."
		echo "  Missing one body."
		return
	    }
	    #echo "if syntax \"$ifsyntax\""
	    set ::syntax(if) $ifsyntax
	    checkCommand $cmd $lineNo $argv $wordstatus $lines
	}
	gurkif {
	    #FIXA, bygg upp en syntaxstrang, anropa checkCommand
	    if {$argc < 2} {
		WA
		return
	    }
	    set state expr
	    foreach arg $argv ws $wordstatus lineNo $lines {
		switch -- $state {
		    then {
			set state body
			if {[string equal $arg then]} {
			    continue
			}
		    }
		    else {
			set state body
			if {[string equal $arg else]} {
			    continue
			}
			if {[string equal $arg elseif]} {
			    set state expr
			    continue
			}
		    }
		}
		switch -- $state {
		    expr {
			if {$ws != 2} {
			    echo "Warning: No braces around condition."
			    echo "  If statement in line $lineNo"
			}
			parseExpr $arg $lineNo knownVars
			set state then
		    }
		    body {
			if {$ws == 0} {
			    echo "Warning: No braces around body."
			    echo "  If statement in line $lineNo"
			}
			parseBody $arg $lineNo knownVars
			set state else
		    }
		}
	    }
	    if {$state != "else"} {
		echo "Something is wrong." ;# FIXA
		echo "  If statement in line $lineNo"
	    }
	}
	switch {
	    if {$argc < 2} {
		WA
		return
	    }
	    set i [checkOptions $cmd $argv $wordstatus $lines]
	    incr i
	    set left [expr {$argc - $i}]

	    if {$left < 1} {
		WA
		return
	    } elseif {$left == 1} {
		#One block. Split it into a list.

		set arg [lindex $argv $i]
		set ws [lindex $wordstatus $i]
		set line [lindex $lines $i]

		set argv [splitList $arg $line lines]
		if {[llength $argv] % 2 == 1} {
		    echo "Odd number of elements in last argument to switch."
		    echo "  Line $line."
		    return
		}
		set wordstatus {}
		foreach word $argv {
		    lappend wordstatus 1
		}
	    } elseif {$left % 2 == 1} {
		WA
		return
	    } else {
		set argv [lrange $argv $i end]
		set wordstatus [lrange $wordstatus $i end]
		set lines [lrange $lines $i end]
	    }
	    foreach {pat body} $argv {ws1 ws2} $wordstatus {l1 l2} $lines {
		if {[string index $pat 0] == "#"} {
		    echo "Warning: Switch pattern starting with #."
		    echo "  This could be a bad comment."
		}
		if {$ws2 == 0} {
		    echo "Warning: No braces around code."
		    echo "  Switch statement in line $l2"
		}
		parseBody $body $l2 knownVars
	    }
	}
	regexp {
	    if {$argc < 2} {
		WA
		return
	    }
	    set i [checkOptions $cmd $argv $wordstatus $lines]
	    set left [expr {$argc - $i}]

	    if {$left < 2} {
		WA
		return
	    } elseif {$left > 2} {
		#Match-variabler
		incr i 2
		for {} {$i < $argc} {incr i} {
		    if {[lindex $wordstatus $i] == 0} {
			echo "Non constant match variable to regexp. Line $lineNo"
		    } else {
			set knownVars([lindex $argv $i]) 1
			lappend constantsDontCheck $i
		    }
		}
	    }
	}
	default {
	    if {[info exists ::syntax($cmd)]} {
		checkCommand $cmd $lineNo $argv $wordstatus $lines
	    } else {
		#echo "Uncheck: $cmd"
	    }
	}
    }

    #Check unmarked constants against known variables to detect missing $.
    if {[lsearch $constantsDontCheck all] == -1} {
	set i 0
	foreach ws $wordstatus {
	    if {$ws == 1 && [lsearch $constantsDontCheck $i] == -1} {
		set var [lindex $argv $i]
		if {[info exists knownVars($var)]} {
		    echo "Found constant \"$var\" which is also a variable.\
	                    Line [lindex $lines $i]."
		}
	    }
	    incr i
	}
    }
}

##syntax splitScript x x n n
#Split a script into individual statements
proc splitScript {script lineNo sVar lVar} {
    upvar $sVar statements
    upvar $lVar lines
    
    set re \[\n\;\]
    set commentre {^\s*#}

    set statements {}
    set lines {}
    set rest $script
    set tryline ""

    while {[string bytelength $rest] != 0} {
	#Move everything up to the next semicolon, newline or eof to tryline
	if {[regexp -indices $re $rest match]} {
	    set i [lindex $match 0]
	    set splitchar [string index $rest $i]
	    append tryline [string range $rest 0 $i]
	    incr i
	    set rest [string range $rest $i end]
	} else {
	    append tryline $rest
	    set rest ""
	    set splitchar ""
	}
	#If we split at a ; we must check that it really is a statement end
	if {$splitchar == ";"} {
	    #Comment lines don't end with ;
	    if {[regexp $commentre $tryline]} {continue}
	    
	    #Look for \'s before the ;
	    #If there is an odd number of \, the ; is ignored
	    if {[string index $tryline end-1] == "\\"} {
		set i [expr {[string length $tryline] - 2}]
		for {set t $i} {$t >= 0} {incr t -1} {
		    if {[string index $tryline $t] != "\\"} {break}
		}
		if {($i - $t) % 2 == 1} {continue}
	    }
	}
	if {[info complete $tryline]} {
	    if {[regexp $commentre $tryline]} {
		checkComment $tryline $lineNo
	    }
            if {$splitchar == ";"} {
                lappend statements [string range $tryline 0 end-1]
            } else {
                lappend statements $tryline
            }
	    lappend lines $lineNo
	    incr lineNo [regsub -all \n $tryline a dummy]
	    set tryline ""
	}
    }
    #If tryline is non empty, it did not become complete
    if {[string bytelength $tryline] != 0} {
	echo "Error in splitScript"
	echo "Could not complete statement in line $lineNo."
	echo "Starting with: [string range $tryline 0 20]"
	echo "tryline:$tryline:"
	echo "script:$script:"
	echo "rest:$rest:"
    }
}

##syntax parseBody x x n
proc parseBody {body lineNo knownVarsVar} {
    upvar $knownVarsVar knownVars

    splitScript $body $lineNo statements lines

    foreach statement $statements line $lines {
	parseStatement $statement $line knownVars
    }
}

proc parseProc {argv lineNo} {
    global knownProcs numArgs
    
    if {[llength $argv] != 3} {
	echo "Wrong number of arguments to proc in line $lineNo"
	return
    }

    foreach {name args body} $argv {break}
    set n 0
    array set knownVars {}
    foreach a $args {
	if {[llength $a] != 1} {
	    set n -1
	    set knownVars([lindex $a 0]) 1
	    continue
	}
	if {[string equal $a "args"]} {
	    set n -1
	}
	set knownVars($a) 1
    }
    if {$n >= 0} {
	set numargs($name) [llength $args]
    }
    lappend knownProcs $name

#    echo "Note: parsing procedure $name"
    parseBody $body $lineNo knownVars
}

#Parse a global script
proc parseScript {script} {
    global knownGlobals

    array set knownVars {}
    foreach g $knownGlobals {
	set knownVars($g) 1
    }
    
    parseBody $script 1 knownVars
}

proc parseFile {filename} {
    set ch [open $filename]
    set script [read $ch]
    close $ch

    parseScript $script
}

proc parseFile2 {filename} {
    set ch [open $filename]
    set script [read $ch]
    close $ch

    splitScript $script 1 ::ss ::ls
}

if {![info exists gurka]} {
    set gurka 1
    if {$argc >= 1} {
	parseFile [lindex $argv 0]
    }
}

catch {console show ; console eval {focus .console}}
set test {apa bepa {apa bepa} [hej hopp] "as[hej gupp]g$w"}
#parseFile eon.tcl


proc tt1 {} {
    global testargv testwordstatus testlines teststartI

    set i 0
    foreach arg $testargv ws $testwordstatus lineNo $testlines {
	if {$i < $teststartI} {
	    incr i
	    continue
	}
	set apa [concat $arg $ws $lineNo]
    }
}

proc tt2 {} {
    global testargv testwordstatus testlines teststartI

    set i $teststartI
    set len [llength $testargv]

    while {$i < $len} {
	set arg [lindex $testargv $i]
	set ws [lindex $testwordstatus $i]
	set lineNo [lindex $testlines $i]

	set apa [concat $arg $ws $lineNo]
	incr i
    }
}

proc testbl {n} {
    for {set i 0} {$i < $n} {incr i} {
	lappend res gurka
    }
    return $res
}


proc testit {v} {
    global testargv testwordstatus testlines teststartI

    set n 10
    set testargv [testbl $n]
    set testwordstatus [testbl $n]
    set testlines [testbl $n]

    foreach teststartI {0 5 9} {
	puts I=$teststartI
	puts [time tt1 $v]
	puts [time tt2 $v]
    }
}

##syntax splitList x x n
proc splitList {str lineNo lVar} {
    upvar $lVar lines

    #Make a copy to perform list operations on
    set lstr [string range $str 0 end]

    set lines {}
    if {[catch {set n [llength $lstr]}]} {
	echo "Bad list in line $lineNo"
	return {}
    }

    #Parse the string to get line numbers for each element
    set escape 0
    set level 0
    set len [string length $str]
    set state ws

    for {set i 0} {$i < $len} {incr i} {
	set c [string index $str $i]
	if {$c == "\n"} {
	    set escape 0
	    incr lineNo
	}
	if {$c == "\\"} {
	    set escape [expr {!$escape}]
	}
	switch -- $state {
	    ws { #Whitespace
		if {!$escape && [string is space $c]} continue
		lappend lines $lineNo
		if {$c == "\{"} {
		    set level 1
		    set state brace
		} elseif {$c == "\""} {
		    set state quote
		    set level 1
		} else {
		    set state word
		}
	    }
	    word {
		if {!$escape && [string is space $c]} {
		    set state ws
		    continue
		}
	    }
	    quote {
		if {!$escape && $c == "\""} {
		    set state ws
		    continue
		}
	    }
	    brace {
		if {!$escape && $c == "\{"} {
		    incr level
		} elseif {!$escape && $c == "\}"} {
		    incr level -1
		}
		if {$level <= 0} {
		    set state ws
		}
	    }
	}
    }

    if {[llength $lines] != $n} {
	echo "Length mismatch in splitList. Line $lineNo"
    }
    return $lstr
}
