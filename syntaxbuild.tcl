# This script is intended to be run in a Tcl interpreter to extract
# information for the syntax checker.
#
# $Revision$

# First get some data about the system

set kG [lsort [info globals]]
set kC [lsort [info commands]]
set kP [lsort [info procs]]

# Function to get an option or subcommand list from an error message.
proc getSubCmds {args} {
    catch {eval $args} err
    
    lappend res {option .* must be (.*)$}
    lappend res {option .* should be one of (.*)$}
    lappend res {bad .* must be (.*)$}
    
    foreach re $res {
	if {[regexp $re $err -> apa]} {
	    regsub -all {( or )|(, or )|(, )} $apa " " apa
	    return [lrange $apa 0 end]
	}
    }
    return {}
}

# Create a syntax description for a procedure
proc createSyntax {procName} {
    set args [info args $procName]

    set min 0
    set unlim 0

    if {[lindex $args end] == "args"} {
        set unlim 1
    }
    set i 1
    foreach a $args {
	if {$a != "args"} {
            if {![info default $procName $a dummy]} {
                set min $i
            }
        }
        incr i
    }
    if {$unlim} {
        set result "r $min"
    } elseif {$min == [llength $args]} {
        set result $min
    } else {
        set result [list r $min [llength $args]]
    }
    return $result
}

proc buildDb {ch} {
    global kG kC kP

    set ver [info tclversion]
    
    puts $ch "# Automatically generated syntax database."
    puts $ch "# Based on Tcl version $ver\n"

    puts $ch [list set knownGlobals $kG]
    puts $ch [list set knownCommands $kC]
    puts $ch [list set knownProcs $kP]
    
    # Build a database of options and subcommands
    foreach cmd {info string file image} {
        set subCmd($cmd) [getSubCmds $cmd gurkmeja]
    }
    
    set subCmd(wm) [getSubCmds wm gurkmeja .]
    set subCmd(array) [getSubCmds array gurkmeja apa]
    set subCmd(binary) [getSubCmds binary gurkmeja]
    
    set option(fconfigure) [getSubCmds fconfigure stdin -gurkmeja]
    set option(switch) [getSubCmds switch -gurkmeja]
    set option(lsearch) [getSubCmds lsearch -gurkmeja g g]

    # Define syntax for commands
    
    # If syntax is an integer, just check the number of arguments agains it.
    # r min ?max?  Specify a range for number of arguments
    
    # x Any
    # o Option, i.e anything starting with -
    # p Option+Any
    # s Subcommand
    # e Expression
    # c Code
    # n, v and l all marks variable names. Those arguments will not be
    #   checked against known variables to detect missing $.
    # n The variable does not have to exist, and is set by the command.
    # v The variable must exist. It is not marked as set.
    # l Does not have to exist. It will be marked as known, but not set.

    # Modifiers that apply to some of the above
    # ? Zero or One
    # * Zero or more
    # . One or nothing at all

    # * after x, n, v or l swallows all the rest and must be last
    # s may only have .
    # e and c may not have any modifier

    # The following commands are handled directly be the code:
    # global, upvar, variable, set
    # if, switch, foreach
    # expr, proc, bind, eval

    # Commands with a set number of arguments that are not checked.
    set syntax(break) 0
    set syntax(continue) 0
    set syntax(pwd) 0
    set syntax(close) 1
    set syntax(llength) 1
    set syntax(source) 1
    set syntax(lindex) 2
    set syntax(lrange) 3

    # Commands with a varying number of arguments that are not checked.
    set syntax(list) "r 0"
    set syntax(console) "r 1"
    set syntax(join) "r 1 2"
    set syntax(read) "r 1 2"
    set syntax(split) "r 1 2"
    set syntax(time) "r 1 2"
    set syntax(open) "r 1 3"

    # Commands with code
    set syntax(for) "c e c c"
    set syntax(while) "e c"
    set syntax(catch) "c n?"

    # Commands with subcommands
    # If a syntax for a subcommand is defined, it is used to check the rest
    set syntax(package) "s x*"
    set syntax(clock) "s x*"
    set syntax(info) "s x*"
    set syntax(info\ exists) "l"
    set syntax(wm) "s x x*"
    set syntax(file) "s x x*"
    set syntax(file\ lstat) "x n"
    set syntax(string) "s x x*"
    set syntax(array) "s n x*"
    set syntax(array\ names) "v x?"
    set syntax(file) "s x*"
    set syntax(file\ stat) "x n"
    set syntax(update) "s."
    set syntax(binary) "s x*"
    set syntax(binary\ scan) "x x n n*"

    # General commands
    set syntax(fconfigure) "x o. x. p*"
    set syntax(lsearch) "o? x x"
    set syntax(puts) "o? x x?"
    set syntax(return) "p* x?"
    set syntax(regsub) "o* x x x n"

    set syntax(gets) "x n?"

    set syntax(append) "n x*"
    set syntax(lappend) "n x*"
    set syntax(incr) "v x?"
    set syntax(regexp) "o* x x n*"

    set syntax(scan) "x x n n*"
    set syntax(unset) "l l*"

    # Build syntax info for procs
    foreach apa $kP {
        if {![info exists syntax($apa)]} {
            set syntax($apa) [createSyntax $apa]
        }
    }

    # Output the data

    foreach a {syntax subCmd option} {
        foreach i [lsort [array names $a]] {
            set v [set ${a}($i)]
            if {[llength $v] != 0} {
                if {[lsearch $kC [lindex [split $i] 0]] == -1} {
                    puts stderr "Skipping ${a}($i) since $i is not known."
                } else {
                    puts $ch [list set ${a}($i) $v]
                }
            }
        }
        puts $ch ""
    }
}

proc buildFile {filename} {
    set ch [open $filename w]
    buildDb $ch
    close $ch
}

if {!$tcl_interactive} {
    if {$argc == 0 && $tcl_platform(platform) == "windows"} {
	set argc 1
	set argv [list syntaxdb.tcl]
    }
    if {$argc == 0} {
	buildDb stdout
    } else {
        buildFile [lindex $argv 0]
    }
    exit
}