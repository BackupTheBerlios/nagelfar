# This script is intended to be run in a Tcl interpreter to extract
# information for the syntax checker.
#
# This file contains hardcoded syntax info for many commands that it
# adds to the resulting syntax database, plus it tries to extract info
# from the interpreter about things like subcommands.
#
# $Revision$

# Autoload stuff to have them available
catch {parray}
foreach gurkmeja [array names auto_index] {
    if {[info procs $gurkmeja] == ""} {
        eval $auto_index($gurkmeja)
    }
}
unset gurkmeja


# First get some data about the system

set ::kG [lsort [info globals]]
set ::kC [lsort [info commands]]
set ::kP [lsort [info procs]]

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

# Build a syntax database and write it to a channel
proc buildDb {ch} {
    set patch [info patchlevel]
    set ver   [package present Tcl]

    puts $ch "# Automatically generated syntax database."
    set useTk [expr {![catch {package present Tk}]}]
    if {!$useTk} {
        puts $ch "# Based on Tcl version $patch\n"
    } else {
        puts $ch "# Based on Tcl/Tk version $patch\n"
    }
    puts $ch [list set ::knownGlobals $::kG]
    puts $ch [list set ::knownCommands $::kC]
    puts $ch [list set ::knownProcs $::kP]

    # Below is the hardcoded syntax for many core commands.
    # It is defined using the "language" below.
    # TODO: Add all core commands.

    # An entry should be a valid list of tokens as described below.

    # If the first token ends with : it means that there are different
    # syntax descriptions for different number of arguments.
    # Any token ending with : starts a syntax for the number of arguments
    # that the number preceding it says. A lone : starts the default syntax.
    # Example "1: x 2: n n : e x*"

    # If a token is an integer, just check the number of arguments against it.
    # r min ?max?  Specify a range for number of arguments

    # x Any
    # o Option, i.e anything starting with -
    # p Option+Any (p as in option Pair)
    # s Subcommand
    # e Expression
    # E Expression that should be in braces
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

    # If a syntax for a subcommand is defined, it is used to check the rest


    # Syntax for Tcl core commands

    set syntax(after)           "r 1"
    set syntax(append)          "n x*"
    set syntax(array)           "s n x*"
    set syntax(array\ names)    "v x?"
    set syntax(binary)          "s x*"
    set syntax(binary\ scan)    "x x n n*"
    set syntax(break)            0
    set syntax(catch)           "c n?"
    set syntax(cd)              "r 0 1"
    set syntax(close)            1
    set syntax(concat)          "r 0"
    set syntax(continue)         0
    set syntax(clock)           "s x*"
    set syntax(encoding)        "s x*"
    set syntax(eof)              1
    set syntax(error)           "r 1 3"
    # "eval" is handled specially
    set syntax(exec)            "o* x x*"
    set syntax(exit)            "r 0 1"
    # "expr" is handled specially
    set syntax(fblocked)         1
    set syntax(fconfigure)      "x o. x. p*"
    set syntax(fcopy)           "x x p*"
    set syntax(file)            "s x*"
    set syntax(file\ lstat)     "x n"
    set syntax(file\ stat)      "x n"
    set syntax(fileevent)       "x x x?"
    set syntax(flush)            1
    set syntax(for)             "c E c c"
    # "foreach" is handled specially
    set syntax(format)          "r 1"
    set syntax(gets)            "x n?"
    set syntax(glob)            "o* x x*"
    # "global" is handled specially
    # "if" is handled specially
    set syntax(incr)            "v x?"
    set syntax(info)            "s x*"
    set syntax(info\ exists)    "l"
    set syntax(info\ default)   "x x n"
    # "interp" is handled specially
    set syntax(interp)          "s x*"
    set syntax(join)            "r 1 2"
    set syntax(lappend)         "n x*"
    set syntax(lindex)           2
    set syntax(linsert)         "r 3"
    set syntax(list)            "r 0"
    set syntax(llength)          1
    set syntax(load)            "r 1 3"
    set syntax(lrange)           3
    set syntax(lreplace)        "r 3"
    if {[catch {lsearch -all -glob apa bepa}]} {
        set syntax(lsearch)     "o? x x"  ;# Pre 8.4
    } else {
        set syntax(lsearch)     "o* x x"
    }
    set syntax(lsort)           "o* x"
    # "namespace" is handled specially
    set syntax(namespace)       "s x*"
    set syntax(open)            "r 1 3"
    set syntax(package)         "s x*"
    set syntax(pid)             "r 0 1"
    # "proc" is handled specially
    set syntax(puts)            "1: x : o? x x?"
    set syntax(pwd)              0
    set syntax(read)            "r 1 2"
    set syntax(regexp)          "o* x x n*"
    set syntax(regsub)          "o* x x x n"
    set syntax(rename)           2   ;# Maybe treat rename specially?
    set syntax(return)          "p* x?"
    set syntax(scan)            "x x n*"
    set syntax(seek)            "r 2 3"
    set syntax(set)             "1: v : n x"
    set syntax(socket)          "r 2"
    set syntax(source)           1
    set syntax(split)           "r 1 2"
    set syntax(string)          "s x x*"
    set syntax(string\ is)      "x o* x"
    set syntax(subst)           "o* x"
    # "switch" is handled specially
    set syntax(tell)             1
    set syntax(time)            "r 1 2"
    # "variable" is handled specially
    set syntax(vwait)           "n"
    set syntax(while)           "E c"
    set syntax(trace)           "s x x*"
    set syntax(trace\ variable) "n x x"
    set syntax(trace\ vinfo)    "l"
    set syntax(trace\ vdelete)  "v x*"
    set syntax(unset)           "l l*"
    set syntax(update)          "s."
    # "uplevel" is handled specially
    # "upvar" is handled specially

    # Some special Tcl commands
    set syntax(dde)             "o? s x*"
    set syntax(history)         "s x*"
    set syntax(parray)          "v x?"

    # Syntax for Tk commands

    if {$useTk} {
        # "bind" is handled specially
        set syntax(console)  "r 1"
        set syntax(image)    "s x*"
	set syntax(winfo)    "s x x*"
        set syntax(wm)       "s x x*"
    }

    # Build a database of options and subcommands

    # subCmd(cmd) contains a list of all allowed subcommands

    # Get subcommands for commands that can't use the standard loop below
    set subCmd(wm) [getSubCmds wm gurkmeja .]

    # Get subcommands for any commands defining "s"
    foreach cmd [array names syntax] {
        if {[info exists subCmd($cmd)]} continue
        set syn $syntax($cmd)
        set oi [lsearch -glob $syn "s*"]
        if {$oi >= 0} {
            set syn [lreplace $syn $oi $oi gurkmeja]
            set opts [eval getSubCmds $cmd $syn]
            if {[llength $opts] > 0} {
                set subCmd($cmd) $opts
                #puts "AutoSub: $cmd $subCmd($cmd)"
            } else {
                #puts "Failed AutoSub: $cmd"
            }
        }
    }

    # option(cmd) contains a list of all allowed options
    # option(cmd subcmd) defines options for subcommands

    # Get options for commands that can't use the standard loop below.
    set option(switch)     [getSubCmds switch -gurkmeja]
    set option(fconfigure) [getSubCmds fconfigure stdin -gurkmeja]
    set option(fcopy)      [getSubCmds fcopy stdin stdout -gurkmeja x]

    # Get options for any commands defining "o*" or "p*"
    foreach cmd [array names syntax] {
        if {[info exists option($cmd)]} continue
        set syn $syntax($cmd)
        set oi [lsearch -glob $syn "o*"]
        if {$oi >= 0} {
            set syn [lreplace $syn $oi $oi -gurkmeja]
        }
        set pi [lsearch -glob $syn "p*"]
        if {$pi >= 0} {
            set syn [lreplace $syn $pi $pi -gurkmeja apa]
        }
        if {$oi >= 0 || $pi >= 0} {
            set opts [eval getSubCmds $cmd $syn]
            if {[llength $opts] > 0} {
                set option($cmd) $opts
                #puts "Autoopt: $cmd $option($cmd)"
            }
        }
    }

    # The default for options is not to take a value unless 'p' is
    # used in the syntax definition.
    # If option(cmd opt) is set, the option is followed by a value.
    # The value of option(cmd opt) may be any of the syntax chars cnvl
    # and will be used to check the option.
    set option(lsort\ -index) 1
    set option(lsort\ -command) 1
    set option(string\ is\ -failindex) n

    # Build syntax info for procs
    foreach apa $::kP {
        if {![info exists syntax($apa)]} {
            set syntax($apa) [createSyntax $apa]
        }
    }

    # Output the data

    foreach a {syntax subCmd option} {
        foreach i [lsort [array names $a]] {
            set v [set ${a}($i)]
            if {[llength $v] != 0} {
                if {[lsearch $::kC [lindex [split $i] 0]] == -1} {
                    puts stderr "Skipping ${a}($i) since $i is not known."
                } else {
                    puts $ch [list set ::${a}($i) $v]
                }
            }
        }
        puts $ch ""
    }
}

# Build a syntax database and write it to a file
proc buildFile {filename} {
    set ch [open $filename w]
    buildDb $ch
    close $ch
}

# This file can be sourced into an interactive interpreter.
# source syntaxbuild.tcl
# buildFile <filename>

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
