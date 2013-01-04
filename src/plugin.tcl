#----------------------------------------------------------------------
#  Nagelfar, a syntax checker for Tcl.
#  Copyright (c) 2013, Peter Spjuth
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
# plugin.tcl
#----------------------------------------------------------------------

proc PluginSearchPath {} {
    set dirs [list . ./plugins]
    lappend dirs [file join $::thisDir .. ..]
    lappend dirs [file join $::thisDir .. .. plugins]
    lappend dirs [file join $::thisDir .. plugins]
    return $dirs
}

# Locate plugin source
proc LocatePlugin {plugin} {
    set src ""
    set dirs [PluginSearchPath]

    foreach dir $dirs {
        set dir [file normalize $dir]
        set files {}
        lappend files [file join $dir $plugin]
        lappend files [file join $dir $plugin.tcl]
        foreach file $files {
            if {![file exists   $file]} continue
            if {![file isfile   $file]} continue
            if {![file readable $file]} continue
            set ch [open $file r]
            set data [read $ch 20]
            close $ch
            if {[string match "##Nagelfar Plugin*" $data]} {
                set src $file
                break
            }
        }
        if {$src ne ""} break
    }
    return $src
}

proc createPluginInterp {plugin} {
    set src [LocatePlugin $plugin]

    if {$src eq ""} {
        return ""
    }

    # Create interpreter
    set pi [interp create -safe]

    # Load source
    $pi invokehidden -global source $src
    $pi eval [list set ::WhoAmI [file rootname [file tail $src]]]
    interp share {} stdout $pi

    # Expose needed commands
    #interp expose $pi fconfigure ;# ??
    interp hide $pi close

    # Set global variables
    set ::Nagelfar(pluginStatementRaw) [expr {[$pi eval info proc statementRaw] ne ""}]
    set ::Nagelfar(pluginStatementWords) [expr {[$pi eval info proc statementWords] ne ""}]
    set ::Nagelfar(pluginEarlyExpr) [expr {[$pi eval info proc earlyExpr] ne ""}]
    set ::Nagelfar(pluginLateExpr) [expr {[$pi eval info proc lateExpr] ne ""}]

    return $pi
}

proc initPlugin {} {
    set ::Nagelfar(pluginStatementRaw) 0
    set ::Nagelfar(pluginStatementWords) 0
    set ::Nagelfar(pluginEarlyExpr) 0
    set ::Nagelfar(pluginLateExpr) 0
    set ::Nagelfar(pluginInterp) ""

    if {$::Nagelfar(plugin) ne ""} {
        set pinterp [createPluginInterp $::Nagelfar(plugin)]
        if {$pinterp eq ""} {
            puts "Bad plugin: $::Nagelfar(plugin)"
            printPlugins
            exit 1
        }
        set ::Nagelfar(pluginInterp) $pinterp
    }
}

proc finalizePlugin {} {
    if {$::Nagelfar(pluginInterp) ne ""} {
        set pi $::Nagelfar(pluginInterp)
        if {[$pi eval info proc finalizePlugin] ne ""} {
            set x [$pi eval finalizePlugin]
            if {[catch {llength $x}] || ([llength $x] % 2) != 0} {
                errorMsg E "Plugin $::Nagelfar(plugin) returned malformed list from finalizePlugin" 0
            } else {
                foreach {cmd value} $x {
                    switch $cmd {
                        error   { errorMsg E $value 0 }
                        warning { errorMsg W $value 0 }
                        note    { errorMsg N $value 0 }
                        default {
                            errorMsg E "Plugin $::Nagelfar(plugin) returned bad keyword '$cmd' from finalizePlugin" 0
                        }
                    }
                }
            }
        }

        interp delete $::Nagelfar(pluginInterp)
    }

    set ::Nagelfar(pluginStatementRaw) 0
    set ::Nagelfar(pluginStatementWords) 0
    set ::Nagelfar(pluginEarlyExpr) 0
    set ::Nagelfar(pluginLateExpr) 0
    set ::Nagelfar(pluginInterp) ""
}

proc printPlugin {plugin} {
    set src [LocatePlugin $plugin]
    if {$src eq ""} {
        printPlugins
        return
    }
    set ch [open $src]
    puts -nonewline [read $ch]
    close $ch
}

proc listPlugins {} {
    set dirs [PluginSearchPath]

    foreach dir $dirs {
        set dir [file normalize $dir]
        set files [glob -nocomplain [file join $dir *.tcl]]
        foreach file $files {
            set file [file normalize $file]
            if {[info exists done($file)]} continue
            if {![file exists $file]} continue
            if {![file isfile $file]} continue
            if {![file readable $file]} continue

            set done($file) 1
            set ch [open $file r]
            set data [read $ch 200]
            close $ch
            if {[regexp {^\#\#Nagelfar Plugin :(.*?)(\n|$)} $data -> descr]} {
                set result([file rootname [file tail $file]]) $descr
            }
        }
    }
    set resultSort {}
    foreach elem [lsort -dictionary [array names result]] {
        lappend resultSort $elem $result($elem)
    }
    return $resultSort
}

proc printPlugins {} {
    set plugins [listPlugins]
    if {[llength $plugins] == 0} {
        puts "No plugins found."
        return
    }
    puts "Available plugins:"
    foreach {plugin descr} $plugins {
        puts "Plugin \"$plugin\" : $descr"
    }
}

# Generic handler to call plugin,
proc PluginHandle {what indata outdataName knownVarsName index} {
    upvar 1 $outdataName outdata $knownVarsName knownVars

    set outdata $indata
    set info [list namespace [currentNamespace] caller [currentProc]]

    set x [$::Nagelfar(pluginInterp) eval [list $what $indata $info]]

    if {[catch {llength $x}] || ([llength $x] % 2) != 0} {
        errorMsg E "Plugin $::Nagelfar(plugin) returned malformed list from $what" $index
        return
    }

    foreach {cmd value} $x {
        switch $cmd {
            replace {
                set outdata $value
            }
            comment {
                foreach line [split $value \n] {
                    checkComment $line $index knownVars
                }
            }
            error   { errorMsg E $value $index }
            warning { errorMsg W $value $index }
            note    { errorMsg N $value $index }
            default {
                errorMsg E "Plugin $::Nagelfar(plugin) returned bad keyword '$cmd' from $what" $index
            }
        }
    }
}

# This is called to let a plugin react to a statement, pre-substitution
proc pluginHandleStatementRaw {stmtName knownVarsName index} {
    upvar 1 $stmtName stmt $knownVarsName knownVars
    if {!$::Nagelfar(pluginStatementRaw)} return

    PluginHandle statementRaw $stmt outdata knownVars $index
    set stmt $outdata
}

# This is called to let a plugin react to a statement, pre-substitution
proc pluginHandleStatementWords {wordsName knownVarsName index} {
    upvar 1 $wordsName words $knownVarsName knownVars
    if {!$::Nagelfar(pluginStatementWords)} return

    PluginHandle statementWords $words outdata knownVars $index
    # A replacement must be a list
    if {[string is list $outdata]} {
        set words $outdata
    } else {
        errorMsg E "Plugin $::Nagelfar(plugin) returned malformed replacement from statementWords" $index
    }
}

# This is called to let a plugin react to an expression, pre-substitution
proc pluginHandleEarlyExpr {expName knownVarsName index} {
    upvar 1 $expName exp $knownVarsName knownVars
    if {!$::Nagelfar(pluginEarlyExpr)} return

    PluginHandle earlyExpr $exp outdata knownVars $index
    set exp $outdata
}

# This is called to let a plugin react to an expression, post-substitution
proc pluginHandleLateExpr {expName knownVarsName index} {
    upvar 1 $expName exp $knownVarsName knownVars
    if {!$::Nagelfar(pluginLateExpr)} return

    PluginHandle lateExpr $exp outdata knownVars $index

    # A replacement expression must not have commands in it
    if {[string first "\[" $outdata] == -1} {
        set exp $outdata
    } else {
        errorMsg E "Plugin $::Nagelfar(plugin) returned malformed replacement from lateExpr" $index
    }
}
