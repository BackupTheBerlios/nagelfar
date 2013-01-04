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
    set ::Nagelfar(pluginEarlyExpr) [expr {[$pi eval info proc earlyExpr] ne ""}]
    set ::Nagelfar(pluginLateExpr) [expr {[$pi eval info proc lateExpr] ne ""}]

    return $pi
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

# This is called to let a plugin react to an expression, pre-substitution
proc pluginHandleEarlyExpr {expName knownVarsName index} {
    upvar 1 $expName exp $knownVarsName knownVars
    if {!$::Nagelfar(pluginEarlyExpr)} return

    set x [$::Nagelfar(pluginInterp) eval [list earlyExpr $exp]]
    if {[catch {llength $x}] || ([llength $x] % 2) != 0} {
        errorMsg E "Plugin $::Nagelfar(plugin) returned malformed list from earlyExpr" $index
        return
    }

    foreach {cmd value} $x {
        switch $cmd {
            replace {
                set exp $value
            }
            comment {
                foreach line [split $value \n] {
                    checkComment $line $index knownVars
                }
            }
            error {
                set severity E
                regexp {^(E|N|W) (.*)$} $value -> severity value
                errorMsg $severity $value $index
            }
        }
    }
}

# This is called to let a plugin react to an expression, post-substitution
proc pluginHandleLateExpr {expName knownVarsName index} {
    upvar 1 $expName exp $knownVarsName knownVars
    if {!$::Nagelfar(pluginLateExpr)} return

    set x [$::Nagelfar(pluginInterp) eval [list lateExpr $exp]]
    if {[catch {llength $x}] || ([llength $x] % 2) != 0} {
        errorMsg E "Plugin $::Nagelfar(plugin) returned malformed list from lateExpr" $index
        return
    }

    foreach {cmd value} $x {
        switch $cmd {
            replace {
                # A replacement expression must not have commands in it
                if {[string first "\[" $value] == -1} {
                    set exp $value
                } else {
                    errorMsg E "Plugin $::Nagelfar(plugin) returned malformed replacement from lateExpr" $index
                }
            }
            comment {
                foreach line [split $value \n] {
                    checkComment $line $index knownVars
                }
            }
            error {
                set severity E
                regexp {^(E|N|W) (.*)$} $value -> severity value
                errorMsg $severity $value $index
            }
        }
    }
}
