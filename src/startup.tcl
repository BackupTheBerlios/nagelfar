#----------------------------------------------------------------------
#  Nagelfar, a syntax checker for Tcl.
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
    set ::Nagelfar(withCtext) 0

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
            -instrument {
                set ::Nagelfar(instrument) 1
                # Put checks down as much as possible
                array set ::Prefs {
                    warnBraceExpr 0
                    warnShortSub 0
                    strictAppend 0
                    forceElse 0
                    noVar 1
                    severity E
                }
            }
            -markup {
                incr i
                instrumentMarkup [lindex $argv $i]
                exit
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
            -strictappend {
                set ::Prefs(strictAppend) 1
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
        # use ctext if available
        if {![catch {package require ctext}]} {
            if {![catch {package require ctext_tcl}]} {
                if {[info procs ctext::setHighlightTcl] ne ""} {
                    set ::Nagelfar(withCtext) 1
                    proc ctext::update {} {::update}
                }
            }
        }

        set ::Nagelfar(gui) 1
        makeWin
        vwait forever
        exit
    }

    doCheck
    #_dumplogme
    exit
}
