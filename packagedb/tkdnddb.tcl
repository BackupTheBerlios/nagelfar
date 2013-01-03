# This syntax definition is for the TkDnd package
# package require tkdnd 2.4

set ::syntax(dnd) "s x x*"
set ::subCmd(dnd) "bindtarget cleartarget bindsource clearsource drag"
set ::syntax(dnd\ bindtarget) "r 1 5"
set ::syntax(dnd\ cleartarget) x
set ::syntax(dnd\ bindsource) "r 1 4"
set ::syntax(dnd\ clearsource) x
set ::syntax(dnd\ drag) x

lappend ::knownPackages tkdnd
