# This syntax definition is for the registry package
# package require registry

set ::syntax(registry) "s x*"
set ::subCmd(registry) "get set delete"
set ::syntax(registry\ get) "x x"
set ::syntax(registry\ set) "x x x"
set ::syntax(registry\ delete) "x"

lappend ::knownPackages registry
