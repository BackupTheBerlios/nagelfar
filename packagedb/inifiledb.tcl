# This syntax definition for the inifile package in TclLib
# package require inifile 0.2.3
# Contributed by Ruediger Haertel

set ::syntax(ini::close) 1
set ::syntax(ini::comment) {r 3}
set ::syntax(ini::commentchar) {r 0 1}
set ::syntax(ini::commit) 1
set ::syntax(ini::delete) {r 2 3}
set ::syntax(ini::exists) {r 2 3}
set ::syntax(ini::filename) 1
set ::syntax(ini::get) 2
set ::syntax(ini::keys) 2
set ::syntax(ini::open) {r 1 2}
set ::syntax(ini::revert) 1
set ::syntax(ini::sections) 1
set ::syntax(ini::set) 4
set ::syntax(ini::value) {r 3 4}
