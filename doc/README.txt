INSTALLATION

Unpack the distribution whereever you want it and add a link
to nagelfar.tcl from any directory in your path.


REQUIREMENTS

Nagelfar requires Tcl 8.4.
If you do not have 8.4, you can run Nagelfar as a Starkit
using a 8.4 based Tclkit. http://wiki.tcl.tk/tclkit


USAGE

This tool is both a command line tool and a GUI tool.

Typical usage:
nagelfar.tcl <tcl-file>

For a usage summary:
nagelfar.tcl -h

Multiple files can be checked in one command. In that case the tool
will remember procedures from previous files when checking a file.


GUI

If you start it without arguments or with -gui, you get GUI mode,
provided that Tk can be found.

Nagelfar supports drag&drop if TkDnd is available.

The GUI lists database files and lets you select one to use.

The other list shows files to check. With multiple files all are checked
in the same way as with multiple files on a command line.

By doubleclicking on an error the file and line is viewed in a simple
editor. You can edit and save the file.


FEEDBACK

Bugs and suggestions can be sent to:
peter.spjuth@space.se
(I also accept beer and teddybears)


SOME INFO

A common source of false warnings have to do with call-by-name.
The syntax checker can be told about procedures using call-by-name
using inline comments or separate info files. See nagelfar.syntax and
tests/test.syntax for examples.


TODO

The database in syntaxbuild is far from complete when it comes to subcommands.
Handle widgets -command options, bind code and other callbacks
Handle e.g. -textvariable
Handle namespaces and qualified vars better
Everything marked FIXA
Tidy up code structure. Things are getting messy.
A standardized way to handle databases for packages, and loading
them when package require is seen.
Handle namespace import if the namespace is known e.g. from a package db.
Maybe places where a constant list is expected (e.g. foreach {a b} ...)
should be able to recognise [list a b] as a constant list.

Recognise the idiom [list cmd arg arg] as code.
Recognise the idiom [set $var] for double dereferencing.
Option to enforce switch --.
Option to enforce not using "then".

Make a GUI to help working with the database builder.  It should
be possible to add packages that should be included in the db.

Recognise the new {expand} syntax introduced in 8.5.


BUGS

The close brace alignment check should match against the line
with the opening brace, not the start of the command:
    cmd xx yy \
        apa {
            hejsan
        }
Line  4: Close brace not aligned with line 1 (4 8)
