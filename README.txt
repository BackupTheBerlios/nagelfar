USAGE

This tool is both a command line tool and a GUI tool.

Typical usage:
syntax.tcl <tcl-file>

For a usage summary:
syntax.tcl -h

Multiple files can be checked in one command. In that case the tool
will remember procedures from previous files when checking a file.


GUI

If you start it without arguments or with -gui, you get GUI mode,
provided that Tk can be found. (I.e., it is run in wish or in Tcl8.4
where Tk is loadable)

It is recommended to use Tk8.4 and TkDnd to get full effect.

The GUI lists database files and lets you select one or more to use.
Stay with one though since I have not documented how multiple may be used.

The other list shows files to check. With multiple files all are checked
in the same way as with multiple files on a command line.

By doubleclicking on an error the file and line is viewed in a simple
editor. You can edit and save the file.


SYNTAX DATABASES

The tool uses a file defining what commands are available and how they
are used. This file is typically called syntaxdb.tcl, and is just a Tcl
file defining variables.

Any file called syntaxdb*.tcl in your current directory or in the
directory where syntax.tcl is located are detected and possibly used as
default database unless you specify one on the command line.
The search order for default database is:
 syntaxdb.tcl in current directory
 syntaxdb*.tcl in current directory (if more than one it is unspecified which)
 syntaxdb.tcl where syntax.tcl is located
 syntaxdb*.tcl where syntax.tcl is located

A syntax database is created by syntaxbuild.tcl which makes
it possible to create customized databases for the interpreter
where your script will run.

For example, if you want to create a database for Tcl8.2:

tclsh82 syntaxbuild.tcl syntaxdb82.tcl

Then use it:
syntax.tcl -s syntaxdb82.tcl <tcl-file>


FEEDBACK

Bugs, suggestions, beer, cash and teddybears can be sent to:
peter.spjuth@space.se


SOME INFO

A common source of false warnings have to do with call-by-name.
The syntax checker can be told about procedures using call-by-name
using inline comments or separate info files. See syntax.syntax and
tests/test.syntax for examples.


Explanations of some error messages.

"Found constant "x" which is also a variable."

  This is the error for detecting missing $.
  This is also a common source for false errors. Typically with commands
  that use call-by-name, and when using simple variable names.
  For example, this will give such a message: $w configure -anchor w
  If the constant is within quotes no warning is issued so in the example
  above you can get rid of it by doing -anchor "w".

"Could not complete statement."

  A valid end of this statement could not be found. This means that
  a brace, quote or bracket is missing.
  The message may be followed by extra info that can help figure out
  what happened.

"Close brace not aligned with line <l> (<i1> <i2>)"

  It is assumed that a close brace is indented equally to the line
  where its corresponding open brace is (line <l>).
  The open brace's indent level is <i1> and the close brace's <i2>.
  This error may indicate a brace mismatch, an indentation slip
  or just that you have a different indentation style.

"Found non indented close brace that did not end statement. This may
 indicate a brace mismatch."

  A close brace that is not indented should normally end the
  preceeding statement. See also above.

"Close brace first in statement."

  A close brace was seen where a command should start.
  Typically a brace mismatch.


TODO

Handle widgets -command options, bind code and other callbacks
Handle e.g. -textvariable
Handle namespaces and qualified vars
Everything marked FIXA
Give this program a silly name. (maybe Psyche, Peter's SYntax CHEcker)
Optimise. Always optimise.
Tidy up messages. Tidy up code structure. Things are getting messy.

Handle when some options take a value (p) and some don't (o).
