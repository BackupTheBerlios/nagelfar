USAGE

This tool is currently a pure command line tool.
It is my plan to add a GUI to it one day.

Typical usage:
syntax.tcl <tcl-file>

For a usage summary:
syntax.tcl -h

Example of how to create a syntax database for your environment:
tclsh syntaxbuild.tcl mysyntaxdb.tcl
Then use it:
syntax.tcl -s mysyntaxdb.tcl <tcl-file>


FEEDBACK

Bugs, suggestions, beer, cash and teddybears can be sent to:
peter.spjuth@space.se


SOME INFO

Note: Messages may not come in line order since a file is not parsed
in a start-to-end fashion, and things are reported when they are seen.
Specially, unknown commands are not reported until the end when
it can be checked if such proc was defined later.


Explanations of some error messages.

"Found constant "x" which is also a variable."

  This is the error for detecting missing $.
  This is also a common source for false errors. Typically with commands
  that use call-by-name, and when using simple variable names.
  For example, this will give such a message: $w configure -anchor w

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
Handle multiple files. Remember variables.
Everything marked FIXA
Give this program a silly name. (maybe Psyche, Peter's SYntax CHEcker)
Optimise. Always optimise.
Tidy up messages. Tidy up code structure. Things are getting messy.
