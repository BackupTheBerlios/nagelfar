# Automatically generated syntax database.
# Generated with syntaxbuild $Revision$
# Based on Tcl/Tk version 8.4.4

set ::knownGlobals {argc argv argv0 auto_index auto_oldpath auto_path env errorCode errorInfo tcl_interactive tcl_libPath tcl_library tcl_nonwordchars tcl_patchLevel tcl_pkgPath tcl_platform tcl_rcFileName tcl_version tcl_wordchars tk_library tk_patchLevel tk_strictMotif tk_version}
set ::knownCommands {. after append array auto_execok auto_import auto_load auto_load_index auto_mkindex auto_mkindex_old auto_qualify auto_reset bell binary bind bindtags break button canvas case catch cd checkbutton clipboard clock close concat continue destroy encoding entry eof error eval event exec exit expr fblocked fconfigure fcopy file fileevent flush focus font for foreach format frame gets glob global grab grid history if image incr info interp join label labelframe lappend lindex linsert list listbox llength load lower lrange lreplace lsearch lset lsort menu menubutton message namespace open option pack package panedwindow parray pid pkg_compareExtension pkg_mkIndex place proc puts pwd radiobutton raise read regexp regsub rename return scale scan scrollbar seek selection send set socket source spinbox split string subst switch tclLdAout tclLog tclPkgSetup tclPkgUnknown tcl_endOfWord tcl_findLibrary tcl_startOfNextWord tcl_startOfPreviousWord tcl_wordBreakAfter tcl_wordBreakBefore tell text time tk tk_chooseColor tk_chooseDirectory tk_getOpenFile tk_getSaveFile tk_menuSetFocus tk_messageBox tk_popup tk_textCopy tk_textCut tk_textPaste tkwait toplevel trace unknown unset update uplevel upvar variable vwait while winfo wm}
set ::knownProcs {auto_execok auto_import auto_load auto_load_index auto_mkindex auto_mkindex_old auto_qualify auto_reset history parray pkg_compareExtension pkg_mkIndex tclLdAout tclLog tclPkgSetup tclPkgUnknown tcl_endOfWord tcl_findLibrary tcl_startOfNextWord tcl_startOfPreviousWord tcl_wordBreakAfter tcl_wordBreakBefore tk_chooseColor tk_chooseDirectory tk_getOpenFile tk_getSaveFile tk_menuSetFocus tk_messageBox tk_popup tk_textCopy tk_textCut tk_textPaste unknown}
set ::syntax(after) {r 1}
set ::syntax(append) {n x*}
set ::syntax(array) {s v x?}
set {::syntax(array exists)} l
set {::syntax(array names)} {v x? x?}
set {::syntax(array set)} {n x}
set {::syntax(array size)} v
set {::syntax(array statistics)} v
set ::syntax(auto_execok) 1
set ::syntax(auto_import) 1
set ::syntax(auto_load) {r 1 2}
set ::syntax(auto_load_index) 0
set ::syntax(auto_mkindex) {r 1}
set ::syntax(auto_mkindex_old) {r 1}
set ::syntax(auto_qualify) 2
set ::syntax(auto_reset) 0
set ::syntax(binary) {s x*}
set {::syntax(binary scan)} {x x n n*}
set ::syntax(break) 0
set ::syntax(catch) {c n?}
set ::syntax(cd) {r 0 1}
set ::syntax(clock) {s x*}
set ::syntax(close) 1
set ::syntax(concat) {r 0}
set ::syntax(continue) 0
set ::syntax(encoding) {s x*}
set ::syntax(eof) 1
set ::syntax(error) {r 1 3}
set ::syntax(exec) {o* x x*}
set ::syntax(exit) {r 0 1}
set ::syntax(fblocked) 1
set ::syntax(fconfigure) {x o. x. p*}
set ::syntax(fcopy) {x x p*}
set ::syntax(file) {s x*}
set {::syntax(file lstat)} {x n}
set {::syntax(file stat)} {x n}
set ::syntax(fileevent) {x x x?}
set ::syntax(flush) 1
set ::syntax(for) {c E c c}
set ::syntax(format) {r 1}
set ::syntax(gets) {x n?}
set ::syntax(glob) {o* x x*}
set ::syntax(history) {s x*}
set ::syntax(image) {s x*}
set ::syntax(incr) {v x?}
set ::syntax(info) {s x*}
set {::syntax(info default)} {x x n}
set {::syntax(info exists)} l
set ::syntax(interp) {s x*}
set ::syntax(join) {r 1 2}
set ::syntax(lappend) {n x*}
set ::syntax(lindex) 2
set ::syntax(linsert) {r 3}
set ::syntax(list) {r 0}
set ::syntax(llength) 1
set ::syntax(load) {r 1 3}
set ::syntax(lrange) 3
set ::syntax(lreplace) {r 3}
set ::syntax(lsearch) {o* x x}
set ::syntax(lsort) {o* x}
set ::syntax(namespace) {s x*}
set ::syntax(open) {r 1 3}
set ::syntax(package) {s x*}
set ::syntax(parray) {v x?}
set ::syntax(pid) {r 0 1}
set ::syntax(pkg_compareExtension) {r 1 2}
set ::syntax(pkg_mkIndex) {r 0}
set ::syntax(puts) {1: x : o? x x?}
set ::syntax(pwd) 0
set ::syntax(read) {r 1 2}
set ::syntax(regexp) {o* x x n*}
set ::syntax(regsub) {o* x x x n}
set ::syntax(rename) 2
set ::syntax(return) {p* x?}
set ::syntax(scan) {x x n*}
set ::syntax(seek) {r 2 3}
set ::syntax(set) {1: v : n x}
set ::syntax(socket) {r 2}
set ::syntax(source) 1
set ::syntax(split) {r 1 2}
set ::syntax(string) {s x x*}
set {::syntax(string bytelength)} 1
set {::syntax(string compare)} {o* x x}
set {::syntax(string equal)} {o* x x}
set {::syntax(string first)} {r 2 3}
set {::syntax(string index)} 2
set {::syntax(string is)} {x o* x}
set {::syntax(string last)} {r 2 3}
set {::syntax(string length)} 1
set {::syntax(string map)} {o? x x}
set {::syntax(string match)} {o? x x}
set {::syntax(string range)} 3
set {::syntax(string repeat)} 2
set {::syntax(string replace)} {r 3 4}
set {::syntax(string tolower)} {r 1 3}
set {::syntax(string totitle)} {r 1 3}
set {::syntax(string toupper)} {r 1 3}
set {::syntax(string trim)} {r 1 2}
set {::syntax(string trimleft)} {r 1 2}
set {::syntax(string trimright)} {r 1 2}
set {::syntax(string wordend)} 2
set {::syntax(string wordstart)} 2
set ::syntax(subst) {o* x}
set ::syntax(tclLdAout) {r 0 3}
set ::syntax(tclLog) 1
set ::syntax(tclPkgSetup) 4
set ::syntax(tclPkgUnknown) {r 2 3}
set ::syntax(tcl_endOfWord) 2
set ::syntax(tcl_findLibrary) 6
set ::syntax(tcl_startOfNextWord) 2
set ::syntax(tcl_startOfPreviousWord) 2
set ::syntax(tcl_wordBreakAfter) 2
set ::syntax(tcl_wordBreakBefore) 2
set ::syntax(tell) 1
set ::syntax(time) {r 1 2}
set ::syntax(tk_chooseColor) {r 0}
set ::syntax(tk_chooseDirectory) {r 0}
set ::syntax(tk_getOpenFile) {r 0}
set ::syntax(tk_getSaveFile) {r 0}
set ::syntax(tk_menuSetFocus) 1
set ::syntax(tk_messageBox) {r 0}
set ::syntax(tk_popup) {r 3 4}
set ::syntax(tk_textCopy) 1
set ::syntax(tk_textCut) 1
set ::syntax(tk_textPaste) 1
set ::syntax(trace) {s x x*}
set {::syntax(trace variable)} {n x x}
set {::syntax(trace vdelete)} {v x*}
set {::syntax(trace vinfo)} l
set ::syntax(unknown) {r 0}
set ::syntax(unset) {l l*}
set ::syntax(update) s.
set ::syntax(vwait) n
set ::syntax(while) {E c}
set ::syntax(winfo) {s x x*}
set ::syntax(wm) {s x x*}

set ::subCmd(array) {anymore donesearch exists get names nextelement set size startsearch statistics unset}
set ::subCmd(binary) {format scan}
set ::subCmd(clock) {clicks format scan seconds}
set ::subCmd(encoding) {convertfrom convertto names system}
set ::subCmd(file) {atime attributes channels copy delete dirname executable exists extension isdirectory isfile join link lstat mtime mkdir nativename normalize owned pathtype readable readlink rename rootname separator size split stat system tail type volumes writable}
set ::subCmd(history) {add change clear event info keep nextid redo}
set ::subCmd(image) {create delete height inuse names type types width}
set ::subCmd(info) {args body cmdcount commands complete default exists functions globals hostname level library loaded locals nameofexecutable patchlevel procs script sharedlibextension tclversion vars}
set ::subCmd(interp) {alias aliases create delete eval exists expose hide hidden issafe invokehidden marktrusted recursionlimit slaves share target transfer}
set ::subCmd(namespace) {children code current delete eval exists export forget import inscope origin parent qualifiers tail which}
set ::subCmd(package) {forget ifneeded names present provide require unknown vcompare versions vsatisfies}
set ::subCmd(string) {bytelength compare equal first index is last length map match range repeat replace tolower toupper totitle trim trimleft trimright wordend wordstart}
set ::subCmd(trace) {add info remove variable vdelete vinfo}
set ::subCmd(update) idletasks
set ::subCmd(winfo) {cells children class colormapfull depth geometry height id ismapped manager name parent pointerx pointery pointerxy reqheight reqwidth rootx rooty screen screencells screendepth screenheight screenwidth screenmmheight screenmmwidth screenvisual server toplevel viewable visual visualid vrootheight vrootwidth vrootx vrooty width x y atom atomname containing interps pathname exists fpixels pixels rgb visualsavailable}
set ::subCmd(wm) {aspect attributes client colormapwindows command deiconify focusmodel frame geometry grid group iconbitmap iconify iconmask iconname iconposition iconwindow maxsize minsize overrideredirect positionfrom protocol resizable sizefrom stackorder state title transient withdraw}

set ::option(exec) {-keepnewline --}
set ::option(fconfigure) {-blocking -buffering -buffersize -encoding -eofchar -translation -mode -queue -ttystatus -xchar}
set ::option(fcopy) {-size -command}
set ::option(glob) {-directory -join -nocomplain -path -tails -types --}
set {::option(glob -directory)} 1
set {::option(glob -path)} 1
set {::option(glob -types)} 1
set ::option(lsearch) {-all -ascii -decreasing -dictionary -exact -glob -increasing -inline -integer -not -real -regexp -sorted -start}
set {::option(lsearch -start)} 1
set ::option(lsort) {-ascii -command -decreasing -dictionary -increasing -index -integer -real -unique}
set {::option(lsort -command)} 1
set {::option(lsort -index)} 1
set ::option(regexp) {-all -about -indices -inline -expanded -line -linestop -lineanchor -nocase -start --}
set {::option(regexp -start)} 1
set ::option(regsub) {-all -nocase -expanded -line -linestop -lineanchor -start --}
set {::option(regsub -start)} 1
set ::option(return) {-code -errorcode -errorinfo}
set {::option(string compare)} {-nocase -length}
set {::option(string compare -length)} 1
set {::option(string equal)} {-nocase -length}
set {::option(string equal -length)} 1
set {::option(string is)} {-strict -failindex}
set {::option(string is -failindex)} n
set {::option(string map)} -nocase
set {::option(string match)} -nocase
set ::option(subst) {-nobackslashes -nocommands -novariables}
set ::option(switch) {-exact -glob -regexp --}

