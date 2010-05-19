# This is an experiment to check snit

# The main thing missing from the engine is to handle the implicit variables.
# Should this be handled for individual commands or on a per namespace basis?

##nagelfar syntax snit::type x cn
##nagelfar syntax snit::type::method x cv
##nagelfar syntax snit::type::constructor cv
##nagelfar syntax snit::type::destructor cl
##nagelfar syntax snit::type::option x p*

##nagelfar syntax _obj,pdf4tcl s x*
##nagelfar subcmd _obj,pdf4tcl destroy cleanup finish RequireVersion configurelist
##nagelfar syntax _obj,pdf4tcl\ RequireVersion x x
##nagelfar syntax _obj,pdf4tcl\ destroy 0
##nagelfar syntax _obj,pdf4tcl\ cleanup 0
##nagelfar syntax _obj,pdf4tcl\ finish 0
##nagelfar syntax _obj,pdf4tcl\ configurelist x

##nagelfar syntax pdf4tcl::pdf4tcl x p*
##nagelfar option pdf4tcl::pdf4tcl -file
##nagelfar return pdf4tcl::pdf4tcl _obj,pdf4tcl

snit::type pdf4tcl::pdf4tcl {
    variable pdf
    option -file      -default "" -readonly 1
    constructor {args} {
        ##nagelfar variable self _obj,pdf4tcl
        $self configurelist $args
    }
    destructor {
        ##nagelfar variable self _obj,pdf4tcl
        ##nagelfar variable pdf
        $self finish
        close $pdf(ch)
    }
    method cleanup {} {
        ##nagelfar variable self _obj,pdf4tcl
        $self destroy
    }
    method RequireVersion {version} {
        ##nagelfar variable pdf
        if {$version > $pdf(version)} {
            set pdf(version) $version
        }
    }
}

set x [pdf4tcl::pdf4tcl %AUTO% -file x]
$x cleanup
