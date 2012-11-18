# This is an experiment to check snit

##############################################################################
# The generic definitions needed for Snit are now handled centrally
##############################################################################

##############################################################################
# This is the annotation needed for this object definition:
# pdf4tcl::pdf4tcl
##############################################################################

##nagelfar syntax pdf4tcl::pdf4tcl dc=_obj,pdf4tcl p*
##nagelfar return pdf4tcl::pdf4tcl _obj,pdf4tcl
##nagelfar option pdf4tcl::pdf4tcl -file
##nagelfar option _obj,pdf4tcl\ configure -file

##nagelfar implicitvarns snit::type::pdf4tcl::pdf4tcl self\ _obj,pdf4tcl pdf

snit::type pdf4tcl::pdf4tcl {
    variable pdf
    option -file      -default "" -readonly 1
    constructor {args} {
        $self configurelist $args
    }
    destructor {
        $self finish
        close $pdf(ch)
    }
    method cleanup {} {
        $self destroy
    }
    method finish {} {
        $self RequireVersion a
    }
    method RequireVersion {version} {
        $self finish
        if {$version > $pdf(version)} {
            set pdf(version) $version
        }
    }
}

set x [pdf4tcl::pdf4tcl %AUTO% -file xx]
$x cleanup

pdf4tcl::pdf4tcl myobj -file xx
myobj cleanup
myobj configure -file apa
myobj RequireVersion y

