# This is an experiment to check itcl

##nagelfar syntax itcl::class x cn
##nagelfar syntax itcl::class::method x cv
##nagelfar syntax itcl::class::proc x cv
##nagelfar syntax itcl::class::constructor cv
##nagelfar syntax itcl::class::destructor cl
##nagelfar syntax itcl::class::common n x?
##nagelfar syntax itcl::class::private s x*
##nagelfar syntax itcl::class::protected s x*
##nagelfar syntax itcl::class::public s x*

##nagelfar implicitvar itcl::class::test this v1 v2

itcl::class test {
    common v1 0
    common v2 0
    puts "Initial v1 and v2 for class test is $v1 $v2"
    method setv1 {v} {
        set v1 $v
    }
    method setv2 {v} {
        set v2 $v
    }
    method showvalues {} {
        puts "$this has commmon values $v1 $v2"
    }
}

# Define the class command
##nagelfar syntax Test x
##nagelfar return Test _obj,Test

##nagelfar syntax _obj,Test s x*
##nagelfar subcmd _obj,Test m1 m2
##nagelfar syntax _obj,Test\ m1 0
##nagelfar syntax _obj,Test\ m2 0

##nagelfar syntax itcl::class::Test::m2 0
##nagelfar syntax itcl::class::Test::p 0

##nagelfar implicitvar itcl::class::Test::proc   c1
##nagelfar implicitvar itcl::class::Test::method x1 x2 x3 c1

itcl::class Test {
    private variable x1 "1"      ; # Visible in all instance methods
    protected variable x2 "2"    ; # Visible in all instance methods
    public variable x3  "3"      ; # Visible in all instance methods
    common c1 "4"                ; # Visible in all instance methods and class procs
    method m1 {} {
        # Printing instance vars and common vars
        puts "x1 is $x1"
        puts "x2 is $x2"
        puts "x3 is $x3"
        puts "c1 is $c1"

        # Calling a class proc
        p

        # Calling an instance method
        m2
    }

    method m2 {} {
        puts "Called m2"
    }

    proc p {} {
        # puts "x1 is $x1"   <-- This fails, because a class proc doesn't have access to anything but common vars
        puts "c1 is $c1"
    }
}

Test t1
##nagelfar copy _obj,Test t1

# Calling an instance method and a class proc
t1 m1

##nagelfar copy itcl::class::Test::p Test::p
# Calling a class proc without an instance
Test::p
