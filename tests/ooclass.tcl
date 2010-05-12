# This is an experiment to check oo in 8.6

# Define the class command

##nagelfar syntax Account s x*
##nagelfar subcmd Account create new
##nagelfar syntax Account\ create x x?
##nagelfar return Account\ create _obj,Account
##nagelfar syntax Account\ new x?
##nagelfar return Account\ new _obj,Account

# Define the object command

##nagelfar syntax _obj,Account s x*
##nagelfar subcmd _obj,Account deposit withdraw transfer dump destroy variable
##nagelfar syntax _obj,Account\ deposit x
##nagelfar syntax _obj,Account\ withdraw x
##nagelfar syntax _obj,Account\ transfer x x
##nagelfar syntax _obj,Account\ dump 0
##nagelfar syntax _obj,Account\ destroy 0
##nagelfar syntax _obj,Account\ variable n*

# Define that "my" within the definition is an object

##nagelfar copy _obj,Account oo::class\ create::Account::my

oo::class create Account {
    constructor {{ownerName undisclosed}} {
        my variable total overdrawLimit owner
        set total 0
        set overdrawLimit 10
        set owner $ownerName
    }
    method deposit amount {
        my variable total
        set total [expr {$total + $amount}]
    }
    method withdraw amount {
        my variable total overdrawLimit
        if {($amount - $total) > $overdrawLimit} {
            error "Can't overdraw - total: $total, limit: $overdrawLimit"
        }
        set total [expr {$total - $amount}]
    }
    method transfer {amount targetAccount} {
        my variable total
        my withdraw $amount
        $targetAccount deposit $amount
        set total
    }
    destructor {
        my variable total
        if {$total} {puts "remaining $total will be given to charity"}
    }
}

set a [Account new "John Doe"]
$a deposit 200
$a deposit 20
$a withdraw 150
$a withdraw 100
$a dump
set b [Account new]
$a transfer 65 $b
$a dump
$b dump
$a transfer 1000000 $b
$b destroy

