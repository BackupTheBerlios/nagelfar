# This is an experiment to get tdbc-style OO checked.

##nagelfar syntax tdbc::sqlite3::connection s x x

tdbc::sqlite3::connection create ::db $::testDBName

##nagelfar syntax db s x*
##nagelfar subcmd db prepare allrows foreach begintransaction commit rollback transaction tables columns configure resultsets
##nagelfar syntax db\ prepare x
##nagelfar syntax db\ begintransaction 0
##nagelfar syntax db\ commit 0
##nagelfar syntax db\ configure o. x. p*
##nagelfar option db\ configure -encoding -isolation -timeout
##nagelfar syntax db\ columns x x?
##nagelfar syntax db\ tables x?
##nagelfar syntax db\ rollback 0
##nagelfar syntax db\ resultsets 0
##nagelfar syntax db\ transaction c
##nagelfar syntax db\ allrows p* x x?
##nagelfar option db\ allrows -as -- -columnsvar
##nagelfar option db\ allrows\ -columnsvar n
##nagelfar syntax db\ foreach p* n x c
##nagelfar option db\ foreach -as -- -columnsvar
##nagelfar option db\ foreach\ -columnsvar n
##nagelfar return db\ prepare _obj,statement

##nagelfar syntax _obj,statement s x*
##nagelfar subcmd _obj,statement execute paramtype close foreach params allrows resultsets
##nagelfar syntax _obj,statement\ execute 0
##nagelfar syntax _obj,statement\ close 0
##nagelfar syntax _obj,statement\ params 0
##nagelfar syntax _obj,statement\ resultsets 0
##nagelfar syntax _obj,statement\ allrows p* x?
##nagelfar option _obj,statement\ allrows -as -- -columnsvar
##nagelfar option _obj,statement\ allrows\ -columnsvar n
##nagelfar syntax _obj,statement\ foreach p* n c
##nagelfar option _obj,statement\ foreach -as -- -columnsvar
##nagelfar option _obj,statement\ foreach\ -columnsvar n
##nagelfar syntax _obj,statement\ paramtype x x x*
##nagelfar return _obj,statement\ execute _obj,result

##nagelfar syntax _obj,result s x*
##nagelfar subcmd _obj,result rowcount columns nextrow foreach close allrows
##nagelfar syntax _obj,result\ rowcount 0
##nagelfar syntax _obj,result\ columns 0
##nagelfar syntax _obj,result\ close 0
##nagelfar syntax _obj,result\ foreach p* n c
##nagelfar option _obj,result\ foreach -as -- -columnsvar
##nagelfar option _obj,result\ foreach\ -columnsvar n
##nagelfar syntax _obj,result\ allrows p*
##nagelfar option _obj,result\ allrows -as -- -columnsvar
##nagelfar option _obj,result\ allrows\ -columnsvar n
##nagelfar syntax _obj,result\ nextrow p* n
##nagelfar option _obj,result\ nextrow -as --

set stmt [::db prepare {
    CREATE TABLE people(
                        idnum INTEGER PRIMARY KEY,
                        name VARCHAR(40) NOT NULL,
                        info INTEGER
                        )
}]
set rs [$stmt execute]
list [expr {[$rs rowcount] <= 0}] [$rs columns] [$rs nextrow nothing]
rename $rs {}
rename $stmt {}

set stmt [::db prepare {
    INSERT INTO people(idnum, name, info) values(1, 'fred', 0)
}]
set rs [$stmt execute]
list [$rs rowcount] [$rs columns] [$rs nextrow nothing]
rename $rs {}
rename $stmt {}

set stmt [::db prepare {
    INSERT INTO people(idnum, name, info) values(:idnum, :name, 0)
}]
$stmt paramtype idnum integer
set rs [$stmt execute]
list [$rs rowcount] [$rs columns] [$rs nextrow nothing]
rename $rs {}
rename $stmt {}

set stmt [::db prepare {
    INSERT INTO people(idnum, name, info) values(:idnum, :name, :info)
}]
set stmt2 [::db prepare {
    SELECT name, info FROM people WHERE idnum = :idnum
}]
$stmt2 paramtype idnum integer

set name "mr. gravel"
set idnum 100
set rs [$stmt execute]
rename $rs {}
set rs [$stmt2 execute]
$rs nextrow -as dicts row
set row

set stmt [::db prepare {
    SELECT * FROM people
}]
set rs [$stmt execute]
$rs columns
rename $rs {}
rename $stmt {}

set stmt [::db prepare {
    SELECT idnum, name FROM people ORDER BY idnum
}]
set rs [$stmt execute]
set idnum 1
set names {}
$rs nextrow -- names
set names

$rs nextrow -as lists -- row

set stmt [::db prepare {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
}]
set rs [$stmt execute]
#set result {}
$rs foreach row {
    lappend result $row
}
set result
$rs close
$stmt close

set stmt [::db prepare {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
}]
$stmt foreach rowX {
    lappend resultX $rowX
}
set resultX
$stmt close

db foreach rowY {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
} {
    lappend resultY $rowY
}
set resultY

$rs foreach -- row {
    lappend resultZ $row
}
set resultZ

$stmt foreach -- row {
    lappend resultXX $row
}
set resultXX

db foreach -- row {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
} {
    lappend resultYY $row
}
set resultYY

$rs foreach -as lists row {
    lappend resultZZ $row
}
set resultZZ

$stmt foreach -as lists row {
    lappend resultXXX $row
}
set resultXXX

db foreach -as lists row {
    SELECT idnum, name, info FROM people WHERE name LIKE 'b%'
} {
    lappend resultYYY $row
}
set resultYYY

$rs foreach -as lists -- row {
    lappend resultZZZ $row
}
set resultZZZ

$stmt foreach -as lists -- row {
    lappend resultXXXX $row
}
set resultXXXX

db foreach -as lists row {
    SELECT idnum, name, info FROM people WHERE name LIKE 'b%'
} {
    lappend resultYYYY $row
}
set resultYYYY

$rs foreach -as lists -columnsvar c -- row {
    foreach cn $c cv $row {
        lappend result $cn $cv
    }
}
set result

$stmt foreach -as lists -columnsvar c -- row {
    foreach cn $c cv $row {
        lappend result $cn $cv
    }
}
set result

db foreach -as lists -columnsvar c -- row {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
} {
    foreach cn $c cv $row {
        lappend result $cn $cv
    }
}

$rs allrows
$stmt allrows
db allrows {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
}
$rs allrows --
$stmt allrows --
db allrows -- {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
}

$rs allrows -as lists
$stmt allrows -as lists
db allrows -as lists {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
}

set rs [$stmt execute]
$rs allrows -as lists --
$stmt allrows -as lists --

set result [$rs allrows -as lists -columnsvar c]
list $c $result

set result [$stmt allrows -as lists -columnsvar c]
list $c $result

set result [db allrows -as lists -columnsvar c {
    SELECT idnum, name FROM people WHERE name LIKE 'b%'
}]
list $c $result

$stmt allrows {thePattern b%}
db allrows {
    SELECT idnum, name FROM people WHERE name LIKE :thePattern
} {thePattern b%}

list \
        [llength [$stmt resultsets]] \
        [llength [::db resultsets]]

::db begintransaction
::db commit
::db begintransaction
::db rollback

db transaction {
    set transVar 1
}
set transVar

::db tables q%
set dict [::db tables]
set dict [::db tables p%]
::db columns rubbish
::db columns people i%

set s [::db prepare {
    SELECT name FROM people 
}]
$s params

::db configure
::db configure -encoding utf-8
::db configure -encoding
::db configure -isolation readuncommitted
::db configure -isolation
::db configure -timeout 0
::db configure -timeout

proc testingwithproc {stmt} {
    ##nagelfar variable stmt _obj,statement
    $stmt foreach row {
        lappend result $row
    }
    set result
    set row
}
