#!/bin/sh

rm -f nagelfar.tar.gz
tar --directory=.. -zcvf nagelfar.tar.gz nagelfar/COPYING nagelfar/README.txt nagelfar/syntaxbuild.tcl nagelfar/syntaxdb.tcl nagelfar/nagelfar.syntax nagelfar/nagelfar.tcl nagelfar/tests/test.tcl nagelfar/tests/test.syntax nagelfar/doc --exclude RCS
