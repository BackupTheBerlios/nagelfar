#!/bin/sh
\rm -f nagelfar nagelfar.exe nagelfar.solaris
\rm -f nagelfar.gz nagelfar.exe.gz nagelfar.solaris.gz
sdx wrap nagelfar -runtime /home/peter/tclkit/tclkit-linux-x86
sdx wrap nagelfar.solaris -runtime /home/peter/tclkit/tclkit-solaris-sparc
sdx wrap nagelfar.exe -runtime /home/peter/tclkit/tclkit-win32.upx.exe
#gzip nagelfar nagelfar.exe nagelfar.solaris
