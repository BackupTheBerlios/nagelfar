all: setup misctest db

#----------------------------------------------------------------
# Setup symbolic links from the VFS to the real files
#----------------------------------------------------------------

nagelfar.vfs/lib/app-nagelfar/nagelfar.tcl:
	cd nagelfar.vfs/lib/app-nagelfar ; ln -s ../../../nagelfar.tcl
nagelfar.vfs/lib/app-nagelfar/syntaxdb.tcl:
	cd nagelfar.vfs/lib/app-nagelfar ; ln -s ../../../syntaxdb.tcl
nagelfar.vfs/lib/app-nagelfar/doc:
	cd nagelfar.vfs/lib/app-nagelfar ; ln -s ../../../doc
nagelfar.vfs/lib/griffin:
	cd nagelfar.vfs/lib ; ln -s /home/peter/tclkit/griffin.vfs/lib/griffin
nagelfar.vfs/lib/tkdnd1.0:
	cd nagelfar.vfs/lib ; ln -s /home/peter/tclkit/griffin.vfs/lib/tkdnd1.0

links: nagelfar.vfs/lib/app-nagelfar/nagelfar.tcl \
	nagelfar.vfs/lib/app-nagelfar/syntaxdb.tcl \
	nagelfar.vfs/lib/app-nagelfar/doc \
	nagelfar.vfs/lib/griffin \
	nagelfar.vfs/lib/tkdnd1.0

setup: links

#----------------------------------------------------------------
# Testing
#----------------------------------------------------------------

selftest:
	@./nagelfar.tcl nagelfar.tcl

test:
	@./tests/all.tcl

#----------------------------------------------------------------
# Generating test examples
#----------------------------------------------------------------

misctests/test.result: misctests/test.tcl nagelfar.tcl
	@cd misctests; ../nagelfar.tcl test.tcl > test.result

misctests/test.html: misctests/test.tcl misctests/htmlize.tcl
	cd misctests; ./htmlize.tcl

misctest: misctests/test.result misctests/test.html

#----------------------------------------------------------------
# Generating database
#----------------------------------------------------------------

syntaxdb.tcl: syntaxbuild.tcl
	@~/tcl/install/bin/wish8.4 syntaxbuild.tcl syntaxdb.tcl

db: syntaxdb.tcl

#----------------------------------------------------------------
# Packaging/Releasing
#----------------------------------------------------------------

wrap:
	sdx wrap nagelfar.kit

wrapexe:
	@\rm -f nagelfar nagelfar.exe nagelfar.solaris
	sdx wrap nagelfar -runtime /home/peter/tclkit/tclkit-linux-x86
	sdx wrap nagelfar.solaris -runtime /home/peter/tclkit/tclkit-solaris-sparc
	sdx wrap nagelfar.exe -runtime /home/peter/tclkit/tclkit-win32.upx.exe

distrib:
	@\rm -f nagelfar.tar.gz
	@tar --directory=.. -zcvf nagelfar.tar.gz nagelfar/COPYING \
		nagelfar/README.txt nagelfar/syntaxbuild.tcl nagelfar/syntaxdb.tcl \
		nagelfar/nagelfar.syntax nagelfar/nagelfar.tcl \
		nagelfar/misctests/test.tcl nagelfar/misctests/test.syntax \
		nagelfar/doc --exclude CVS

release: distrib wrap wrapexe
