#----------------------------------------------------------------------
# Make file for Nagelfar
#----------------------------------------------------------------------
# $Revision$
#----------------------------------------------------------------------

# Path to the TclKits used for creating StarPacks.
TCLKIT = /home/peter/tclkit
TCLKIT_LINUX   = $(TCLKIT)/tclkit-linux-x86
TCLKIT_SOLARIS = $(TCLKIT)/tclkit-solaris-sparc
TCLKIT_WIN     = $(TCLKIT)/tclkit-win32.upx.exe

# Path to the libraries used
GRIFFIN = /home/peter/tclkit/griffin.vfs/lib/griffin
TKDND   = /home/peter/tkdnd/lib/tkdnd1.0

# Path to the interpreter used for generating the syntax database
TCLSHDB  = ~/tcl/install/bin/wish8.4
TCLSHDB2 = ~/tcl/install/bin/wish8.5
DB2NAME  = syntaxdb85.tcl
TCLSH85  = ~/tcl/install/bin/tclsh8.5

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
	cd nagelfar.vfs/lib ; ln -s $(GRIFFIN) griffin
nagelfar.vfs/lib/tkdnd:
	cd nagelfar.vfs/lib ; ln -s $(TKDND) tkdnd

links: nagelfar.vfs/lib/app-nagelfar/nagelfar.tcl \
	nagelfar.vfs/lib/app-nagelfar/syntaxdb.tcl \
	nagelfar.vfs/lib/app-nagelfar/doc \
	nagelfar.vfs/lib/griffin \
	nagelfar.vfs/lib/tkdnd

setup: links

#----------------------------------------------------------------
# Testing
#----------------------------------------------------------------

spell:
	@cat doc/*.txt | ispell -d british -l | sort -u

check:
	@./nagelfar.tcl nagelfar.tcl

test:
	@./tests/all.tcl

test85:
	@$(TCLSH85) ./tests/all.tcl

#----------------------------------------------------------------
# Generating test examples
#----------------------------------------------------------------

misctests/test.result: misctests/test.tcl nagelfar.tcl
	@cd misctests; ../nagelfar.tcl test.tcl > test.result

misctests/test.html: misctests/test.tcl misctests/htmlize.tcl \
		misctests/test.result
	cd misctests; ./htmlize.tcl

misctest: misctests/test.result misctests/test.html

#----------------------------------------------------------------
# Generating database
#----------------------------------------------------------------

syntaxdb.tcl: syntaxbuild.tcl $(TCLSHDB)
	$(TCLSHDB) syntaxbuild.tcl syntaxdb.tcl

$(DB2NAME): syntaxbuild.tcl $(TCLSHDB2)
	$(TCLSHDB2) syntaxbuild.tcl $(DB2NAME)

db: syntaxdb.tcl $(DB2NAME)

#----------------------------------------------------------------
# Packaging/Releasing
#----------------------------------------------------------------

wrap:
	sdx wrap nagelfar.kit

wrapexe:
	@\rm -f nagelfar nagelfar.exe nagelfar.solaris
	sdx wrap nagelfar         -runtime $(TCLKIT_LINUX)
	sdx wrap nagelfar.solaris -runtime $(TCLKIT_SOLARIS)
	sdx wrap nagelfar.exe     -runtime $(TCLKIT_WIN)

distrib:
	@\rm -f nagelfar.tar.gz
	@tar --directory=.. -zcvf nagelfar.tar.gz nagelfar/COPYING \
		nagelfar/README.txt nagelfar/syntaxbuild.tcl nagelfar/syntaxdb.tcl \
		nagelfar/nagelfar.syntax nagelfar/nagelfar.tcl \
		nagelfar/misctests/test.tcl nagelfar/misctests/test.syntax \
		nagelfar/doc --exclude CVS

release: setup misctest db distrib wrap wrapexe
