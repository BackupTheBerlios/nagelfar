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
CTEXT   = /home/peter/src/ctext

# Path to the interpreter used for generating the syntax database
TCLSHDB  = ~/tcl/install/bin/wish8.4
TCLSHDB2 = ~/tcl/install/bin/wish8.5
DB2NAME  = syntaxdb85.tcl
TCLSH85  = ~/tcl/install/bin/tclsh8.5

all: base

base: nagelfar.tcl setup misctest db

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
nagelfar.vfs/lib/ctext:
#	cd nagelfar.vfs/lib ; ln -s $(CTEXT) ctext

links: nagelfar.vfs/lib/app-nagelfar/nagelfar.tcl \
	nagelfar.vfs/lib/app-nagelfar/syntaxdb.tcl \
	nagelfar.vfs/lib/app-nagelfar/doc \
	nagelfar.vfs/lib/griffin \
	nagelfar.vfs/lib/tkdnd \
	nagelfar.vfs/lib/ctext

setup: links

#----------------------------------------------------------------
# Concatening source
#----------------------------------------------------------------

CATFILES = src/nagelfar.tcl src/gui.tcl src/dbbrowser.tcl \
	src/registry.tcl src/preferences.tcl src/startup.tcl


nagelfar.tcl: $(CATFILES)
	cat $(CATFILES) > nagelfar.tcl
	@chmod 775 nagelfar.tcl

#----------------------------------------------------------------
# Testing
#----------------------------------------------------------------

spell:
	@cat doc/*.txt | ispell -d british -l | sort -u

check:
	@./nagelfar.tcl -strictappend nagelfar.tcl

test: base
	@./tests/all.tcl $(TESTFLAGS)

test85:
	@$(TCLSH85) ./tests/all.tcl $(TESTFLAGS)

#----------------------------------------------------------------
# Coverage
#----------------------------------------------------------------

# Source files for code coverage
SRCFILES = $(CATFILES)
IFILES   = $(SRCFILES:.tcl=.tcl_i)
LOGFILES = $(SRCFILES:.tcl=.tcl_log)
MFILES   = $(SRCFILES:.tcl=.tcl_m)

# Instrument source file for code coverage
%.tcl_i: %.tcl
	@./nagelfar.tcl -instrument $<

# Target to prepare for code coverage run. Makes sure log file is clear.
instrument: $(IFILES)
	@rm -f $(LOGFILES)

# Top file for coverage run
nagelfar.tcl_i: $(IFILES)
	@rm -f nagelfar.tcl_i
	@touch nagelfar.tcl_i
	@for i in $(IFILES) ; do echo "source $$i" >> nagelfar.tcl_i ; done

# Run tests to create log file.
testcover $(LOGFILES): nagelfar.tcl_i
	@./tests/all.tcl $(TESTFLAGS)
	@$(TCLSH85) ./tests/all.tcl -match expand-*

# Create markup file for better view of result
%.tcl_m: %.tcl_log 
	@./nagelfar.tcl -markup $*.tcl

# View code coverage result
icheck: $(MFILES)
	@for i in $(SRCFILES) ; do eskil -noparse $$i $${i}_m & done

# Remove code coverage files
clean:
	@rm -f $(LOGFILES) $(IFILES) $(MFILES) nagelfar.tcl_i

#----------------------------------------------------------------
# Generating test examples
#----------------------------------------------------------------

misctests/test.result: misctests/test.tcl nagelfar.tcl
	@cd misctests; ../nagelfar.tcl test.tcl > test.result

misctests/test.html: misctests/test.tcl misctests/htmlize.tcl \
		misctests/test.result
	@cd misctests; ./htmlize.tcl

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

release: base distrib wrap wrapexe
