setup:
	cd nagelfar.vfs/lib ;\
	ln -sf /home/peter/tclkit/griffin.vfs/lib/griffin ;\
	ln -sf /home/peter/tkdnd/lib/tkdnd1.0
	cd nagelfar.vfs/lib/app-nagelfar ;\
	ln -sf ../../../doc ;\
	ln -sf ../../../nagelfar.tcl ;\
	ln -sf ../../../syntaxdb.tcl

test:
	@./tests/all.tcl

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
