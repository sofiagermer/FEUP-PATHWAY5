#
# Copyright (C) 1988-1998 Swedish Institute of Computer Science.
#
#
# NOTE: This Makefile requires GNU make.
#
# This file is probably not useful for end users. It is for internal testing of SICStus.

SRCDIR=.

all: run_simple1 run_queens

# Allow override from command line (for test suite on installed SICStus)
COMMON = $(SRCDIR)/../../../Common
# Include common Makefile parts.
include $(COMMON)

GLOBALSTKSIZE=5MB
PROLOGINITSIZE=10MB

targets: jqueens.po simple.po Queens.class Simple.class

.SUFFIXES: .pl .po .class
.pl.po:
	-echo "compile('$<'), save_files(['$^'],'$@'), halt." | $(JASPER_ENV) $(SICSTUS) -f

%.class: %.java
	$(JAVAC) -verbose -deprecation -classpath '$(call NATIVEPATH, $(SP_BINAUXDIR)/jasper.jar)$(PATHSEP).' $^

.PHONY: run_queens
run_queens: jqueens.po Queens.class
	( \
	$(if $(GLOBALSTKSIZE), GLOBALSTKSIZE=$(GLOBALSTKSIZE); export GLOBALSTKSIZE; ) \
	$(if $(PROLOGINITSIZE), PROLOGINITSIZE=$(PROLOGINITSIZE); export PROLOGINITSIZE; ) \
	$(JAVAEXEC) $(JAVA_EXECOPTS) \
		-classpath '$(call NATIVEPATH, $(SP_BINAUXDIR)/jasper.jar)$(PATHSEP).' \
		Queens $(BSIZE) $(NSOLS); \
	)

.PHONY: run_simple1
run_simple1: Simple.class simple.po
	( \
	$(if $(GLOBALSTKSIZE), GLOBALSTKSIZE=$(GLOBALSTKSIZE); export GLOBALSTKSIZE; ) \
	$(if $(PROLOGINITSIZE), PROLOGINITSIZE=$(PROLOGINITSIZE); export PROLOGINITSIZE; ) \
	$(JAVAEXEC) $(JAVA_EXECOPTS)  \
		-classpath '$(call NATIVEPATH, $(SP_BINAUXDIR)/jasper.jar)$(PATHSEP).' \
		Simple \
	)

.PHONY: clean distclean
clean distclean:
	-rm -f $(patsubst %, '%', $(wildcard core *~ *.o *.$(JNI_SHSFX) *.class *.ql *.po *.sav)) $(RMDUMMY)





