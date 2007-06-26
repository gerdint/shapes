CC = g++
LD = g++
AR = ar
YACC = $(shell ../tools/source/whichYACC)
LEX = flex++
LEXERFIXER = ../tools/source/lexerfixer
LBLAS = $(shell ../tools/source/whichlBLAS)
LLAPACK = $(shell ../tools/source/whichlLAPACK)
ILAPACK := $(shell ../tools/source/whichiLAPACK)
LICONV = $(shell ../tools/source/whichlICONV)
DICONV = $(shell ../tools/source/whichdICONV)
DVERSION := -D'VERSION_DATE="$(shell date '+%Y-%m-%d %H:%M')"' \
-D'VERSION_REVISION="$(shell svn info .. | sed -e '/Last Changed Rev/! d' -e 's/Last Changed Rev: //')"' \
-D'VERSION_NUMBER="$(shell cat VERSION | sed -e '2,$$ d')"'

PROGRAM = ../bin/drool
OBJECTS = metapdfyylex.o \
pdfstructure.o pdfscanner.o pdfyylex.o simplepdfo.o simplepdfi.o consts.o \
../tools/source/autoonoff.o \
../tools/source/physical.o \
continuations.o \
gettypeid.o elementarytypes.o concretecolors.o facetnormals.o facettypes.o texttypes.o \
pagecontentstates.o \
elementarycoords.o statetypes.o pathtypes.o elementarypath2D.o elementarypath3D.o \
pathslider2D.o pathslider3D.o drawabletypes.o shadingtypes.o functiontypes.o pdffunctiontypes.o \
lighttypes.o containertypes.o timetypes.o hottypes.o multipage.o annotations.o tagtypes.o metapdfvalue.o \
metapdfscanner.o sourcelocation.o metapdfexceptions.o strrefdup.o \
metapdfparser.o environment.o dynamicenvironment.o \
metapdfcore.o coreelem.o coreconstruct.o coreannotation.o corepoint.o corepath.o coremisc.o coredraw.o corestate.o \
angleselect.o texlabelmanager.o specialunits.o trianglefunctions.o basicsimplex.o \
computeelementarypath.o \
zbuftriangle.o zbufline.o zbufto2D.o zbufinternals.o zsorterto2D.o \
metapdfast.o metapdfastfun.o metapdfastvalues.o metapdfastvar.o metapdfastexpr.o \
metapdfastexpr_unary.o metapdfastexpr_arithmetic.o metapdfastexpr_relational.o coreoperators.o metapdfastflow.o \
classtypes.o metapdfastclass.o errorhandlers.o constructorrepresentation.o charconverters.o \
fontmetrics.o afmscanner.o afmyylex.o fonttypes.o corefont.o glyphlist.o characterencoding.o \
globals.o main.o

FONTMETER = fontmeter
FONTMETEROBJECTS = testers/fontmeter.o strrefdup.o fontmetrics.o afmscanner.o afmyylex.o

NONCOMPILED := $(patsubst %.o,%.cc, $(filter-out ../tools/source/autoonoff.o $(wildcard *.o), $(OBJECTS) ) )
BUTmetapdfvalue := $(filter-out metapdfvalue.o, $(OBJECTS) )


# When building a release version, the CHECK macro may be turned off by -DNOCHECK
#  pagescanner.o pageyylex.o
CFLAGS = -Wall -Wsynth -DDISPATCHSTYLE=DISPATCHSTYLE_CASE $(DICONV) -g

# The -d flag tells lex to set up for debugging. Can turn on/off by
# setting value of global yy_flex_debug inside the scanner itself
LEXFLAGS = -d

# The -d flag tells yacc to generate header with token types
# The -v flag writes out a verbose description of the states and conflicts
# The -t flag turns on debugging capability
YACCFLAGS = -dvt

ARFLAGS = -r

# The order here is important in order for clapack.h to be taken from the framework in case it exists there,
# and only otherwise from ../tools/include.
# $(ILAPACK) is no longer used since I use gsl instead.
INCS = -I../tools/include -I$(SVNCO)/include

LIBS = -lm -lc -ll -lz -lgsl $(LBLAS) $(LICONV)

all:$(PROGRAM) $(LEXERFIXER)

%yylex.cc: %yylex.l %scanner.h $(LEXERFIXER)
	$(LEX) $(LEXFLAGS) $<
	$(LEXERFIXER) < lex.yy.cc > $@
#	sed -e 's/class istream;/#include <iostream>\nusing namespace std;/' < lex.yy.cc > $@

%.o : %.cc
	$(CC) $(CFLAGS) -c -o $@ $< $(INCS)

#pdfparser.o: pdfparser.tab.c
#	$(CC) $(CFLAGS) -c -o $@ $< $(INCS)
#pdfparser.tab.h pdfparser.tab.c: pdfparser.y
#	$(YACC) -p pdf $(YACCFLAGS) $<

%parser.o: %parser.tab.c
	$(CC) $(CFLAGS) -c -o $@ $< $(INCS)
%parser.tab.h %parser.tab.c: %parser.y
	$(YACC) -p $* $(YACCFLAGS) $<

classtree2.h: classtree1.h
	cat $< | sed -e 's/CLASSTREE1/CLASSTREE2/' > $@

$(PROGRAM): classtree2.h $(OBJECTS) VERSION
	$(CC) $(CFLAGS) -c -o version.o version.c $(DVERSION)
	$(LD) -o $(PROGRAM) $(OBJECTS) version.o $(LIBS)

install:
	mv $(PROGRAM) /home/rt/tidefelt/bin/

.PHONY: clean
clean: 
	rm -f metapdfyylex.cc *.o $(THELIB) $(PROGRAM) 
.PHONY: cleansome
cleansome: 
	rm -f metapdfyylex.cc $(BUTmetapdfvalue) $(THELIB) $(PROGRAM) 

$(LEXERFIXER): ../tools/source/lexerfixer.l
	flex $<
	$(CC) $(CFLAGS) -o $@ lex.yy.c

gettypeid.o: classtree1.h classtree2.h

# metapdfparser.o: metapdfast*.h
metapdfyylex.cc: metapdfparser.tab.h

tools/bin/unbalanced: ../tools/source/unbalanced.l
	flex $<
	$(CC) $(CFLAGS) -o $@ lex.yy.c


.PHONY: whichYACC
whichYACC:
	echo $(YACC)

noncompiledlist: $(wildcard *.o)
	echo $(NONCOMPILED) > $@

$(FONTMETER): $(FONTMETEROBJECTS)
	$(LD) -o $(FONTMETER) $(FONTMETEROBJECTS) $(LIBS)
.PHONY: cleanfontmeter
cleanfontmeter: 
	rm -f afmyylex.cc $(FONTMETEROBJECTS) $(FONTMETER) 

.PHONY: sourcesize
sourcesize:
	tar -zcf source.tgz *.h *.cc *.l *.y Makefile
	ls -l source.tgz
	rm source.tgz
