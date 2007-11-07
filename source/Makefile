CC = g++
LD = g++
AR = ar
YACC = $(shell ../tools/source/whichYACC)
LEX = flex -+
LBLAS = $(shell ../tools/source/whichlBLAS)
LICONV = $(shell ../tools/source/whichlICONV)
DICONV = $(shell ../tools/source/whichdICONV)
DVERSION := -D'VERSION_DATE="$(shell date '+%Y-%m-%d %H:%M')"' \
-D'VERSION_REVISION="$(shell svn info .. | sed -e '/Last Changed Rev/! d' -e 's/Last Changed Rev: //')"' \
-D'VERSION_NUMBER="$(shell cat VERSION | sed -e '2,$$ d')"'

PROGRAM = ../bin/shapes
OBJECTS = shapesyylex.o \
pdfstructure.o pdfscanner.o pdfyylex.o simplepdfo.o simplepdfi.o consts.o \
../tools/source/autoonoff.o \
../tools/source/physical.o \
continuations.o \
gettypeid.o elementarytypes.o concretecolors.o facetnormals.o facettypes.o texttypes.o \
pagecontentstates.o \
elementarycoords.o statetypes.o pathtypes.o elementarypath2D.o elementarypath3D.o bezier.o upsamplers.o \
pathslider2D.o pathslider3D.o drawabletypes.o shadingtypes.o functiontypes.o pdffunctiontypes.o \
lighttypes.o containertypes.o timetypes.o hottypes.o multipage.o annotations.o tagtypes.o shapesvalue.o \
shapesscanner.o sourcelocation.o shapesexceptions.o strrefdup.o \
shapesparser.o environment.o dynamicenvironment.o registerclasses.o \
shapescore.o coreast.o coreelem.o coreconstruct.o coreannotation.o corepoint.o corepath.o coremisc.o coredraw.o corestate.o \
angleselect.o texlabelmanager.o specialunits.o trianglefunctions.o basicsimplex.o \
computeelementarypath.o \
zbuftriangle.o zbufline.o zbufto2D.o zbufinternals.o zsorterto2D.o \
ast.o astfun.o astvalues.o astvar.o astexpr.o \
astexpr_unary.o astexpr_arithmetic.o astexpr_relational.o coreoperators.o astflow.o \
classtypes.o astclass.o errorhandlers.o constructorrepresentation.o charconverters.o \
fontmetrics.o afmscanner.o afmyylex.o fonttypes.o corefont.o glyphlist.o characterencoding.o \
debuglog.o globals.o main.o

FONTMETER = fontmeter
FONTMETEROBJECTS = testers/fontmeter.o strrefdup.o fontmetrics.o afmscanner.o afmyylex.o

NONCOMPILED := $(patsubst %.o,%.cc, $(filter-out ../tools/source/autoonoff.o $(wildcard *.o), $(OBJECTS) ) )
BUTshapesvalue := $(filter-out shapesvalue.o, $(OBJECTS) )


# When building a release version, the CHECK macro may be turned off by -DNOCHECK
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
# and only otherwise from ../tools/include.  In ../tools/include there are copies of relevant files from $(SVNCO)/include.
INCS = -I../tools/include

LIBS = -lm -lc -ll -lz -lgsl $(LBLAS) $(LICONV)

all:$(PROGRAM)

%yylex.cc: %yylex.l %scanner.h
	$(LEX) $(LEXFLAGS) -o$@ $<

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
	rm -f shapesyylex.cc *.o $(THELIB) $(PROGRAM) 
.PHONY: cleansome
cleansome: 
	rm -f shapesyylex.cc $(BUTshapesvalue) $(THELIB) $(PROGRAM) 

lexerfixer-%: lexerfixer-%.l
	flex $<
	$(CC) $(CFLAGS) -o $@ lex.yy.c

gettypeid.o: classtree1.h classtree2.h

# shapesparser.o: ast*.h
shapesyylex.cc: shapesparser.tab.h

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
