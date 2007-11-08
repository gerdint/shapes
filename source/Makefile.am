DVERSION := -D'VERSION_DATE="$(shell date '+%Y-%m-%d %H:%M')"' \
-D'VERSION_REVISION="$(shell svn info .. | sed -e '/Last Changed Rev/! d' -e 's/Last Changed Rev: //')"' \
-D'VERSION_NUMBER="$(shell cat VERSION | sed -e '2,$$ d')"'

AM_CPPFLAGS = -Wall -Wsynth -DDISPATCHSTYLE=DISPATCHSTYLE_CASE ${GSL_CFLAGS} ${ZLIB_CFLAGS} \
              -I../tools/include
AM_LDFLAGS = -lz -lm -lc
LIBS = ${LIBICONV} ${GSL_LIBS} ${ZLIB_LIBS}
AM_LFLAGS = -olex.yy.c -d
AM_YFLAGS = -dvt -pshapes
CLEANFILES = shapesparser.h shapesparser.cc  shapesparser.output \
             afmyylex.cc shapesyylex.cc pdfyylex.cc
shapes_LDADD = version.o

shapesparser.h : shapesparser.cc

bin_PROGRAMS = shapes

nodist_shapes_SOURCES = shapesparser.h
BUILT_SOURCES = shapesparser.h

shapes_SOURCES = shapesyylex.ll \
                pdfstructure.cc pdfscanner.cc pdfyylex.ll simplepdfo.cc \
		simplepdfi.cc consts.cc \
		../tools/source/autoonoff.cc \
		../tools/source/physical.cc \
		continuations.cc \
		gettypeid.cc \
		elementarytypes.cc \
		concretecolors.cc \
		facetnormals.cc facettypes.cc texttypes.cc \
		pagecontentstates.cc \
		elementarycoords.cc statetypes.cc pathtypes.cc elementarypath2D.cc \
		elementarypath3D.cc bezier.cc upsamplers.cc \
		pathslider2D.cc pathslider3D.cc drawabletypes.cc shadingtypes.cc \
		functiontypes.cc pdffunctiontypes.cc \
		lighttypes.cc containertypes.cc timetypes.cc hottypes.cc \
		multipage.cc annotations.cc tagtypes.cc shapesvalue.cc \
		shapesscanner.cc sourcelocation.cc shapesexceptions.cc strrefdup.cc \
		shapesparser.yy environment.cc dynamicenvironment.cc registerclasses.cc \
		shapescore.cc coreast.cc coreelem.cc coreconstruct.cc coreannotation.cc \
	        corepoint.cc corepath.cc coremisc.cc coredraw.cc corestate.cc \
		angleselect.cc texlabelmanager.cc specialunits.cc trianglefunctions.cc basicsimplex.cc \
		computeelementarypath.cc \
		zbuftriangle.cc zbufline.cc zbufto2D.cc zbufinternals.cc zsorterto2D.cc \
		ast.cc astfun.cc astvalues.cc astvar.cc astexpr.cc \
		astexpr_unary.cc astexpr_arithmetic.cc astexpr_relational.cc \
		coreoperators.cc astflow.cc \
		classtypes.cc astclass.cc errorhandlers.cc constructorrepresentation.cc charconverters.cc \
		fontmetrics.cc afmscanner.cc afmyylex.ll fonttypes.cc corefont.cc glyphlist.cc \
		characterencoding.cc \
		debuglog.cc globals.cc main.cc


version.o : version.cc FORCE
	${COMPILE} ${DVERSION} -c $< -o $@

FORCE :