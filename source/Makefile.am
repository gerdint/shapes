AM_CPPFLAGS = -Wall -Wsynth -DDISPATCHSTYLE=DISPATCHSTYLE_CASE \
              -D'RESOURCES_DIR="${pkgdatadir}"' \
              ${GSL_CFLAGS} ${ZLIB_CFLAGS} -I${top_srcdir}/tools/include
AM_LDFLAGS = -lz -lm -lc
LIBS = ${LIBICONV} ${GSL_LIBS} ${ZLIB_LIBS}
AM_LFLAGS = -olex.yy.c -d
AM_YFLAGS = -dvt
CLEANFILES = shapesparser.output SHAPES-VERSION-FILE
shapes_LDADD = version.o

FORCE :
SHAPES-VERSION-FILE : FORCE
	@${srcdir}/SHAPES-VERSION-GEN ${top_srcdir}
-include SHAPES-VERSION-FILE

bin_PROGRAMS = shapes
dist_noinst_SCRIPTS = SHAPES-VERSION-GEN

dist-hook : shapes${EXEEXT} SHAPES-VERSION-FILE
	@${srcdir}/SHAPES-VERSION-GEN ${top_srcdir} && cp SHAPES-VERSION-FILE ${distdir}/version

BUILT_SOURCES = shapesparser.h

noinst_HEADERS = afmscanner.h angleselect.h annotations.h \
                 astclass.h astexpr.h astexprs.h astflow.h \
                 astfun.h ast.h astidentifier.h astinsertion.h \
                 astobsolete.h astvalues.h astvar.h shapesparser.h \
                 basicsimplex.h bezier.h characterencoding.h \
                 charconverters.h charptrless.h check.h \
                 classtree1.h classtree2.h classtreemacros.h classtypes.h \
                 concretecolors.h constructorrepresentation.h \
                 consts.h containertypes.h continuations.h debuglog.h \
                 dispatch_case.h dispatch_none.h dispatch_void.h \
                 dispatch_vtbl.h drawabletypes.h dynamicenvironment.h \
                 elementarycoords.h elementarylength.h elementarytypes.h \
                 environment.h errorhandlers.h facettypes.h \
                 FontMetrics_decls.h fontmetrics.h fonttypes.h \
                 functiontypes.h globals.h glyphlist.h hottypes.h \
                 lighttypes.h methodid.h multipage.h operatorside.h \
                 operatorside_none.h pagecontentstates.h pathtypes.h \
                 pdffunctiontypes.h pdfscanner.h pdfstructure.h pdfversion.h \
                 shadingtypes.h Shapes_Ast_decls.h \
                 Shapes_Computation_decls.h Shapes_Concrete_decls.h \
                 shapescore.h Shapes_Exceptions_decls.h shapesexceptions.h \
                 Shapes_Helpers_decls.h Shapes_Kernel_decls.h \
                 Shapes_Lang_decls.h shapesscanner.h shapestypes.h \
                 shapesvalue.h SimplePDF_decls.h simplepdfi.h pdfimport.h \
                 simplepdfo.h sourcelocation.h specialunits.h statetypes.h \
                 strrefdup.h tagtypes.h texlabelmanager.h texscanner.h texttypes.h \
                 thunkimpl.h timetypes.h trianglefunctions.h upsamplers.h \
                 value.h yyltype.h zbufinternals.h utf8tools.h \
                 ../tools/include/autoonoff.h ../tools/include/isnan.h \
                 ../tools/include/physical.h ../tools/include/nullptr.h \
                 ../tools/include/ptrowner.h ../tools/include/refcount.h

shapes_SOURCES = shapesyylex.ll \
                pdfstructure.cc pdfscanner.cc pdfyylex.ll simplepdfo.cc pdfversion.cc \
		simplepdfi.cc pdfimport.cc consts.cc \
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
		pathslider2D.cc pathslider3D.cc drawabletypes.cc singlesidedpolygons.cc shadingtypes.cc \
		functiontypes.cc pdffunctiontypes.cc \
		lighttypes.cc containertypes.cc timetypes.cc hottypes.cc \
		multipage.cc annotations.cc tagtypes.cc shapesvalue.cc \
		shapesscanner.cc sourcelocation.cc shapesexceptions.cc strrefdup.cc \
		shapesparser.yy environment.cc dynamicenvironment.cc registerclasses.cc \
		shapescore.cc coreast.cc coreelem.cc coreconstruct.cc coreannotation.cc \
	        corepoint.cc corepath.cc coremisc.cc coredraw.cc corestate.cc \
		angleselect.cc texlabelmanager.cc texscanner.cc texyylex.ll specialunits.cc trianglefunctions.cc basicsimplex.cc \
		computeelementarypath.cc \
		zbuftriangle.cc zbufline.cc zbufto2D.cc zbufinternals.cc zsorterto2D.cc \
		ast.cc astfun.cc astvalues.cc astvar.cc astexpr.cc \
		astexpr_unary.cc astexpr_arithmetic.cc astexpr_relational.cc \
		coreoperators.cc astflow.cc \
		classtypes.cc astclass.cc errorhandlers.cc constructorrepresentation.cc charconverters.cc \
		fontmetrics.cc afmscanner.cc afmyylex.ll fonttypes.cc corefont.cc glyphlist.cc \
		characterencoding.cc \
		debuglog.cc globals.cc main.cc

dist_noinst_DATA = version.cc

shapesparser.h : shapesparser.cc shapesparser.yy

classtree2.h : classtree1.h
	cat $< | sed -e 's/CLASSTREE1/CLASSTREE2/' > $@

version.o : version.cc SHAPES-VERSION-FILE
	${COMPILE} -D'VERSION_DATE="${VERSION_DATE}"' \
	           -D'VERSION_NUMBER="${VERSION_NUMBER}"' -c $< -o $@

main.o : main.cc
	${COMPILE} -D'HTMLDIR="${htmldir}"' -c $< -o $@
