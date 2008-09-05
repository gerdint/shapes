SAXON = @SAXON@

-include ${top_builddir}/source/SHAPES-VERSION-FILE

EXDIR=${top_srcdir}/examples
EXOUTDIR=${top_srcdir}/doc/output
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell date -d"$(VERSION_DATE)" +%F || echo ---)

SSIFLAGS=--includebase ${top_srcdir}/doc/parts/man -dEXAMPLES=$(EXDIR) -dEXAMPLES_OUTPUT=$(EXOUTDIR) -dBASE=$(HREF_BASE) $(MORE_SSIFLAGS)
SSI=${top_builddir}/doc/tools/ssi/ssipp

CLEANFILES = man.1
DISTCLEANFILES = shapes.1 .deps/man.dep .deps/man.xml

dist_man1_MANS = shapes.1

docdir=${top_srcdir}/doc/parts

shapes.1 : man.1
	cp man.1 shapes.1

dist_noinst_DATA = ${docdir}/man/index.sxml ${docdir}/formats/html.xsl \
                   ${docdir}/formats/man.xsl ${docdir}/man/formats/man.xsl \
                   ${docdir}/algo-tol/tolerances.sxml

.deps/%.xml : ${docdir}/%/index.sxml .deps/%.dep ${docdir}/formats/html.xsl
	if cat $< | ${SSI} ${SSIFLAGS} > .deps/$*.Txml; \
	then \
		mv .deps/$*.Txml $@; \
	else \
		${RM} .deps/$*.Txml; \
	fi

.deps/%.dep : ${docdir}/%/index.sxml
	install -d .deps
	if cat $< | $(SSI) $(SSIFLAGS) --head $< --deps > .deps/$*.Tdep; \
	then \
		mv .deps/$*.Tdep $@; \
	else \
		${RM} .deps/$*.Tdep; \
	fi

-include .deps/man.dep

%.1 : .deps/%.xml ${docdir}/%/formats/man.xsl ${docdir}/formats/man.xsl
	$(SAXON) $< ${docdir}/$*/formats/man.xsl | \
	  ${top_builddir}/doc/tools/postproc/postproc-man > $@

.PRECIOUS : .deps/man.dep .deps/man.xml