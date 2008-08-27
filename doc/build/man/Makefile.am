SAXON = @SAXON@

-include ${top_builddir}/source/SHAPES-VERSION-FILE

EXDIR=${top_srcdir}/examples
EXOUTDIR=${top_srcdir}/doc/output
DATE_PATTERN='[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}'
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell echo $(VERSION_DATE) | \
	             sed -e 's!.*\('${DATE_PATTERN}'\).*!\1!' | \
               grep -e ${DATE_PATTERN} 2> /dev/null || echo --- )

SSIFLAGS=--includebase ${top_srcdir}/doc/parts/man -dEXAMPLES=$(EXDIR) -dEXAMPLES_OUTPUT=$(EXOUTDIR) -dBASE=$(HREF_BASE) $(MORE_SSIFLAGS)
SSI=${top_builddir}/doc/tools/ssi/ssipp

CLEANFILES = man.1
DISTCLEANFILES = shapes.1 ${DEPDIR}/man.dep ${DEPDIR}/man.xml

dist_man1_MANS = shapes.1

docdir=${top_srcdir}/doc/parts

shapes.1 : man.1
	cp man.1 shapes.1

dist_noinst_DATA = ${docdir}/man/index.sxml ${docdir}/formats/html.xsl \
                   ${docdir}/formats/man.xsl ${docdir}/man/formats/man.xsl \
                   ${docdir}/algo-tol/tolerances.sxml

${DEPDIR}/%.xml : ${docdir}/%/index.sxml ${DEPDIR}/%.dep ${docdir}/formats/html.xsl
	if cat $< | ${SSI} ${SSIFLAGS} > ${DEPDIR}/$*.Txml; \
	then \
		mv ${DEPDIR}/$*.Txml $@; \
	else \
		${RM} ${DEPDIR}/$*.Txml; \
	fi

${DEPDIR}/%.dep : ${docdir}/%/index.sxml
	install -d ${DEPDIR}
	if cat $< | $(SSI) $(SSIFLAGS) --head $< --deps > ${DEPDIR}/$*.Tdep; \
	then \
		mv ${DEPDIR}/$*.Tdep $@; \
	else \
		${RM} ${DEPDIR}/$*.Tdep; \
	fi

-include ${DEPDIR}/man.dep

%.1 : ${DEPDIR}/%.xml ${docdir}/%/formats/man.xsl ${docdir}/formats/man.xsl
	$(SAXON) $< ${docdir}/$*/formats/man.xsl | \
	  ${top_builddir}/doc/tools/postproc/postproc-man > $@

.PRECIOUS : ${DEPDIR}/man.dep ${DEPDIR}/man.xml
