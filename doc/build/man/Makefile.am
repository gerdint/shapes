SAXON = @SAXON@

-include ${top_builddir}/source/SHAPES-VERSION-FILE

DATE_PATTERN='[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}'
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell echo $(VERSION_DATE) | \
	             sed -e 's!.*\('${DATE_PATTERN}'\).*!\1!' | \
               grep -e ${DATE_PATTERN} 2> /dev/null || echo --- ) \
               -dHTML_DOC_DIR=$(htmldir)

SSIFLAGS=-dBASE=$(HREF_BASE) -dEXAMPLES=example/ $(MORE_SSIFLAGS)
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

${DEPDIR}/%.xml : ${docdir}/%/index.sxml ${DEPDIR}/%.ssidep ${docdir}/formats/html.xsl
	if ${SSI} --in $< ${SSIFLAGS} > ${DEPDIR}/$*.Txml; \
	then \
		mv ${DEPDIR}/$*.Txml $@; \
	else \
		${RM} ${DEPDIR}/$*.Txml; \
	fi

-include ${DEPDIR}/man.ssidep

${DEPDIR}/%.ssidep : ${docdir}/%/index.sxml
	install -d ${DEPDIR}
	if $(SSI) --in $< $(SSIFLAGS) --head $< --deps > ${DEPDIR}/$*.Tssidep; \
	then \
		mv ${DEPDIR}/$*.Tssidep $@; \
	else \
		${RM} ${DEPDIR}/$*.Tssidep; \
		exit 1; \
	fi


%.1 : ${DEPDIR}/%.xml ${docdir}/%/formats/man.xsl ${docdir}/formats/man.xsl
	$(SAXON) $< ${docdir}/$*/formats/man.xsl | \
	  ${top_builddir}/doc/tools/postproc/postproc-man > $@

.PRECIOUS : ${DEPDIR}/man.ssidep ${DEPDIR}/man.xml
