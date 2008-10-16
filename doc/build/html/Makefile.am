docdir=../..
partsdir=${docdir}/parts
extensionsdir=${partsdir}/extensions
formatsdir=${partsdir}/formats
stylesdir=${docdir}/styles
toolsdir=${docdir}/tools

POSTPROC_HTML=${top_builddir}/doc/tools/postproc/postproc-html

SAXON = @SAXON@

SHAPES = $(top_builddir)/source/shapes
SHAPESFLAGS = --resources=no \
              --fontmetricspath ${top_srcdir}/resources/fontmetrics \
              --needpath ${top_srcdir}/resources/extensions \
              --tp=no \
              --tmpdir /tmp


include ${top_builddir}/source/SHAPES-VERSION-FILE

DATE_PATTERN='[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}'
SSI=${top_builddir}/doc/tools/ssi/ssipp
# Setting BUILDDIR=^ means that paths will be relative to the current working
# directory.
SSIFLAGS=-dEXAMPLES=example/ -dBASE= -dBUILDDIR=^/ $(MORE_SSIFLAGS)
SSIFLAGS_EXTENSIONS=-dEXAMPLES=example/ -dBASE=../ -dBUILDDIR=^/ \
                     $(MORE_SSIFLAGS) -I${srcdir}
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell ${ECHO} $(VERSION_DATE) | \
	             ${SED} -e 's!.*\('${DATE_PATTERN}'\).*!\1!' | \
               grep -e ${DATE_PATTERN} 2> /dev/null || ${ECHO} --- ) \
               -dHTML_DOC_DIR=$(htmldir) -I${srcdir}

SHEXT_LIST=doc/blockdraw-tutorial-tut-prefs.shext
SHEXT_FILES=$(addprefix example/, $(SHEXT_LIST))

html_FILES = algo-tol.html bindings.html dynamic.html man.html \
            state-types.html states.html syntax.html types.html \
            tutorial.html index.html \
            $(addprefix extensions/, arrowheads.html blockdraw_wfo.html \
              bondgraph.html centering.html circle.html conssupport.html \
              linkpaths.html pathmapping.html shiftoff.html index.html )

nobase_dist_html_DATA = ${html_FILES} shapes.css

nobase_dist_noinst_DATA = ${extensionsdir}/formats/html.xsl \
                   $(addprefix ${formatsdir}/, \
                      examplecode-html.xsl html.xsl language-elements-html.xsl \
                      man.xsl plain-book-html.xsl) \
                   $(addprefix ${partsdir}/, \
                      $(addsuffix /formats/html.xsl, \
                         algo-tol bindings dynamic man states state-types \
                         syntax tutorial types index)) $(SHEXT_FILES)

extensions/%.xml : ${extensionsdir}/%.sxml ${DEPDIR}/extensions/%.ssidep
	test -d  $(@D) || ${mkdir_p} $(@D)
	if ${SSI} --in $< ${SSIFLAGS_EXTENSIONS} > extensions/$*.Txml; \
	then \
		mv extensions/$*.Txml $@; \
	else \
		${RM} extensions/$*.Txml; \
		exit 1; \
	fi

%.xml : ${partsdir}/%/index.sxml ${formatsdir}/html.xsl ${DEPDIR}/%.ssidep
	if ${SSI} --in $< ${SSIFLAGS} > $*.Txml; \
	then \
		mv $*.Txml $@; \
	else \
		${RM} $*.Txml; \
		exit 1; \
	fi

${DEPDIR}/extensions/%.ssidep : ${extensionsdir}/%.sxml Makefile
	test -d  $(@D) || ${mkdir_p} ${@D}
	if $(SSI) --in $< $(SSIFLAGS_EXTENSIONS) --head extensions/$*.xml \
	  --deps > ${DEPDIR}/extensions/$*.Tssidep; \
	then \
		${SED} -e 's!^.*:!nobase_dist_noinst_DATA += $< !' \
		  ${DEPDIR}/extensions/$*.Tssidep > ${DEPDIR}/extensions/$*.dist ; \
	  ${SED} -e 's!^!$@ !' ${DEPDIR}/extensions/$*.Tssidep > $@ ; \
		mv ${DEPDIR}/extensions/$*.Tssidep $@; \
	else \
		${RM} ${DEPDIR}/extensions/$*.Tssidep; \
		exit 1; \
	fi

${DEPDIR}/%.ssidep : ${partsdir}/%/index.sxml Makefile
	test -d  $(@D) || ${mkdir_p} ${@D}
	if $(SSI) --in $< $(SSIFLAGS) --head $*.xml --deps > \
	  ${DEPDIR}/$*.Tssidep; \
	then \
		${SED} -e 's!^.*:!nobase_dist_noinst_DATA += $< !' \
		  ${DEPDIR}/$*.Tssidep > ${DEPDIR}/$*.dist ; \
	  ${SED} -e 's!^!$@ !' ${DEPDIR}/$*.Tssidep > $@ ; \
		mv ${DEPDIR}/$*.Tssidep $@; \
	else \
		${RM} ${DEPDIR}/$*.Tssidep; \
		exit 1; \
	fi

example/%.shape : ${top_srcdir}/examples/%.shape
	test -d  $(@D) || ${mkdir_p} $(@D)
	${INSTALL_DATA} $< $@

example/%.shext : ${top_srcdir}/examples/%.shext
	test -d  $(@D) || ${mkdir_p} $(@D)
	${INSTALL_DATA} $< $@

%.css : ${stylesdir}/html/%.css
	test -d  $(@D) || ${mkdir_p} $(@D)
	${INSTALL_DATA} $< $@

%.pdf %.stdout: %.shape $(SHEXT_FILES)
	test -d  $(@D) || ${mkdir_p} $(@D)
	$(SHAPES) $(SHAPESFLAGS) --in $< --out $*.pdf > $*.stdout

%_3.pdf %.stdout: %.shape $(SHEXT_FILES)
	test -d  $(@D) || ${mkdir_p} $(@D)
	$(SHAPES) $(SHAPESFLAGS) -vs1.3 --in $< --out $*_3.pdf > $*.stdout

# Some stupid versions of convert exit with a non-zero value even though the conversion was a success...
%_70%.jpg: %.pdf
	-convert -density 100 -resize $(shell ${ECHO} $@ | ${SED} -e 's!.*_\([^._]*\).jpg!\1!') $< $@

%_y_big.jpg: %.pdf
	-convert -density 150 -resize x150 $< $@

%_y_small.jpg: %.pdf
	-convert -density 50 -resize x50 $< $@

%_x_big.jpg: %.pdf
	-convert -density 150 -resize 150x $< $@

%_x_small.jpg: %.pdf
	-convert -density 50 -resize 50x $< $@

# WARNING!
# Dependencies such as extensions/../dynamic.html will cause
# dynamic.html to be built as if it was the extension ../dynamic.html,
# which will produce the wrong result.  Therefore, we make an
# attempt at removing collapsable parts from the dependencies.
extensions/%.html : extensions/%.xml ${extensionsdir}/formats/html.xsl \
                    ${formatsdir}/html.xsl
	test -d  ${@D} || ${mkdir_p} ${@D} ; \
	( $(SAXON) $< $$( test -f ${extensionsdir}/formats/html.xsl || \
	  ${ECHO} '${srcdir}'/ )${extensionsdir}/formats/html.xsl || exit 1 ) | \
	  ${POSTPROC_HTML} > $@
	if $(SAXON) $@ $$( test -f ${toolsdir}/xhtml-deps/filter.xsl || \
	  ${ECHO} '${srcdir}'/)${toolsdir}/xhtml-deps/filter.xsl  > \
	   ${DEPDIR}/extensions/$*.Thtmldep ; \
	then \
		${ECHO} "nobase_dist_html_DATA += $$( ${SED} -e 's!\([^ ]\{1,\}\)!extensions/\1!g' < ${DEPDIR}/extensions/$*.Thtmldep | ${SED} -e 's![^/ ]\{1,\}/\.\./!!g' | tr ' ' '\n' | sort -u | tr '\n' ' ' )" > ${DEPDIR}/extensions/$*.htmldep ; \
		${RM} ${DEPDIR}/extensions/$*.Thtmldep ; \
	else \
		${RM} ${DEPDIR}/extensions/$*.Thtmldep ; \
		exit 1; \
	fi

%.html : %.xml ${partsdir}/%/formats/html.xsl ${formatsdir}/html.xsl
	test -d  ${@D} || ${mkdir_p} ${@D} ; \
	( $(SAXON) $< $$( test -f ${partsdir}/$*/formats/html.xsl || \
	  ${ECHO} '${srcdir}'/ )${partsdir}/$*/formats/html.xsl || exit 1 ) | \
	  ${POSTPROC_HTML} > $*.html
	if $(SAXON) $@ $$( test -f ${toolsdir}/xhtml-deps/filter.xsl || \
	  ${ECHO} '${srcdir}'/)${toolsdir}/xhtml-deps/filter.xsl  > \
	  ${DEPDIR}/$*.Thtmldep ; \
	then \
		${ECHO} "nobase_dist_html_DATA += $$( tr ' ' '\n' < ${DEPDIR}/$*.Thtmldep | sort -u | tr '\n' ' ' )" > ${DEPDIR}/$*.htmldep ; \
		${RM} ${DEPDIR}/$*.Thtmldep ; \
	else \
		${RM} ${DEPDIR}/$*.Thtmldep ; \
		exit 1; \
	fi

%.htmldep : %.ssidep
	@touch $@

.PRECIOUS : $(addprefix ${DEPDIR}/, $(html_FILES:.html=.ssidep) \
                                    $(html_FILES:.html=.htmldep) \
                                    $(html_FILES:.html=.dist)) \
            $(html_FILES:.html=.xml)

clean-local :
	find . \( -name "*~" -o -name "*.html" -o -name "*.xml" \) -exec ${RM} {} \;
	find ${DEPDIR} -type f -exec ${RM} {} \;

distclean-local :
	-find . -type f -exec ${RM} {} \;

include $(addprefix ${DEPDIR}/, $(html_FILES:.html=.ssidep))
-include $(addprefix ${DEPDIR}/, $(html_FILES:.html=.htmldep))
-include $(addprefix ${DEPDIR}/, $(html_FILES:.html=.dist))

html-local : ${nobase_dist_html_DATA} ${nobase_dist_noinst_DATA} FORCE
dist-hook : all-local
all-local : html-local
	${MAKE} html-local

FORCE :