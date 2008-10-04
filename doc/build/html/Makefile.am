SAXON = @SAXON@
SHAPES = $(top_builddir)/source/shapes
SHAPESFLAGS = --resources=no \
              --fontmetricspath ${top_srcdir}/resources/fontmetrics \
              --needpath ${top_srcdir}/resources/extensions \
              --tp=no \
              --tmpdir /tmp


include ${top_builddir}/source/SHAPES-VERSION-FILE

DATE_PATTERN='[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}'
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell echo $(VERSION_DATE) | \
	             sed -e 's!.*\('${DATE_PATTERN}'\).*!\1!' | \
               grep -e ${DATE_PATTERN} 2> /dev/null || echo --- ) \
               -dHTML_DOC_DIR=$(htmldir) -I${srcdir}

# Setting BUILDDIR=^ means that paths will be relative to the current working directory.
SSIFLAGS=-dEXAMPLES=example/ -dBASE= -dBUILDDIR=^/ $(MORE_SSIFLAGS)
SSIFLAGS_EXTENSIONS=-dEXAMPLES=example/ -dBASE=../ -dBUILDDIR=^/ $(MORE_SSIFLAGS) -I${srcdir}
SSI=${top_builddir}/doc/tools/ssi/ssipp

SHEXT_LIST=doc/blockdraw-tutorial-tut-prefs.shext

html_DATA = man.html tutorial.html syntax.html algo-tol.html bindings.html dynamic.html states.html state-types.html types.html index.html \
extensions/index.html extensions/blockdraw_wfo.html  extensions/centering.html  extensions/circle.html \
shapes.css

docdir=${top_srcdir}/doc/parts

dist_noinst_DATA = ${docdir}/man/index.sxml ${docdir}/formats/html.xsl \
                   ${docdir}/formats/man.xsl ${docdir}/man/formats/man.xsl \
                   ${docdir}/algo-tol/tolerances.sxml

SHEXT_FILES=$(patsubst %,example/%,$(SHEXT_LIST))
html-INSTALL: $(SHEXT_FILES)

extensions/%.xml : ${docdir}/extensions/%.sxml ${DEPDIR}/extensions/%.ssidep
	test -d  $(@D) || ${mkdir_p} $(@D)
	if ${SSI} --in $< ${SSIFLAGS_EXTENSIONS} > extensions/$*.Txml; \
	then \
		mv extensions/$*.Txml $@; \
	else \
		${RM} extensions/$*.Txml; \
		exit 1; \
	fi

%.xml : ${docdir}/%/index.sxml ${DEPDIR}/%.ssidep ${docdir}/formats/html.xsl
	if ${SSI} --in $< ${SSIFLAGS} > $*.Txml; \
	then \
		mv $*.Txml $@; \
	else \
		${RM} $*.Txml; \
		exit 1; \
	fi

${DEPDIR}/extensions/%.ssidep : ${docdir}/extensions/%.sxml
	test -d  $(@D) || ${mkdir_p} ${@D}
	if $(SSI) --in $< $(SSIFLAGS_EXTENSIONS) --head extensions/$*.xml --deps > ${DEPDIR}/extensions/$*.Tssidep; \
	then \
		mv ${DEPDIR}/extensions/$*.Tssidep $@; \
	else \
		${RM} ${DEPDIR}/extensions/$*.Tssidep; \
		exit 1; \
	fi

${DEPDIR}/%.ssidep : ${docdir}/%/index.sxml
	test -d  $(@D) || ${mkdir_p} ${@D}
	if $(SSI) --in $< $(SSIFLAGS) --head $*.xml --deps > ${DEPDIR}/$*.Tssidep; \
	then \
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

%.css : ${top_srcdir}/doc/styles/html/%.css
	test -d  $(@D) || ${mkdir_p} $(@D)
	${INSTALL_DATA} $< $@

%.pdf %.stdout: %.shape $(SHEXT_FILES)
	test -d  $(@D) || ${mkdir_p} $(@D)
	$(SHAPES) $(SHAPESFLAGS) --in $(test -f $< || echo '${srcdir}/')$< --out $*.pdf > $*.stdout

%_3.pdf %.stdout: %.shape $(SHEXT_FILES)
	test -d  $(@D) || ${mkdir_p} $(@D)
	$(SHAPES) $(SHAPESFLAGS) -vs1.3 --in $(test -f $< || echo '${srcdir}/')$< --out $*_3.pdf > $*.stdout

# Some stupid versions of convert exit with a non-zero value even though the conversion was a success...
%_70%.jpg: %.pdf
	-convert -density 100 -resize $(shell echo $@ | sed -e 's!.*_\([^._]*\).jpg!\1!') $< $@

%_y_big.jpg: %.pdf
	-convert -density 150 -resize x150 $< $@

%_y_small.jpg: %.pdf
	-convert -density 50 -resize x50 $< $@

%_x_big.jpg: %.pdf
	-convert -density 150 -resize 150x $< $@

%_x_small.jpg: %.pdf
	-convert -density 50 -resize 50x $< $@

# Warning!
# Dependencies such as extensions/../dynamic.html will cause dynamic.html to be built as if it was the extension ../dynamic.html ,
# which will produce the wrong result.
# Therefore, we make an attempt at removing collapsable parts from the dependencies.
extensions/%.html : extensions/%.xml ${docdir}/extensions/formats/html.xsl ${docdir}/formats/html.xsl
	( $(SAXON) $< ${docdir}/extensions/formats/html.xsl  || exit 1 ) | \
	  ${top_builddir}/doc/tools/postproc/postproc-html > $@
	if $(SAXON) $@ ${top_srcdir}/doc/tools/xhtml-deps/filter.xsl > ${DEPDIR}/extensions/$*.Thtmldep ; \
	then \
		echo "html-BUILD html-INSTALL : $$( sed -e 's!\([^ ]\{1,\}\)!extensions/\1!g' < ${DEPDIR}/extensions/$*.Thtmldep | sed -e 's![^/ ]\{1,\}/\.\./!!g' | tr ' ' '\n' | sort -u | tr '\n' ' ' )" > ${DEPDIR}/extensions/$*.htmldep ; \
		${RM} ${DEPDIR}/extensions/$*.Thtmldep ; \
	else \
		${RM} ${DEPDIR}/extensions/$*.Thtmldep ; \
		exit 1; \
	fi

%.html : %.xml ${docdir}/%/formats/html.xsl ${docdir}/formats/html.xsl
	( $(SAXON) $< ${docdir}/$*/formats/html.xsl || exit 1 ) | \
	  ${top_builddir}/doc/tools/postproc/postproc-html > $*.html
	if $(SAXON) $@ ${top_srcdir}/doc/tools/xhtml-deps/filter.xsl > ${DEPDIR}/$*.Thtmldep ; \
	then \
		echo "html-BUILD html-INSTALL : $$( tr ' ' '\n' < ${DEPDIR}/$*.Thtmldep | sort -u | tr '\n' ' ' )" > ${DEPDIR}/$*.htmldep ; \
		${RM} ${DEPDIR}/$*.Thtmldep ; \
	else \
		${RM} ${DEPDIR}/$*.Thtmldep ; \
		exit 1; \
	fi

%.htmldep : %.ssidep
	@touch $@

.PRECIOUS : $(addprefix ${DEPDIR}/, $(html_DATA:.html=.ssidep)) $(html_DATA:.html=.xml)
.PHONY : html-tmp

clean-local :
	${RM} -r *.xml *.html .deps extensions example \#*

include $(addprefix ${DEPDIR}/, $(html_DATA:.html=.ssidep))
-include $(addprefix ${DEPDIR}/, $(html_DATA:.html=.htmldep))

all-local : html-local

html-local : ${html_DATA} shapes.css
	${MAKE} ${AM_MAKEFLAGS} html-BUILD

install-data-local : html-local
	${MAKE} ${AM_MAKEFLAGS} html-INSTALL

html-INSTALL : ${html_DATA} shapes.css
	for f in $^; \
	do \
		dir="${htmldir}/$$( dirname $$f )" ; \
		test -z  $$dir || ${mkdir_p} $(DESTDIR)$$dir ; \
		${INSTALL_DATA} $$f $(DESTDIR)$$dir ; \
	done
