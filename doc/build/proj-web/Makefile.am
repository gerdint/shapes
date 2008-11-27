# This file is part of Shapes.
#
# Shapes is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# Shapes is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright 2008 Gustaf Hendeby

docdir=../..
partsdir=${docdir}/parts/proj-web
formatsdir=${docdir}/parts/formats
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
SSIFLAGS=-dEXAMPLES=example/ -dBASE=doc/ -dBUILDDIR=^/ $(MORE_SSIFLAGS)
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell ${ECHO} $(VERSION_DATE) | \
	             ${SED} -e 's!.*\('${DATE_PATTERN}'\).*!\1!' | \
               grep -e ${DATE_PATTERN} 2> /dev/null || ${ECHO} --- ) \
               -dHTML_DOC_DIR=$(htmldir) -I${srcdir}

SHEXT_LIST=
SHEXT_FILES=$(addprefix example/, $(SHEXT_LIST))

html_FILES = index.html

nobase_dist_html_DATA = ${html_FILES} logo.jpg

# The common XSL files for transform to HTML is taken care of by ../html/Makefile.am .
nobase_dist_noinst_DATA = \
	                 $(foreach part,\
                     $(addprefix ${partsdir}/, \
	                     index),\
                     $(part)/formats/html.xsl $(wildcard $(part)/formats/*-html.xsl))

%.xml : ${partsdir}/%/index.sxml ${formatsdir}/html.xsl ${DEPDIR}/%.ssidep
	if ${SSI} --in $< ${SSIFLAGS} > $*.Txml; \
	then \
		mv $*.Txml $@; \
	else \
		${RM} $*.Txml; \
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

%.jpg : ${partsdir}/index/%.jpg
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
dist-hook :
all-local : html-local
	${MAKE} html-local

FORCE :
