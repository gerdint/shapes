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
partsdir=${docdir}/parts
extensionsdir=${partsdir}/extensions
formatsdir=${partsdir}/formats
# Note: stylesdir must not be relative, or cyclic dependencies will result!
stylesdir=${top_srcdir}/doc/styles
toolsdir=${docdir}/tools

POSTPROC_HTML=${top_builddir}/doc/tools/postproc/postproc-html

SAXON = @SAXON@

SHAPES = $(top_builddir)/source/shapes
SHAPESFLAGS = --resources=no \
              --fontmetricspath ${top_srcdir}/resources/fontmetrics \
              --needpath ${top_srcdir}/resources/extensions \
              --tp=no \
              --tmpdir "/tmp/shapes.${USER}" --tmp*=yes


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

html_FILES = algo-tol.html bindings.html dynamic.html install.html \
             man.html state-types.html states.html structure.html syntax.html types.html \
             tutorial.html index.html \
             $(addprefix extensions/, arrowheads.html blockdraw_wfo.html \
               bondgraph.html centering.html circle.html conssupport.html \
               linkpaths.html pathmapping.html shiftoff.html index.html )

nobase_dist_html_DATA = ${html_FILES} shapes.css

nobase_dist_noinst_DATA = ${extensionsdir}/formats/html.xsl \
                   $(addprefix ${formatsdir}/, \
                      examplecode-html.xsl html.xsl language-elements-html.xsl \
                      man.xsl plain-book-html.xsl) \
	                 $(foreach part,\
                     $(addprefix ${partsdir}/, \
	                     algo-tol bindings dynamic man states state-types \
                         structure syntax tutorial types index),\
                     $(part)/formats/html.xsl $(wildcard $(part)/formats/*-html.xsl)) \
                   $(patsubst ${srcdir}/%,%,\
	                   $(foreach part,\
                       $(addprefix ${srcdir}/${partsdir}/, \
	                       algo-tol bindings dynamic man states state-types \
                           structure syntax tutorial types index),\
                       $(part)/formats/html.xsl $(wildcard $(part)/formats/*-html.xsl))) \
                   $(SHEXT_FILES)


# Temporary solution for putting the SF logo on each page:
ADD-SF: $(html_FILES)
	-for i in $(html_FILES); do \
	  echo "Putting SF logo in " $$i; \
	  ex -sC $$i < insert-SF-logo.ex; \
	done

extensions/%.xml : ${extensionsdir}/%.sxml ${DEPDIR}/extensions/%.ssidep
	test -d  $(@D) || ${mkdir_p} $(@D)
	if ${SSI} --in $< ${SSIFLAGS_EXTENSIONS} > extensions/$*.Txml; \
	then \
		mv extensions/$*.Txml $@; \
	else \
		${RM} extensions/$*.Txml; \
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

%.css : ${stylesdir}/html/%.css
	test -d  $(@D) || ${mkdir_p} $(@D)
	${INSTALL_DATA} $< $@

example/%.shape : ${top_srcdir}/examples/%.shape
	test -d  $(@D) || ${mkdir_p} $(@D)
	${INSTALL_DATA} $< $@

example/%.shext : ${top_srcdir}/examples/%.shext
	test -d  $(@D) || ${mkdir_p} $(@D)
	${INSTALL_DATA} $< $@

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

include ../Makefile.common

html-local : ${nobase_dist_html_DATA} ${nobase_dist_noinst_DATA} FORCE
dist-hook : all-local
all-local : html-local
	${MAKE} html-local

FORCE :
