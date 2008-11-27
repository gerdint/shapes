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
stylesdir=${docdir}/styles
toolsdir=${docdir}/tools

POSTPROC_MAN=${top_builddir}/doc/tools/postproc/postproc-man

SAXON = @SAXON@

include ${top_builddir}/source/SHAPES-VERSION-FILE

DATE_PATTERN='[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}'
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell echo $(VERSION_DATE) | \
	             sed -e 's!.*\('${DATE_PATTERN}'\).*!\1!' | \
               grep -e ${DATE_PATTERN} 2> /dev/null || echo --- ) \
               -dHTML_DOC_DIR=$(htmldir)

SSIFLAGS=-dBASE=$(HREF_BASE) -dEXAMPLES=example/ $(MORE_SSIFLAGS)
SSI=${top_builddir}/doc/tools/ssi/ssipp

CLEANFILES = man.1
DISTCLEANFILES = shapes.1 ${DEPDIR}/man.ssidep man.xml

dist_man1_MANS = shapes.1

shapes.1 : man.1
	cp man.1 shapes.1

nobase_dist_noinst_DATA = \
                   ${formatsdir}/man.xsl $(wildcard ${formatsdir}/*-man.xsl) \
	                 $(foreach part,\
                     $(addprefix ${partsdir}/, \
	                     man),\
                     $(part)/formats/man.xsl $(wildcard $(part)/formats/*-man.xsl)) \
                   ${partsdir}/man/index.sxml \
                   ${partsdir}/algo-tol/tolerances.sxml

%.xml : ${partsdir}/%/index.sxml ${DEPDIR}/%.ssidep ${formatsdir}/html.xsl
	if ${SSI} --in $< ${SSIFLAGS} > $*.Txml; \
	then \
		mv $*.Txml $@; \
	else \
		${RM} $*.Txml; \
	fi

-include ${DEPDIR}/man.ssidep

${DEPDIR}/%.ssidep : ${partsdir}/%/index.sxml
	install -d ${DEPDIR}
	if $(SSI) --in $< $(SSIFLAGS) --head $< --deps > ${DEPDIR}/$*.Tssidep; \
	then \
		mv ${DEPDIR}/$*.Tssidep $@; \
	else \
		${RM} ${DEPDIR}/$*.Tssidep; \
		exit 1; \
	fi

%.1 : %.xml ${partsdir}/%/formats/man.xsl ${formatsdir}/man.xsl
	$(SAXON) $< $$(test -f ${partsdir}/$*/formats/man.xsl || \
	  echo ${srcdir}/ )${partsdir}/$*/formats/man.xsl | \
	  ${POSTPROC_MAN} > $@

.PRECIOUS : ${DEPDIR}/man.ssidep man.xml
