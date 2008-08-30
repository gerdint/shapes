POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output
include ../../../source/SHAPES-VERSION-FILE
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell echo "$(VERSION_DATE)" | sed -e 's/\([^ ]*\).*/\1/' )

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HREF_BASE=.
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl

PRODUCTS = $(HTMLDIR)/index.html
DEPENDS = build/index.depend

all: $(DEPENDS) $(PRODUCTS)

build/%.xml: %.sxml build/%.depend $(SSI)
	$(SSI) --in $< $(SSIFLAGS) > $@

$(HTMLDIR)/index.html: build/index.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/postproc-html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/postproc-html > $@

.PRECIOUS: build/*.depend
build/%.depend: %.sxml $(wildcard *.sxml) $(SSI)
	$(SSI) --in $*.sxml $(SSIFLAGS) --head build/$*.xml --deps > $@

include ../Makefile.incl
-include $(wildcard build/*.depend)
