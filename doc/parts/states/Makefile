SSIDIR=../../tools/ssi
POSTPROCDIR=../../tools/postproc
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl

PRODUCTS = $(HTMLDIR)/states.html
DEPENDS = build/states.depend

all: $(DEPENDS) $(PRODUCTS)

$(HTMLDIR)/states.html: build/states.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/postproc-html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/postproc-html > $@

include ../Makefile
-include $(wildcard build/*.depend)
