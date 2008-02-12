POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl

PRODUCTS = $(HTMLDIR)/tutorial.html
DEPENDS = build/tutorial.depend

all: $(PRODUCTS)

$(HTMLDIR)/tutorial.html: build/tutorial.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/html > $@

include ../Makefile
-include $(wildcard build/*.depend)
