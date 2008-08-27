POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl ../formats/examplecode-html.xsl ../formats/plain-book-html.xsl

PRODUCTS = $(HTMLDIR)/tutorial.html
DEPENDS = build/index.depend

all: $(PRODUCTS)

$(HTMLDIR)/tutorial.html: build/index.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/postproc-html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/postproc-html > $@

include ../Makefile.incl
-include $(wildcard build/*.depend)
