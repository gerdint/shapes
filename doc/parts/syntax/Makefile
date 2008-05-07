POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl ../formats/examplecode-html.xsl ../formats/plain-book-html.xsl

PRODUCTS = $(HTMLDIR)/syntax.html
DEPENDS = build/syntax.depend

all: $(DEPENDS) $(PRODUCTS)

$(HTMLDIR)/syntax.html: build/syntax.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/postproc-html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/postproc-html > $@

include ../Makefile
-include $(wildcard build/*.depend)
