POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl

MAINBASE=index
PRODUCTS = $(HTMLDIR)/algo-tol.html
DEPENDS = build/$(MAINBASE).depend

all: $(DEPENDS) $(PRODUCTS)

$(HTMLDIR)/algo-tol.html: build/$(MAINBASE).xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/postproc-html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/postproc-html > $@

include ../Makefile.incl
-include $(wildcard build/*.depend)
