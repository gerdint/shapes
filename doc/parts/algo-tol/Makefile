POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl

MAINBASE=algo-tol
PRODUCTS = $(HTMLDIR)/$(MAINBASE).html
DEPENDS = build/$(MAINBASE).depend

all: $(DEPENDS) $(PRODUCTS)

$(HTMLDIR)/$(MAINBASE).html: build/$(MAINBASE).xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/html > $@

include ../Makefile
-include $(wildcard build/*.depend)
