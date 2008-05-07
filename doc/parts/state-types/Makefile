POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output
SSIFLAGS=-dEXAMPLES=$(EXDIR) -dEXAMPLES_OUTPUT=$(EXOUTDIR)

SSI=$(SSIDIR)/ssipp
HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl

PRODUCTS = $(HTMLDIR)/state-types.html
DEPENDS = build/index.depend

all: $(DEPENDS) $(PRODUCTS)

$(HTMLDIR)/state-types.html: build/index.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/postproc-html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/postproc-html > $@

include ../Makefile
-include $(wildcard build/*.depend)
