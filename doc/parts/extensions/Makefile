POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html/extensions
HREF_BASE=../
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl ../formats/plain-book-html.xsl
INDEX_HTMLFORMAT=../index/formats/html.xsl

JOBS = circle centering blockdraw_wfo

PRODUCTS = $(HTMLDIR) $(HTMLDIR)/index_.html $(patsubst %,$(HTMLDIR)/%.html,$(JOBS))
DEPENDS = build/index_.depend $(patsubst %,build/%.depend,$(JOBS))

all: $(DEPENDS) $(PRODUCTS)

$(HTMLDIR):
	mkdir $@

$(HTMLDIR)/%.html: build/%.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/html > $@

$(HTMLDIR)/index_.html: build/index_.xml $(INDEX_HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
	$(SAXON) $< $(INDEX_HTMLFORMAT) | $(POSTPROCDIR)/html > $@

include ../Makefile
-include $(wildcard build/*.depend)
