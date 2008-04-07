POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl
INDEX_HTMLFORMAT=../index/formats/html.xsl

JOBS = circle centering blockdraw_wfo

PRODUCTS =  $(HTMLDIR)/extensions.html $(patsubst %,$(HTMLDIR)/extensions-%.html,$(JOBS))
DEPENDS = build/index.depend $(patsubst %,build/%.depend,$(JOBS))

all: $(DEPENDS) $(PRODUCTS)

$(HTMLDIR)/extensions-%.html: build/%.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/html > $@

$(HTMLDIR)/extensions.html: build/index.xml $(INDEX_HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
	$(SAXON) $< $(INDEX_HTMLFORMAT) | $(POSTPROCDIR)/html > $@

include ../Makefile
-include $(wildcard build/*.depend)
