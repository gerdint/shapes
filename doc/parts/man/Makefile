POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output
include ../../../source/SHAPES-VERSION-FILE
MORE_SSIFLAGS=-dMODIFICATION_DATE=$(shell echo "$(VERSION_DATE)" | sed -e 's/\([^ ]*\).*/\1/' )

SSI=$(SSIDIR)/ssipp
HTMLDIR=$(PRODUCED_FORMATS_DIR)/html
HTMLFORMAT=$(PART_FORMATS_DIR)/html.xsl
HTML_MORE_FORMATS=../formats/html.xsl
MAN1DIR=$(PRODUCED_FORMATS_DIR)/man/man1
MANFORMAT=$(PART_FORMATS_DIR)/man.xsl

PRODUCTS = $(MAN1DIR)/shapes.1 $(HTMLDIR)/man.html
DEPENDS = build/tool-interface.depend

all: $(DEPENDS) $(PRODUCTS)

$(MAN1DIR)/shapes.1: build/tool-interface.xml $(MANFORMAT) ../formats/man.xsl $(POSTPROCDIR)/man
	$(SAXON) $< $(MANFORMAT) | $(POSTPROCDIR)/man > $@

$(HTMLDIR)/man.html: build/tool-interface.xml $(HTMLFORMAT) ../formats/html.xsl $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/html > $@

include ../Makefile
-include $(wildcard build/*.depend)
