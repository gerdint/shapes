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
MAN1DIR=$(PRODUCED_FORMATS_DIR)/man/man1
MANFORMAT=$(PART_FORMATS_DIR)/man.xsl

JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
ifneq ($(strip $(wildcard $(JAR1))),)
SAXON=java -jar $(JAR1)
else
SAXON=java -jar $(JAR2)
endif

PRODUCTS = $(MAN1DIR)/shapes.1 $(HTMLDIR)/man.html
DEPENDS = build/tool-interface.depend

all: $(DEPENDS) $(PRODUCTS)

build/%.xml: %.sxml build/%.depend $(SSI)
	cat $< | $(SSI) $(SSIFLAGS) > $@

$(MAN1DIR)/shapes.1: build/tool-interface.xml $(MANFORMAT) $(POSTPROCDIR)/man
	$(SAXON) $< $(MANFORMAT) | $(POSTPROCDIR)/man > $@

$(HTMLDIR)/man.html: build/tool-interface.xml $(HTMLFORMAT) $(POSTPROCDIR)/html
	$(SAXON) $< $(HTMLFORMAT) | $(POSTPROCDIR)/html > $@


$(POSTPROCDIR)/html: $(POSTPROCDIR)/html.l
	cd $(POSTPROCDIR); make
$(POSTPROCDIR)/man: $(POSTPROCDIR)/man.l
	cd $(POSTPROCDIR); make
$(SSI): $(SSIDIR)/main.cc $(SSIDIR)/ssiyylex.ll $(SSIDIR)/ssiscanner.h $(SSIDIR)/ssiscanner.cc
	cd $(SSIDIR); make

.PHONY: clean
clean:
	-rm $(PRODUCTS)

.PRECIOUS: build/*.depend
build/%.depend: %.sxml $(wildcard *.sxml) $(SSI)
	cat $*.sxml | $(SSI) $(SSIFLAGS) --head $*.sxml --deps > $@

-include $(wildcard build/*.depend)
