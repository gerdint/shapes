POSTPROCDIR=../../tools/postproc
SSIDIR=../../tools/ssi
PRODUCED_FORMATS_DIR=../../formats
PART_FORMATS_DIR=formats
EXDIR=../../../examples
EXOUTDIR=../../output

SSI=$(SSIDIR)/ssipp -dEXAMPLES=$(EXDIR) -dEXAMPLES_OUTPUT=$(EXOUTDIR)
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

all: $(PRODUCTS)

$(MAN1DIR)/shapes.1: tool-interface.sxml $(MANFORMAT) $(POSTPROCDIR)/man
	$(SAXON) $< $(MANFORMAT) | $(POSTPROCDIR)/man > $@

$(HTMLDIR)/man.html: tool-interface.sxml $(HTMLFORMAT) $(POSTPROCDIR)/html
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

.PRECIOUS: *.depend
%.depend: %.sxml $(wildcard *.sxml) $(SSI)
	cat $*.sxml | $(SSI) --head $*.sxml --deps > $@

-include $(wildcard *.depend)
