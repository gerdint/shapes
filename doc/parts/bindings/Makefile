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

JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
ifneq ($(strip $(wildcard $(JAR1))),)
SAXON=java -jar $(JAR1)
else
SAXON=java -jar $(JAR2)
endif

PRODUCTS = $(HTMLDIR)/bindings.html
DEPENDS = build/bindings.depend

all: $(DEPENDS) $(PRODUCTS)

build/%.xml: %.sxml build/%.depend $(SSI)
	cat $< | $(SSI) $(SSIFLAGS) > $@

$(HTMLDIR)/bindings.html: build/bindings.xml $(HTMLFORMAT) $(HTML_MORE_FORMATS) $(POSTPROCDIR)/html
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
	cat $*.sxml | $(SSI) $(SSIFLAGS) --head build/$*.xml --deps > $@

-include $(wildcard build/*.depend)
