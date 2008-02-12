## Please make sure that index is made last, since making the parts will ensure that meta data about file locations is generated.

all-parts:
	cd man; make
	cd syntax; make
	cd bindings; make
	cd states; make
	cd dynamic; make
	cd types; make
	cd state-types; make
	cd tutorial; make
	cd index; make

SSIFLAGS=-dEXAMPLES=$(EXDIR) -dEXAMPLES_OUTPUT=$(EXOUTDIR)
SSI=$(SSIDIR)/ssipp

$(POSTPROCDIR)/html: $(POSTPROCDIR)/html.l
	cd $(POSTPROCDIR); make
$(POSTPROCDIR)/man: $(POSTPROCDIR)/man.l
	cd $(POSTPROCDIR); make
$(SSI): $(SSIDIR)/main.cc $(SSIDIR)/ssiyylex.ll $(SSIDIR)/ssiscanner.h $(SSIDIR)/ssiscanner.cc
	cd $(SSIDIR); make

JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
ifneq ($(strip $(wildcard $(JAR1))),)
SAXON=java -jar $(JAR1)
else
SAXON=java -jar $(JAR2)
endif

build/%.xml: %.sxml build/%.depend $(SSI)
	cat $< | $(SSI) $(SSIFLAGS) > $@

.PHONY: clean
clean:
	-rm $(PRODUCTS)

.PRECIOUS: build/*.depend
build/%.depend: %.sxml $(wildcard *.sxml) $(SSI)
	cat $*.sxml | $(SSI) $(SSIFLAGS) --head build/$*.xml --deps > $@
