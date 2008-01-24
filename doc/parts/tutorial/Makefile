JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
POSTPROCDIR=../postproc
SSIDIR=../ssi
SSI=$(SSIDIR)/ssipp

ifneq ($(strip $(wildcard $(JAR1))),)
SAXON=java -jar $(JAR1)
else
SAXON=java -jar $(JAR2)
endif

PRODUCTS = html/index.html
TUT_DEPS = tutorial.sxml chap-hello.sxml

all: $(PRODUCTS)

# This is how I used to apply the transform before I needed XPATH 2 functions:
#	@xsltproc man.xsl mini.sxml | sed '1,1d' > $@

%.xml: %.sxml %.depend $(SSI)
	cat $< | $(SSI) > $@

html/index.html: tutorial.xml ../lang/html.xsl $(POSTPROCDIR)/html
	$(SAXON) $< ../lang/html.xsl | $(POSTPROCDIR)/html > $@

.PHONY: clean
clean:
	-rm $(PRODUCTS)

$(POSTPROCDIR)/html: $(POSTPROCDIR)/html.l
	cd $(POSTPROCDIR); make
$(POSTPROCDIR)/man: $(POSTPROCDIR)/man.l
	cd $(POSTPROCDIR); make
$(SSI): # Add dependencies here?
	cd $(SSIDIR); make

.PRECIOUS: *.depend
%.depend: %.sxml $(wildcard *.sxml)
	cat $*.sxml | $(SSI) --head $*.sxml --deps > $@

-include $(wildcard *.depend)
