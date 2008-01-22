JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
POSTPROCDIR=../postproc
SSI=../ssi/ssipp

ifneq ($(strip $(wildcard $(JAR1))),)
SAXON=java -jar $(JAR1)
else
SAXON=java -jar $(JAR2)
endif

PRODUCTS = html/index.html html/coretypes.html html/bindings.html
TUT_DEPS = tutorial.sxml chap-hello.sxml

all: $(PRODUCTS)

# This is how I used to apply the transform before I needed XPATH 2 functions:
#	@xsltproc man.xsl mini.sxml | sed '1,1d' > $@

%.xml: %.sxml %.depend
	cat $< | $(SSI) > $@

html/index.html: lang.xml html.xsl $(POSTPROCDIR)/html
	$(SAXON) $< html.xsl | $(POSTPROCDIR)/html > $@
html/coretypes.html: coretypes.xml html-types.xsl $(POSTPROCDIR)/html
	$(SAXON) $< html-types.xsl | $(POSTPROCDIR)/html > $@
html/bindings.html: bindings.xml html-bindings.xsl $(POSTPROCDIR)/html
	$(SAXON) $< html-bindings.xsl | $(POSTPROCDIR)/html > $@

.PHONY: clean
clean:
	-rm $(PRODUCTS)

$(POSTPROCDIR)/html: $(POSTPROCDIR)/html.l
	cd $(POSTPROCDIR); make
$(POSTPROCDIR)/man: $(POSTPROCDIR)/man.l
	cd $(POSTPROCDIR); make

.PRECIOUS: *.depend
%.depend: %.sxml $(wildcard *.sxml)
	cat $*.sxml | $(SSI) --head $*.sxml --deps > $@

-include $(wildcard *.depend)
