JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
PP_HTML=../postproc/html
PP_MAN=../postproc/man
SSI=../ssi/ssipp

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

%.xml: %.sxml %.depend
	cat $< | $(SSI) > $@

html/index.html: tutorial.xml ../lang/html.xsl $(PP_HTML)
	$(SAXON) $< ../lang/html.xsl | $(PP_HTML) > $@

.PHONY: clean
clean:
	-rm $(PRODUCTS) *.depend

.PRECIOUS: *.depend
%.depend: %.sxml $(wildcard *.sxml)
	cat $*.sxml | $(SSI) --head $*.sxml --deps > $@

-include $(wildcard *.depend)
