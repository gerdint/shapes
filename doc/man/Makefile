JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
POSTPROCDIR=../postproc

ifneq ($(strip $(wildcard $(JAR1))),)
SAXON=java -jar $(JAR1)
else
SAXON=java -jar $(JAR2)
endif

PRODUCTS = man1/shapes.1 html/index.html

all: $(PRODUCTS)

# This is how I used to apply the transform before I needed XPATH 2 functions:
#	@xsltproc man.xsl mini.sxml | sed '1,1d' > $@

man1/shapes.1: tool-interface.sxml man.xsl $(POSTPROCDIR)/man
	$(SAXON) $< man.xsl | $(POSTPROCDIR)/man > $@

html/index.html: tool-interface.sxml html.xsl $(POSTPROCDIR)/html
	$(SAXON) $< html.xsl | $(POSTPROCDIR)/html > $@

.PHONY: clean
clean:
	-rm $(PRODUCTS)

$(POSTPROCDIR)/html $(POSTPROCDIR)/man:
	cd $(POSTPROCDIR); make
