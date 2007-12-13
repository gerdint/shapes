JAR1=/Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar
JAR2=/home/rt/tidefelt/sw/Java/Extensions/saxonb9-0-0-2j/saxon9.jar

ifneq ($(strip $(wildcard $(JAR1))),)
SAXON=java -jar $(JAR1)
else
SAXON=java -jar $(JAR2)
endif

PRODUCTS = man1/shapes.1 html/index.html

all: $(PRODUCTS)

# This is how I used to apply the transform before I needed XPATH 2 functions:
#	@xsltproc man.xsl mini.sxml | sed '1,1d' > $@

man1/shapes.1: tool-interface.sxml man.xsl man-postproc
	$(SAXON) $< man.xsl | ./man-postproc > $@

html/index.html: tool-interface.sxml html.xsl html-postproc
	$(SAXON) $< html.xsl | ./html-postproc > $@

%-postproc: %-postproc.l
	flex --out $*-postproc.c $*-postproc.l
	gcc -o $@ $*-postproc.c -ll

.PHONY: clean
clean:
	-rm $(PRODUCTS)
