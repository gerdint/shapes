JAVA=java -classpath '/Library/Java/Extensions/;/home/rt/tidefelt/sw/Java/Extensions/'

all: man1/shapes.1 html/index.html

# This is how I used to apply the transform before I needed XPATH 2 functions:
#	@xsltproc man.xsl mini.sxml | sed '1,1d' > $@

man1/shapes.1: tool-interface.sxml man.xsl man-postproc
	$(JAVA) -jar saxonb9-0-0-2j/saxon9.jar $< man.xsl | ./man-postproc > $@

html/index.html: tool-interface.sxml html.xsl
	$(JAVA) -jar saxonb9-0-0-2j/saxon9.jar $< html.xsl > $@

man-postproc: man-postproc.l
	flex --out man-postproc.c man-postproc.l
	gcc -o man-postproc man-postproc.c -ll
