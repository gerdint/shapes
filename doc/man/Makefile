all: man1/shapes.1

# This is how I used to apply the transform before I needed XPATH 2 functions:
#	@xsltproc man.xsl mini.sxml | sed '1,1d' > $@

man1/shapes.1: tool-interface.sxml man.xsl single-breaker
	java -jar /Library/Java/Extensions/saxonb9-0-0-2j/saxon9.jar $< man.xsl | sed '1,1d' | ./single-breaker > $@

single-breaker: single-breaker.l
	flex --out single-breaker.c single-breaker.l
	gcc -o single-breaker single-breaker.c -ll
