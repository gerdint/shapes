PRODUCTS = man html

all: $(PRODUCTS)

# This is how I used to apply the transform before I needed XPATH 2 functions:
#	@xsltproc man.xsl mini.sxml | sed '1,1d' > $@

%: %.l
	flex --out yylex.c $<
	gcc -o $@ yylex.c -ll

.PHONY: clean
clean:
	-rm *.o yylex.c $(PRODUCTS)
