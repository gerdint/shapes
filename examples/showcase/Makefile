ALLSHAPE := $(patsubst %.shape,%.pdf, $(wildcard *.shape) )
SHAPESFLAGS = -N../../resources/extensions --tmp*=yes --tmpdir /tmp/shapes-$(USER)

all: $(ALLSHAPE)

%.pdf: %.shape
	../../source/shapes $(SHAPESFLAGS) $<

.PHONY: cleanlabels
cleanlabels:
	rm -f *.labels*.*
	rm -f *~

.PHONY: cleanpdf
cleanpdf:
	rm *.pdf
