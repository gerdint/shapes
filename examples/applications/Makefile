ALLSHAPE := $(patsubst %.shape,%.pdf, $(wildcard *.shape) )
RTLOGOS := rtlogo_right.pdf rtlogo_left.pdf rtlogo_right_nobg.pdf rtlogo_left_nobg.pdf rtlogo_square.pdf
SHAPESFLAGS = --v w1.3

all: $(ALLSHAPE)

rt: $(RTLOGOS)
$(RTLOGOS): rtlogo_elements.shext Makefile

%.pdf: %.shape
	shapes $(SHAPESFLAGS) $*.

.PHONY: cleanlabels
cleanlabels:
	rm -f *.0pdf
	rm -f *.labels*.*
	rm -f *~

.PHONY: cleanpdf
cleanpdf:
	rm *.pdf
