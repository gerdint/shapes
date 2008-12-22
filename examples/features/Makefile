ALLSHAPE := $(patsubst %.shape,%.pdf, $(filter-out static_errors.shape, $(wildcard *.shape) ) )
SHAPESFLAGS = -N../../resources/extensions --tmp*=yes --tmpdir /tmp/shapes-$(USER)

all: $(ALLSHAPE)

e:
	-echo $(ALLSHAPE)

%.pdf: %.shape
	../../source/shapes $(SHAPESFLAGS) $<

.PHONY: cleanlabels
cleanlabels:
	rm -f *.labels*.*
	rm -f *~

.PHONY: cleanpdf
cleanpdf:
	rm *.pdf

imports.pdf: multipage.pdf
