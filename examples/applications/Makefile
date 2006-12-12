ALLDROOL := $(patsubst %.drool,%.pdf, $(wildcard *.drool) )
RTLOGOS := rtlogo_right.pdf rtlogo_left.pdf rtlogo_right_nobg.pdf rtlogo_left_nobg.pdf rtlogo_square.pdf
DROOLFLAGS = --v w1.3

all: $(ALLDROOL)

rt: $(RTLOGOS)
$(RTLOGOS): rtlogo_elements.drext Makefile

%.pdf: %.drool
	drool $(DROOLFLAGS) $*.

.PHONY: cleanlabels
cleanlabels:
	rm -f *.0pdf
	rm -f *.labels*.*
	rm -f *~

.PHONY: cleanpdf
cleanpdf:
	rm *.pdf
