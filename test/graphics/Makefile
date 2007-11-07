# Important:  The tests in this cathegory shall produce graphics.
# The output on stdout and stderr are not used in the tests.
#
# To create a new test, add the Shapes source, say test1.shape to this directory.  Then
#   make test1.correct
# That's all one has to do.
#
# To run the test, do
#   make test1.test
# The result is stored in test1.test, so to find all tests that fail, you may
#   grep Failed *.test
# or simply,
#   make clean
#   make

ALLTEST := $(patsubst %.shape,%.test, $(wildcard *.shape) )
SHAPES = ../../source/shapes

ALL: report

report: $(ALLTEST)
	-rm report
	cat *.test > $@
	cat ../expretty | ex -sC $@

# When using cmp, also capture stderr, since cmp will complain there if a file is empty.
%.test: %.png %.correct.png
	-cmp $^ >& cmptmp
	echo $(CURDIR)/$* `cat cmptmp` > $@

# One rule is used to make the %.png frm the %.pdf, so that errors can be handled within the rule.
%.png: %.pdf
	-convert -density 150 -resize 150x $< $@
	touch $@

# One rule is used to make the %.shape frm the %.pdf, so that errors can be handled within the rule.
%.pdf: %.shape $(SHAPES)
	-$(SHAPES) $<
	touch $@

.PHONY: clean
clean:
	-rm *.labels* $(filter-out $(wildcard *.correct.pdf), $(wildcard *.pdf) ) *.png *.test cmptmp *~ report
