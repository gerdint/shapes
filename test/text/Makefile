# Important:  The tests in this cathegory shall use stdout for the output.
# The produced graphics is not used in the test, and neither is stderr.
#
# To create a new test, add the Drool source, say test1.drool to this directory.  Then
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

ALLTEST := $(patsubst %.drool,%.test, $(wildcard *.drool) )
DROOL = ../../bin/drool

ALL: report

report: $(ALLTEST)
	-rm report
	cat *.test > $@
	cat ../expretty | ex -sC $@

# When using cmp, also capture stderr, since cmp will complain there if a file is empty.
%.test: %.out # the correct output is not added here, since it should never be made in request to this rule.
	-cmp $< $*.correct >& cmptmp
	echo $(CURDIR)/$* `cat cmptmp` > $@

%.correct: %.out
	mv $< $@

%.out: %.drool $(DROOL)
	-$(DROOL) $*. > $@

.PHONY: clean
clean:
	-rm *.labels* *.pdf *.out *.test *~
