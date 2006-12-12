# To run all the tests, change to this directory and do
#   make clean
#   make
# The results shall then be stored in report, unless the test process failed for some reason...

all: report

report: text/report graphics/report
	cat text/report graphics/report | sort > report

.PHONY: FORCE

text/report: FORCE
	cd text && $(MAKE) clean && $(MAKE) report
graphics/report: FORCE
	cd graphics && $(MAKE) clean && $(MAKE) report

.PHONY: clean
clean:
	rm report
