## Please make sure that index is made last, since making the parts will ensure that meta data about file locations is generated.

DIRS = man syntax bindings states dynamic types state-types tutorial extensions index

all:
	for d in $(DIRS); \
	do \
		make -C $$d all ; \
	done

install:

check:

.PHONY: clean install all check
clean:
	for d in $(DIRS); do \
		make -C $$d clean ; \
	done

