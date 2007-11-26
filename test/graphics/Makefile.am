SHAPES = ${top_builddir}/source/shapes

TEST_BASE = bad hello approximate2d

XFAIL_TESTS = test_bad
TESTS = ${addprefix test_, ${TEST_BASE}}
check_DATA = ${addsuffix .pdf, ${TEST_BASE}}
dist_check_DATA = testTEMPLATE \
                  ${addsuffix .shape, ${TEST_BASE}} \
                  ${addsuffix .correct.pdf, ${TEST_BASE}}
CLEANFILES = ${addprefix test_, ${TEST_BASE}} \
             ${addsuffix .pdf, ${TEST_BASE}} \
             ${addsuffix .log, ${TEST_BASE}}

TESTS_ENVIRONMENT = COMPARE="${COMPARE}"

test_% : testTEMPLATE
	@sed -e 's/FNAME/$*/g' < $< > $@ && \
  chmod a+x $@

# %.correct.pdf : %.shape ${SHAPES}
# 	-$(SHAPES) $< && touch $@

%.pdf: ${srcdir}/%.shape $(SHAPES)
	-@$(SHAPES) $< > /dev/null 2>&1 || touch $@
