SHAPES = ${top_builddir}/source/shapes

TEST_BASE = bad hello approximate2d

XFAIL_TESTS = test_bad
TESTS = ${addprefix test_, ${TEST_BASE}}
check_DATA = ${addsuffix .png, ${TEST_BASE}} \
             ${addsuffix .correct.png, ${TEST_BASE}}
dist_check_DATA = testTEMPLATE \
                  ${addsuffix .shape, ${TEST_BASE}} \
                  ${addsuffix .correct.pdf, ${TEST_BASE}}
CLEANFILES = ${addprefix test_, ${TEST_BASE}} \
             ${addsuffix .png, ${TEST_BASE}} \
             ${addsuffix .correct.png, ${TEST_BASE}}

test_% : testTEMPLATE
	@sed -e 's/FNAME/$*/g' < $< > $@ && \
  chmod a+x $@

%.png: %.pdf
	@CONVERT@ $< $@ > /dev/null 2>&1 || touch $@

%.png: ${srcdir}/%.pdf
	@CONVERT@ $< $@ > /dev/null 2>&1 || touch $@

# %.correct.pdf : %.shape ${SHAPES}
# 	-$(SHAPES) $< && touch $@

%.pdf: ${srcdir}/%.shape $(SHAPES)
	-@$(SHAPES) $< > /dev/null 2>&1 || touch $@
