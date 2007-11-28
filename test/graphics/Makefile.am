SHAPES = ${top_builddir}/source/shapes
SHAPESFLAGS = --no-resources \
              --fontmetricspath ${top_srcdir}/resourses/fontmetrics \
              --needpath ${top_srcdir}/resourses/extensions \
              --tmpdir ./tmp \
              --outdir ./out

#TEST_BASE = bad hello approximate2d texlabel
TEST_BASE = $(patsubst %.shape,%,$(wildcard *.shape))

XFAIL_TESTS = test_bad
TESTS = ${addprefix test_, ${TEST_BASE}}
check_DATA = $(patsubst %,out/%.pdf,${TEST_BASE})
dist_check_DATA = testTEMPLATE \
                  ${addsuffix .shape, ${TEST_BASE}} \
                  ${addsuffix .correct.pdf, ${TEST_BASE}}
CLEANFILES = ${addprefix test_, ${TEST_BASE}} \
             ${addsuffix .log, ${TEST_BASE}} \
             tmp/*.* \
             out/*.*

TESTS_ENVIRONMENT = COMPARE="${COMPARE}"

test_% : testTEMPLATE
	@sed -e 's/FNAME/$*/g' < $< > $@ && \
  chmod a+x $@

# %.correct.pdf : %.shape ${SHAPES}
# 	-$(SHAPES) $< && touch $@

out/%.pdf: ${srcdir}/%.shape $(SHAPES)
	-@$(SHAPES) $(SHAPESFLAGS) $< > /dev/null 2>&1 || touch $@
