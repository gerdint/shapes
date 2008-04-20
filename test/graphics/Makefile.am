# === How to setup a new test ===
# It is actually very simple:
# Just add your .shape file and a corresponding .correct.pdf file.  If
# the test is expected to fail, simply add a .correct.pdf file such
# that a failing test will not be able to create it, for instance by
# copying the .correct.pdf file from another test.  See below for how
# to indicate that a test is expected to fail.

# The following variable must be edited by hand to include every test
# that is expected to fail.
XFAIL_TESTS = test_bad


SHAPES = ${top_builddir}/source/shapes
SHAPESFLAGS = --resources=no \
              --fontmetricspath ${top_srcdir}/resources/fontmetrics \
              --needpath ${top_srcdir}/resources/extensions \
              --tmpdir ./tmp \
              --outdir ./out

TEST_BASE = $(notdir $(basename $(wildcard ${srcdir}/*.shape)))
TESTS = ${addprefix test_, ${TEST_BASE}}
check_DATA = $(patsubst %,out/%.pdf,${TEST_BASE})
dist_check_DATA = testTEMPLATE \
                  ${addsuffix .shape, ${TEST_BASE}} \
                  ${addsuffix .correct.pdf, ${TEST_BASE}} \
                  mypackage.sty
CLEANFILES = ${addprefix test_, ${TEST_BASE}} \
             ${addsuffix .log, ${TEST_BASE}} \
             tmp/*.* \
             out/*.*

TESTS_ENVIRONMENT = COMPARE="${COMPARE}"

test_% : testTEMPLATE
	@sed -e 's/FNAME/$*/g' < $< > $@ && \
  chmod a+x $@

out/%.pdf: ${srcdir}/%.shape $(SHAPES)
	-@mkdir out tmp > /dev/null 2>&1
	-@$(SHAPES) $(SHAPESFLAGS) $< > /dev/null 2>&1 || touch $@
