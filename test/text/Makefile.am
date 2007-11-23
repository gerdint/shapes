SHAPES = ${top_builddir}/source/shapes
SHAPESFLAGS = --no-resources \
              --fontmetricspath  ${top_srcdir}/resourses/fontmetrics

TEST_BASE = bad scopes

XFAIL_TESTS = test_bad
TESTS = ${addprefix test_, ${TEST_BASE}}
check_DATA = ${addsuffix .out, ${TEST_BASE}}
dist_check_DATA = testTEMPLATE \
                  ${addsuffix .shape, ${TEST_BASE}} \
                  ${addsuffix .correct, ${TEST_BASE}}
CLEANFILES = ${addprefix test_, ${TEST_BASE}} \
             ${addsuffix .out, ${TEST_BASE}} \
             ${addsuffix .pdf, ${TEST_BASE}}

test_% : testTEMPLATE
	@sed -e 's/FNAME/$*/g' < $< > $@ && \
  chmod a+rx $@

# %.correct : %.out
# 	mv $< $@

%.out : ${srcdir}/%.shape ${SHAPES}
	-@$(SHAPES) ${SHAPESFLAGS} $< > $@ 2> /dev/null