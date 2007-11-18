SHAPES = ${top_builddir}/source/shapes
XFAIL_TESTS = test_bad
TESTS = test_bad test_scopes
check_DATA = bad.out scopes.out
dist_check_DATA = bad.shape bad.correct \
            scopes.shape scopes.correct
CLEANFILES = test_bad bad.out bad.pdf \
             test_scopes scopes.out scopes.pdf


test_% :
	@echo -e "#/bin/sh\n\ncmp -s $*.out ${srcdir}/$*.correct\nexit "'$$?' > $@
	@chmod a+x $@

# %.correct : %.out
# 	mv $< $@

%.out : ${srcdir}/%.shape ${SHAPES}
	-$(SHAPES) $< > $@