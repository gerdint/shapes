SHAPES = ${top_builddir}/source/shapes
XFAIL_TESTS = test_bad
TESTS = test_bad test_hello test_approximate2d
CLEANFILES = test_bad bad.correct.png bad.png \
             test_hello hello.correct.png hello.png \
             test_approximate2d approximate2d.correct.png approximate2d.png
check_DATA = bad.png bad.correct.png \
             hello.png hello.correct.png \
             approximate2d.png approximate2d.correct.png
dist_check_DATA = bad.shape bad.correct.pdf \
                  hello.shape hello.correct.pdf \
                  approximate2d.shape approximate2d.correct.pdf

test_% :
	@echo -e "#/bin/sh\n\ncmp -s $*.png $*.correct.png\nexit "'$$?' > $@
	@chmod a+x $@


%.png: %.pdf
	-@CONVERT@ $< $@ || touch $@

%.png: ${srcdir}/%.pdf
	-@CONVERT@ $< $@ || touch $@

# %.correct.pdf : %.shape ${SHAPES}
# 	-$(SHAPES) $< && touch $@

%.pdf: ${srcdir}/%.shape $(SHAPES)
	-$(SHAPES) $< || touch $@
