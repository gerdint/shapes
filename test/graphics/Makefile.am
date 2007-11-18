SHAPES = ${top_builddir}/source/shapes
XFAIL_TESTS = test_bad
TESTS = test_bad test_hello
CLEANFILES = test_bad bad.correct.png bad.png \
             test_hello hello.correct.png hello.png
check_DATA = bad.png bad.correct.png \
             hello.png hello.correct.png
dist_check_DATA = bad.shape bad.correct.pdf \
                  hello.shape hello.correct.pdf

test_% :
	@echo -e "#/bin/sh\n\ncmp -s $*.png $*.correct.png\nexit "'$$?' > $@
	@chmod a+x $@


%.png: %.pdf
	-convert -density 150 -resize 150x $< $@
	touch $@

%.png: ${srcdir}/%.pdf
	-convert -density 150 -resize 150x $< $@
	touch $@

# %.correct.pdf : %.shape ${SHAPES}
# 	-$(SHAPES) $< && touch $@

%.pdf: ${srcdir}/%.shape $(SHAPES)
	-$(SHAPES) $< && touch $@
