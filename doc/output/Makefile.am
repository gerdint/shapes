SHAPES = $(top_builddir)/source/shapes
SHAPESFLAGS = --resources=no \
              --fontmetricspath ${top_srcdir}/resources/fontmetrics \
              --needpath ${top_srcdir}/resources/extensions

dist_noinst_DATA = pathconstruction_y_small.jpg boxedeq_y_small.jpg \
                   cornerpoint_y_big.jpg tutorial-transforms_y_big.jpg \
                   tutorial-path-painting_y_big.jpg \
                   tutorial-graphics-params_y_big.jpg \
                   facefun_y_big.jpg tutorial-one-segment_y_big.jpg \
                   tutorial-bezier-connected_y_big.jpg \
                   tutorial-slider-use_y_big.jpg upsample_y_big.jpg \
                   sidepaths_y_big.jpg \
                   dynamic.stdout escape.stdout evalorder.stdout \
                   scopes.stdout values.stdout states.stdout curry.stdout \
                   ranges.stdout

clean-local :
	${RM} \#shapes.*


%.pdf %.stdout: $(top_srcdir)/examples/features/%.shape
	$(SHAPES) $(SHAPESFLAGS) --in $< --out $*.pdf > $*.stdout
%.pdf %.stdout: $(top_srcdir)/examples/doc/%.shape
	$(SHAPES) $(SHAPESFLAGS) --in $< --out $*.pdf > $*.stdout

%_3.pdf: $(top_srcdir)/examples/features/%.shape
	$(SHAPES) $(SHAPESFLAGS) -vs1.3 --in $< --out $*_3.pdf > $*.stdout
%_3.pdf: $(top_srcdir)/examples/doc/%.shape
	$(SHAPES) $(SHAPESFLAGS) -vs1.3 --in $< --out $*_3.pdf > $*.stdout

%_y_big.jpg: %_3.pdf
	convert -density 150 -resize x150 $< $@

%_y_small.jpg: %.pdf
	convert -density 50 -resize x50 $< $@

%_x_big.jpg: %.pdf
	convert -density 150 -resize 150x $< $@

%_x_small.jpg: %.pdf
	convert -density 50 -resize 50x $< $@
