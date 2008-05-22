SHAPES = $(top_builddir)/source/shapes
SHAPESFLAGS = --resources=no \
              --fontmetricspath ${top_srcdir}/resources/fontmetrics \
              --needpath ${top_srcdir}/resources/extensions \
              --tp=no

IMG_OBJS = pathconstruction_y_small boxedeq_y_small cornerpoint_y_big tutorial-transforms_y_big tutorial-path-painting_y_big tutorial-graphics-params_y_big facefun_y_big tutorial-one-segment_y_big tutorial-bezier-connected_y_big tutorial-slider-use_y_big upsample_y_big sidepaths_y_big mergepaths_y_big asymmetric-notation_y_small tutorial-maximizer_y_big
TXT_OBJS = dynamic escape evalorder scopes values states curry ranges

dist_noinst_DATA = $(patsubst %, %.stdout, $(TXT_OBJS) ) \
                   $(patsubst %, %.jpg, $(IMG_OBJS) ) \
                   $(patsubst %_y_small, %.pdf, $(filter %_y_small, $(IMG_OBJS) ) ) \
                   $(patsubst %_y_big, %.pdf, $(filter %_y_big, $(IMG_OBJS) ) ) \
                   $(patsubst %_x_small, %.pdf, $(filter %_x_small, $(IMG_OBJS) ) ) \
                   $(patsubst %_x_big, %.pdf, $(filter %_x_big, $(IMG_OBJS) ) )

clean-local :
	${RM} \#shapes.*


%.pdf %.stdout: $(top_srcdir)/examples/features/%.shape
	$(SHAPES) $(SHAPESFLAGS) --in $< --out $*.pdf > $*.stdout
%.pdf %.stdout: $(top_srcdir)/examples/showcase/%.shape
	$(SHAPES) $(SHAPESFLAGS) --in $< --out $*.pdf > $*.stdout
%.pdf %.stdout: $(top_srcdir)/examples/doc/%.shape
	$(SHAPES) $(SHAPESFLAGS) --in $< --out $*.pdf > $*.stdout

%_y_big.jpg: %.pdf
	convert -density 150 -resize x150 $< $@

%_y_small.jpg: %.pdf
	convert -density 50 -resize x50 $< $@

%_x_big.jpg: %.pdf
	convert -density 150 -resize 150x $< $@

%_x_small.jpg: %.pdf
	convert -density 50 -resize 50x $< $@
