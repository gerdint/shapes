all: jpg stdout

jpg: pathconstruction_y_small.jpg boxedeq_y_small.jpg cornerpoint_y_big.jpg tutorial-transforms_y_big.jpg tutorial-path-painting_y_big.jpg tutorial-graphics-params_y_big.jpg facefun_y_big.jpg tutorial-one-segment_y_big.jpg tutorial-bezier-connected_y_big.jpg
stdout: dynamic.stdout escape.stdout evalorder.stdout scopes.stdout values.stdout

.PRECIOUS: %.pdf %_3.pdf

%.pdf %.stdout: ../../examples/features/%.shape
	shapes --in $< --out $*.pdf > $*.stdout
%.pdf %.stdout: ../../examples/doc/%.shape
	shapes --in $< --out $*.pdf > $*.stdout

%_3.pdf: ../../examples/features/%.shape
	shapes --v s1.3 --in $< --out $*_3.pdf > $*.stdout
%_3.pdf: ../../examples/doc/%.shape
	shapes --v s1.3 --in $< --out $*_3.pdf > $*.stdout

%_y_big.jpg: %_3.pdf
	convert -density 150 -resize x150 $< $@

%_y_small.jpg: %.pdf
	convert -density 50 -resize x50 $< $@

%_x_big.jpg: %.pdf
	convert -density 150 -resize 150x $< $@

%_x_small.jpg: %.pdf
	convert -density 50 -resize 50x $< $@

.PHONY: clean
clean:
	-rm \#shapes.*
