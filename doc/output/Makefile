all: jpg stdout

jpg: pathconstruction_y_small.jpg boxedeq_y_small.jpg cornerpoint_y_big.jpg
stdout: dynamic.stdout escape.stdout evalorder.stdout scopes.stdout values.stdout

.PRECIOUS: %.pdf

%.pdf %.stdout: ../../examples/features/%.drool
	drool --in $< --out $*.pdf > $*.stdout

%_3.pdf: ../../examples/features/%.drool
	drool --v s1.3 --in $< --out $*_3.pdf > $*.stdout

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
	-rm \#drool.*
