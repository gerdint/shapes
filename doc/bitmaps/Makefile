all: pathconstruction_y_small.jpg boxedeq_y_small.jpg

%_y_big.jpg: ../examples/features/%.pdf
	convert -density 150 -resize x150 $< $@

%_y_small.jpg: ../examples/features/%.pdf
	convert -density 50 -resize x50 $< $@

%_x_big.jpg: ../examples/features/%.pdf
	convert -density 150 -resize 150x $< $@

%_x_small.jpg: ../examples/features/%.pdf
	convert -density 50 -resize 50x $< $@
