all: pathconstruction_small.jpg boxedeq_small.jpg

%.jpg: ../examples/features/%.pdf
	convert -density 150 -resize 150x $< $@

%_small.jpg: ../examples/features/%.pdf
	convert -density 50 -resize 50x $< $@
