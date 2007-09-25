SHAPES=drool --v w1.4

all: spline.pdf absolute.pdf linkoping.pdf sidepaths.pdf blendarrows.pdf approximators3d.pdf zbufdemo.pdf lightdemo.pdf kjmrobot.pdf intersections.pdf

%.pdf: %.drool
	$(SHAPES) $*.

%.pdf: ../../../../examples/features/%.drool
	$(SHAPES) --in $< --out $@

%.pdf: ../../../../examples/showcase/%.drool
	$(SHAPES) --in $< --out $@
