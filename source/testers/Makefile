CC = g++
LD = g++

SIMPLEXOBJECTS = ../basicsimplex.o \
../../tools/source/autoonoff.o \
simplex.o

CFLAGS = -Wall -Wsynth -g

INCS = -I../../tools/include -I/home/rt/tidefelt/source/DTForth3 -I/home/rt/tidefelt/include

LIBS = -lm -lc -ll -L/home/rt/tidefelt/lib -ldtforth

all:simplex

simplex: $(SIMPLEXOBJECTS)
	$(LD) -o $@ $(SIMPLEXOBJECTS) $(LIBS)

%.o : %.cc
	$(CC) $(CFLAGS) -c -o $@ $< $(INCS)
