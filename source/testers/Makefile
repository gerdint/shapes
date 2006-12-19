CC = g++
LD = g++

SIMPLEXOBJECTS = ../basicsimplex.o \
../../tools/source/autoonoff.o \
simplex.o

CFLAGS = -Wall -Wsynth -g

INCS = -I../../tools/include

LIBS = -lm -lc -ll

all:simplex

simplex: $(SIMPLEXOBJECTS)
	$(LD) -o $@ $(SIMPLEXOBJECTS) $(LIBS)

%.o : %.cc
	$(CC) $(CFLAGS) -c -o $@ $< $(INCS)
