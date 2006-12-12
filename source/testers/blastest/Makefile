CC = g++
LD = g++
AR = ar

LLAPACK = $(shell ../tools/source/whichlLAPACK)
ILAPACK = $(shell ../tools/source/whichiLAPACK) -I../tools/include

PROGRAM = blastest
OBJECTS = main.o

CFLAGS = 

ARFLAGS = -r

LIBS = -lm -lc $(LLAPACK)

all:$(PROGRAM)

%.o : %.cc
	$(CC) $(CFLAGS) -c -o $@ $< $(INCS) $(ILAPACK)

$(PROGRAM): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(PROGRAM) $(OBJECTS) $(LIBS)
clean: 
	rm -f *.o $(THELIB) $(PROGRAM) 
