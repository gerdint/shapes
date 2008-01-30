CC = g++
LD = g++
LEX = flex -+

PROGRAM = ssipp
OBJECTS = ssiyylex.o ssiscanner.o main.o

CFLAGS = -Wall $(CPPFLAGS)
# The -d flag tells lex to set up for debugging. Can turn on/off by
# setting value of global yy_flex_debug inside the scanner itself
LEXFLAGS = -d

# The -d flag tells yacc to generate header with token types
# The -v flag writes out a verbose description of the states and conflicts
# The -t flag turns on debugging capability
YACCFLAGS = -dvt

INCS =
LIBS = -lc -lfl

all:$(PROGRAM)

%.o: %.cc
	$(CC) $(CFLAGS) -c  -o $@ $< $(INCS)


%yylex.cc: %yylex.ll # %scanner.h
	$(LEX) -o $@ $<

$(PROGRAM):$(OBJECTS)
	$(CC) $(CFLAGS) -o $(PROGRAM) $(OBJECTS) $(LIBS)
clean:
	rm -f *.o *yylex.cc$(PROGRAM)
