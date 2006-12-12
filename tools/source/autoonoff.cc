#include "autoonoff.h"
#include <iostream>

using namespace std;

void
autoonoff__exitWithMessage( const char * msg )
{
  cerr << msg << endl ;
  exit( 1 );
}

int HelloGoodbye::indent( 0 );

HelloGoodbye::HelloGoodbye( const char *_mess )
  : mess( strdup( _mess ) )
{
  for(int i = 0; i < indent; i++)
    cerr << " " ;
  cerr << "Hello, " << mess << endl ;
  indent += TABSIZE;
}

HelloGoodbye::~HelloGoodbye()
{
  indent -= TABSIZE;
  for(int i = 0; i < indent; i++)
    cerr << " " ;
  cerr << "Good bye, " << mess << endl ;
  delete mess;
}

Hello::Hello( const char * mess )
{
  cerr << "Hello, " << mess << endl ;
}

Hello::~Hello()
{ }

Goodbye::Goodbye( const char * _mess )
  : mess( _mess )
{ }

Goodbye::~Goodbye()
{
  cerr << "Good bye, " << mess << endl ;
}
