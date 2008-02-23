%{
#include <iostream>
#include <stack>
#include <string>

  bool verbose = false;
  bool doBraces = false;
  bool doParens = false;

  size_t lineNumber = 1;

  std::stack< char > tokenStack;
  std::stack< size_t > lineNumberStack;

%}
%option noyywrap

WS [ \t\n]*

%%

"\{" {
  if( doBraces )
    {
      if( verbose )
	{
	  std::cout << std::string( tokenStack.size( ), ' ' ) << yytext << " on line " << lineNumber << std::endl ;
	}
      tokenStack.push( '{' );
      lineNumberStack.push( lineNumber );
    }
 }
"\}" {
  if( doBraces )
    {
      if( tokenStack.empty( ) )
	{
	  std::cerr << "Underflow at \"" << yytext << "\" on line " << lineNumber << std::endl ;
	  exit( 1 );
	}
      if( tokenStack.top( ) != '{' )
	{
	  std::cerr << tokenStack.top( ) << " on line " << lineNumberStack.top( ) << " balanced by } on line " << lineNumber << "." << std::endl ;
	  exit( 1 );
	}
      tokenStack.pop( );
      lineNumberStack.pop( );
      if( verbose )
	{
	  std::cout << std::string( tokenStack.size( ), ' ' ) << yytext << " on line " << lineNumber << std::endl ;
	}
    }
}

"\(" {
  if( doParens )
    {
      if( verbose )
	{
	  std::cout << std::string( tokenStack.size( ), ' ' ) << yytext << " on line " << lineNumber << std::endl ;
	}
      tokenStack.push( '(' );
      lineNumberStack.push( lineNumber );
    }
 }
"\)" {
  if( doParens )
    {
      if( tokenStack.empty( ) )
	{
	  std::cerr << "Underflow at \"" << yytext << "\" on line " << lineNumber << std::endl ;
	  exit( 1 );
	}
      if( tokenStack.top( ) != '(' )
	{
	  std::cerr << tokenStack.top( ) << " on line " << lineNumberStack.top( ) << " balanced by ) on line " << lineNumber << "." << std::endl ;
	  exit( 1 );
	}
      tokenStack.pop( );
      lineNumberStack.pop( );
      if( verbose )
	{
	  std::cout << std::string( tokenStack.size( ), ' ' ) << yytext << " on line " << lineNumber << std::endl ;
	}
    }
}

"\n" {
  ++lineNumber;
}

. {
  // Do nothing
}

%%

int
main( int argc, char ** argv )
{
  --argc;
  ++argv;

  if( argc != 1 )
    {
      std::cerr << "This program takes exactly one argument, not " << argc << "." << std::endl ;
      exit( 1 );
    }

  for( const char * src = *argv; *src != '\0'; ++src )
    {
      switch( *src )
	{
	case '{':
	  doBraces = true;
	  break;
	case '(':
	  doParens = true;
	  break;
	case 'v':
	  verbose = true;
	  break;
	default:
	  std::cerr << "The only allowed open-characters are [v{(], not " << *src << "." << std::endl ;
	  exit( 1 );
	}
    }

  yylex( );

  if( tokenStack.empty( ) )
    {
      return 0;
    }

  std::cerr << "The opening " << tokenStack.top( ) << " on line " << lineNumberStack.top( ) << " was never closed." << std::endl ;
  exit( 1 );
}
