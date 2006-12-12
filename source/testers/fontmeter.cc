#include "../afmscanner.h"

#include <iostream>
#include <sstream>
#include <fstream>

void argcAssertion( const char * optionSpecifier, int argc, int argcMin );


int
main( int argc, char ** argv )
{
  const char * fontname = "Helvetica";
  bool yydebug = false;

  argc -= 1;
  argv += 1;
  while( argc > 0 )
    {
      if( strcmp( *argv, "--font" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  fontname = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--yydebug" ) == 0 )
	{
	  yydebug = true;
	  argv += 1;
	  argc -= 1;
	}
      else
	{
	  std::cerr << "Illegal command line option: " << *argv << std::endl ;
	  exit( 1 );
	}
    }

  std::ostringstream filename;
  filename << "resources/fontmetrics/" << fontname << ".afm" ;
  std::ifstream afmFile( filename.str( ).c_str( ) );
  if( ! afmFile.is_open( ) )
    {
      std::cerr << "Failed to open " << filename.str( ) << std::endl ;
      exit( 1 );
    }
  
  FontMetrics::BaseFont metrics;
  AfmScanner scanner( & metrics, & afmFile );
  scanner.setTellQue( true ); // We want to know about things that are not recognized and this ignored.
  if( yydebug )
    {
      scanner.set_debug( 1 );
    }
  try
    {
      int status = scanner.yylex( );
      if( status != 0 )
	{
	  throw "Scanner failed with non-zero return value ";
	}
    }
  catch( const char * ball )
    {
      std::cout << "Parse failed with message: " << ball << std::endl ;
      exit( 1 );
    }
  catch( const RefCountPtr< const char > ball )
    {
      std::cout << "Parse failed with message: " << ball << std::endl ;
      exit( 1 );
    }

  RefCountPtr< FontMetrics::WritingDirectionMetrics > horizontalMetrics = metrics.horizontalMetrics_;
  if( horizontalMetrics == NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
    {
      std::cerr << "No horizontal metrics defined." << std::endl ;
      exit( 1 );
    }

  //  std::cout << "=== Named characters ===" << std::endl ;
  //  horizontalMetrics->display( std::cout );
  std::cout << std::endl
	    << std::endl
	    << "Characters are now read from stdin." << std::endl ;

  char name[2];
  name[1] = '\0';
  std::cin.get( name[0] );
  for( ; ! std::cin.eof( ); std::cin.get( name[0] ) )
    {
      try
	{
	  const FontMetrics::CharacterMetrics * charMetrics = horizontalMetrics->charByName( name );
	  std::cout << "The character named \"" << name << "\" has horizontal width " << charMetrics->horizontalCharWidthX_ << std::endl ;
	}
      catch( const char * ball )
	{
	  std::cout << "The character named \"" << name << "\" was not found: " << ball << std::endl ;
	}
      catch( const RefCountPtr< const char > ball )
	{
	  std::cout << "The character named \"" << name << "\" was not found: " << ball << std::endl ;
	}
    }

  return 0;
}

void
argcAssertion( const char * optionSpecifier, int argc, int argcMin )
{
  if( argc < argcMin )
    {
      std::cerr << "The command line option " << optionSpecifier << " requires " << argcMin - 1 << " parameters." << std::endl ;
      exit( 1 );
    }
}
