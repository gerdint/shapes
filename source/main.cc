#include "simplepdfi.h"
#include "simplepdfo.h"
#include "globals.h"
#include "consts.h"
#include "metapdfexceptions.h"
#include "hottypes.h"
#include "continuations.h"
#include "charconverters.h"
#include "pagecontentstates.h"
#include "autoonoff.h"
#include "metapdfscanner.h"
#include "texlabelmanager.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <unistd.h>
#include <stdlib.h>
#include <list>
#include <time.h>
#include <errno.h>
#include <limits>
#include "iconvselect.h"

int metapdfparse( );
extern int metapdfdebug;
void printVersion( );

using namespace MetaPDF;
using namespace SimplePDF;
using namespace std;

void argcAssertion( const char * optionSpecifier, int argc, int argcMin );
void performIterativeStartup( const std::string & texJobName );
void abortProcedure( ofstream * oFile, const std::string & outputName );
void setupGlobals( );
enum XpdfAction{ XPDF_DEFAULT, XPDF_RAISE, XPDF_RELOAD, XPDF_QUIT, XPDF_NOSERVER };
void xpdfHelper( const std::string & filename, const std::string & server, const XpdfAction & action );
void openHelper( const std::string & filename, const char * application );
void addDefaultNeedPath( );
void addDefaultFontMetricsPath( );
void destroyGlobals( );

namespace MetaPDF
{
  namespace Interaction
  {
    void systemDebugMessage( const std::string & msg );
  }
}

int
main( int argc, char ** argv )
{
  if( ! std::numeric_limits< double >::has_signaling_NaN )
    {
      std::cerr << "The program was built on a platform where double does not have a NaN representative.  Too bad.  Bye." << std::endl ;
      exit( 1 );
    }

  srand( time(NULL) );

  setupGlobals( );

  bool iterativeMode = true;

  string baseName;
  string inputName;
  string outputName;
  string texJobName;
  string fontmetricsOutputName;

  enum FilenameRequests{ FILENAME_RESOURCE, FILENAME_IN, FILENAME_OUT, FILENAME_TEXJOB, FILENAME_AFM };

  list< int > filenameRequestList;
  list< const char * > resourceRequestList;

  bool evalTrace = false;
  bool evalBackTrace = false;
  bool cleanupMemory = true;
  bool memoryStats = false;
  bool launch_xpdf = false;
  SimplePDF::PDF_out::Version pdfVersion = SimplePDF::PDF_out::VERSION_UNDEFINED;
  SimplePDF::PDF_out::VersionAction pdfVersionAction = SimplePDF::PDF_out::WARN;
  XpdfAction xpdfAction = XPDF_DEFAULT;
  string xpdfServer;
  bool do_open = false;
  const char * do_open_application = 0;
  std::ostringstream prependStreamOut;

  argc -= 1;
  argv += 1;
  while( argc > 0 )
    {
      if( strcmp( *argv, "--yydebug" ) == 0 )
	{
	  metapdfdebug = 1;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--metapdfdebug" ) == 0 )
	{
	  metapdfdebug = 1;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--systemdebug" ) == 0 )
	{
	  Interaction::debugSystem = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--afmdebug" ) == 0 )
	{
	  Interaction::fontMetricDebug = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--afmmessages" ) == 0 )
	{
	  Interaction::fontMetricMessages = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--bytecolumn" ) == 0 )
	{
	  Interaction::characterColumnInBytes = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--debugstep" ) == 0 )
	{
	  char * endp;
	  int tmp = strtol( *( argv + 1 ), & endp, 10 );
	  if( tmp < 0 )
	    {
	      std::cerr << "The --debugstep value must be nonnegative: " << tmp << std::endl ;
	      exit( 1 );
	    }
	  Interaction::debugStep = static_cast< size_t >( tmp );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--nodtminerror" ) == 0 )
	{
	  Computation::dtMinIsError = false;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--dtminerror" ) == 0 )
	{
	  Computation::dtMinIsError = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--nofmguesserror" ) == 0 )
	{
	  Computation::fontMetricGuessIsError = false;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--fmguesserror" ) == 0 )
	{
	  Computation::fontMetricGuessIsError = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--evaltrace" ) == 0 )
	{
	  evalTrace = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--evalbacktrace" ) == 0 )
	{
	  evalTrace = true;
	  evalBackTrace = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--nobacktrace" ) == 0 )
	{
	  Interaction::debugBacktrace = false;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--noiteration" ) == 0 )
	{
	  iterativeMode = false;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--stats" ) == 0 )
	{
	  memoryStats = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--nostats" ) == 0 )
	{
	  memoryStats = false;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--nomemclean" ) == 0 )
	{
	  cleanupMemory = false;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--showfiles" ) == 0 )
	{
	  Ast::theMetaPDFScanner.setShowFiles( true );
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--v" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  if( pdfVersion != SimplePDF::PDF_out::VERSION_UNDEFINED )
	    {
	      std::cerr << "Multiply defined pdf version." << std::endl ;
	      exit( 1 );
	    }
	  const char * tmp = *( argv + 1 );
	  switch( *tmp )
	    {
	    case 'e':
	      pdfVersionAction = SimplePDF::PDF_out::ERROR;
	      break;
	    case 'w':
	      pdfVersionAction = SimplePDF::PDF_out::WARN;
	      break;
	    case 's':
	      pdfVersionAction = SimplePDF::PDF_out::SILENT;
	      break;
	    default:
	      std::cerr << "The only allowed action-characters in the pdf version specification are: \"e\" (error), \"w\" (warn), and \"s\" (silent)." << std::endl ;
	      exit( 1 );
	    }
	  ++tmp;
	  if( strcmp( tmp, "1.3" ) == 0 )
	    {
	      pdfVersion = SimplePDF::PDF_out::PDF_1_3;
	    }
	  else if( strcmp( tmp, "1.4" ) == 0 )
	    {
	      pdfVersion = SimplePDF::PDF_out::PDF_1_4;
	    }
	  else
	    {
	      std::cerr << "Unsupported pdf version specification: " << tmp << std::endl ;
	      exit( 1 );
	    }
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--unit" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  Interaction::displayUnitName = *( argv + 1 );

	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--splicingtol" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  char * endp;
	  Computation::theTrixelizeSplicingTol = strtod( *( argv + 1 ), &endp );
	  if( *endp != '\0' )
	    {
	      cerr << "Argument to --splicingtol was not a float: " << *( argv + 1 ) << std::endl ;
	      exit( 1 );
	    }
	  if( Computation::theTrixelizeSplicingTol <= 0 )
	    {
	      cerr << "Argument to --splicingtol not positive: " << Computation::theTrixelizeSplicingTol.offtype< 1, 0 >( ) << std::endl ;
	      exit( 1 );
	    }

	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--overlaptol" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  char * endp;
	  Computation::theTrixelizeOverlapTol = strtod( *( argv + 1 ), &endp );
	  if( *endp != '\0' )
	    {
	      cerr << "Argument to --overlaptol was not a float: " << *( argv + 1 ) << std::endl ;
	      exit( 1 );
	    }
	  if( Computation::theTrixelizeOverlapTol <= 0 )
	    {
	      cerr << "Argument to --overlaptol not positive: " << Computation::theTrixelizeOverlapTol.offtype< 1, 0 >( ) << std::endl ;
	      exit( 1 );
	    }

	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--needpath" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  const char * pth = *( argv + 1 );

	  if( strchr( pth, ':' ) != 0 )
	    {
	      cerr << "The path separator ':' is not allowed in the --needpath argument.  Consider repeating --needpath (or -N)." << std::endl ;
	      exit( 1 );
	    }

	  Ast::theMetaPDFScanner.push_backNeedPath( pth );

	  argv += 2;
	  argc -= 2;
	}
      else if( strncmp( *argv, "-N", 2 ) == 0 )
	{
	  const char * pth = (*argv) + 2;

	  if( strchr( pth, ':' ) != 0 )
	    {
	      cerr << "The path separator ':' is not allowed in the -N argument.  Consider repeating -N." << std::endl ;
	      exit( 1 );
	    }

	  Ast::theMetaPDFScanner.push_backNeedPath( pth );

	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--fontmetricspath" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  const char * pth = *( argv + 1 );

	  if( strchr( pth, ':' ) != 0 )
	    {
	      cerr << "The path separator ':' is not allowed in the --fontmetricspath argument.  Consider repeating --fontmetricspath (or -F)." << std::endl ;
	      exit( 1 );
	    }

	  Lang::Font::push_backFontMetricsPath( pth );

	  argv += 2;
	  argc -= 2;
	}
      else if( strncmp( *argv, "-M", 2 ) == 0 )
	{
	  const char * pth = (*argv) + 2;

	  if( strchr( pth, ':' ) != 0 )
	    {
	      cerr << "The path separator ':' is not allowed in the -M argument.  Consider repeating -M." << std::endl ;
	      exit( 1 );
	    }

	  Lang::Font::push_backFontMetricsPath( pth );

	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--seed" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  char * endp;
	  long s = strtol( *( argv + 1 ), &endp, 10 );
	  if( *endp != '\0' )
	    {
	      cerr << "Argument to --seed was not an integer: " << *( argv + 1 ) << std::endl ;
	      exit( 1 );
	    }

	  srand( s );

	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--arcdelta" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  char * endp;
	  Computation::the_arcdelta = strtod( *( argv + 1 ), &endp );
	  if( *endp != '\0' )
	    {
	      cerr << "Argument to --arcdelta was not a float: " << *( argv + 1 ) << std::endl ;
	      exit( 1 );
	    }
	  if( Computation::the_arcdelta <= 0 )
	    {
	      cerr << "Argument to --arcdelta not positive: " << Computation::the_arcdelta.offtype< 1, 0 >( ) << std::endl ;
	      exit( 1 );
	    }

	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--dtmin" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );

	  char * endp;
	  Computation::the_dtMin = strtod( *( argv + 1 ), &endp );
	  if( *endp != '\0' )
	    {
	      cerr << "Argument to --dtmin was not a float: " << *( argv + 1 ) << std::endl ;
	      exit( 1 );
	    }
	  if( Computation::the_dtMin <= 0 )
	    {
	      cerr << "Argument to --dtmin not positive: " << Computation::the_dtMin << std::endl ;
	      exit( 1 );
	    }

	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--prepend" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  prependStreamOut << *( argv + 1 ) << std::endl ;
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--base" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  if( baseName != "" )
	    {
	      cerr << "The name base is multiply specified." << endl ;
	      exit( 1 );
	    }
	  baseName = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--which" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  filenameRequestList.push_back( FILENAME_RESOURCE );
	  resourceRequestList.push_back( *( argv + 1 ) );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--whichin" ) == 0 )
	{
	  filenameRequestList.push_back( FILENAME_IN );
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--in" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  if( inputName != "" )
	    {
	      cerr << "The input file is multiply specified." << endl ;
	      exit( 1 );
	    }
	  inputName = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--whichout" ) == 0 )
	{
	  filenameRequestList.push_back( FILENAME_OUT );
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--out" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  if( outputName != "" )
	    {
	      cerr << "The output file is multiply specified." << endl ;
	      exit( 1 );
	    }
	  outputName = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--whichtexjob" ) == 0 )
	{
	  filenameRequestList.push_back( FILENAME_TEXJOB );
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--texjob" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  if( texJobName != "" )
	    {
	      cerr << "The tex job name is multiply specified." << endl ;
	      exit( 1 );
	    }
	  texJobName = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--whichafmout" ) == 0 )
	{
	  filenameRequestList.push_back( FILENAME_AFM );
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--afmout" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  if( fontmetricsOutputName != "" )
	    {
	      cerr << "The font metrics output name is multiply specified." << endl ;
	      exit( 1 );
	    }
	  fontmetricsOutputName = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--xpdf" ) == 0 )
	{
	  launch_xpdf = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--xpdf-remote" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  if( texJobName != "" )
	    {
	      cerr << "The xpdf server is multiply specified." << endl ;
	      exit( 1 );
	    }
	  xpdfServer = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--xpdf-noserver" ) == 0 )
	{
	  if( xpdfAction != XPDF_DEFAULT )
	    {
	      cerr << "The xpdf action is multiply specified." << endl ;
	      exit( 1 );
	    }
	  xpdfAction = XPDF_NOSERVER;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--xpdf-reload" ) == 0 )
	{
	  if( xpdfAction != XPDF_DEFAULT )
	    {
	      cerr << "The xpdf action is multiply specified." << endl ;
	      exit( 1 );
	    }
	  xpdfAction = XPDF_RELOAD;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--xpdf-quit" ) == 0 )
	{
	  if( xpdfAction != XPDF_DEFAULT )
	    {
	      cerr << "The xpdf action is multiply specified." << endl ;
	      exit( 1 );
	    }
	  xpdfAction = XPDF_QUIT;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--open" ) == 0 )
	{
	  do_open = true;
	  argv += 1;
	  argc -= 1;
	}
      else if( strcmp( *argv, "--open-a" ) == 0 )
	{
	  argcAssertion( *argv, argc, 2 );
	  do_open = true;
	  do_open_application = *( argv + 1 );
	  argv += 2;
	  argc -= 2;
	}
      else if( strcmp( *argv, "--version" ) == 0 )
	{
	  printVersion( );
	  exit( 0 );
	}
      else if( argc == 1 )
	{
	  if( baseName != "" )
	    {
	      cerr << "The name base is multiply specified." << endl ;
	      exit( 1 );
	    }
	  baseName = *argv;
	  argv += 1;
	  argc -= 1;
	}
      else
	{
	  cerr << "Illegal command line option: " << *argv << endl ;
	  exit( 1 );
	}
    }

  if( baseName == "" )
    {
      if( texJobName == "" )
	{
	  texJobName = "#metapdf.labels";
	}
    }
  else
    {
      if( inputName == "" )
	{
	  inputName = baseName + "drool";
	}
      if( outputName == "" )
	{
	  outputName = baseName + "pdf";
	}
      if( texJobName == "" )
	{
	  texJobName = baseName + "labels";
	}
      if( fontmetricsOutputName == "" )
	{
	  fontmetricsOutputName = baseName + "afm";
	}
    }

  if( Computation::theTrixelizeSplicingTol >= Computation::theTrixelizeOverlapTol )
    {
      std::cerr << "The splicing tolerance (" << Concrete::Length::offtype( Computation::theTrixelizeSplicingTol ) << "bp) must be less than the overlap tolerance (" << Concrete::Length::offtype( Computation::theTrixelizeOverlapTol ) << "bp)." << std::endl ;
      exit( 1 );
    }

  if( xpdfServer == "" )
    {
      xpdfServer = outputName;
    }
  
  if( xpdfAction == XPDF_DEFAULT )
    {
      xpdfAction = XPDF_RAISE;
    }

  addDefaultNeedPath( );
  addDefaultFontMetricsPath( );

  if( filenameRequestList.size( ) > 0 )
    {
      list< const char * >::const_iterator resource = resourceRequestList.begin( );
      for( list< int >::const_iterator i = filenameRequestList.begin( );
	   i != filenameRequestList.end( );
	   ++i )
	{
	  if( i != filenameRequestList.begin( ) )
	    {
	      cout << " " ;
	    }
	  switch( *i )
	    {
	    case FILENAME_IN:
	      if( inputName == "" )
		{
		  cout << "<stdin>" ;
		}
	      else
		{
		  cout << inputName ;
		}
	      break;
	    case FILENAME_OUT:
	      if( outputName == "" )
		{
		  cout << "<stdout>" ;
		}
	      else
		{
		  cout << outputName ;
		}
	      break;
	    case FILENAME_TEXJOB:
	      cout << texJobName ;
	      break;
	    case FILENAME_AFM:
	      cout << fontmetricsOutputName ;
	      break;
	    case FILENAME_RESOURCE:
	      {
		try
		  {
		    cout << Ast::theMetaPDFScanner.searchFile( *resource ) ;
		  }
		catch( const Exceptions::Exception & ball )
		  {
		    std::cout.flush( );
		    ball.display( cerr );
		    exit( 1 );
		  }
		++resource;
	      }
	      break;
	    default:
	      cerr << "Internal error:  filename request switch in main out of range." << endl ;
	      exit( 1 );
	    }
	}
      exit( 0 );
    }

  if( pdfVersion == SimplePDF::PDF_out::VERSION_UNDEFINED )
    {
      pdfVersion = SimplePDF::PDF_out::PDF_1_5;
    }
  
  Kernel::the_pdfo->setVersion( pdfVersion );
  Kernel::the_pdfo->setVersionAction( pdfVersionAction );
  Kernel::the_pdfo->pageResources->requireProcedureSet( SimplePDF::PDF_Resources::PROC_SET_PDF );

  {
    ostringstream oss;
    time_t tmp;
    time( &tmp );
    struct tm * now( gmtime( &tmp ) );
    oss << "D:"
	<< std::setfill( '0' )
	<< std::setw(4) << 1900 + now->tm_year
	<< std::setw(2) << 1 + now->tm_mon
	<< std::setw(2) << now->tm_mday
	<< std::setw(2) << now->tm_hour
	<< std::setw(2) << now->tm_min
	<< std::setw(2) << now->tm_sec
	<< "+0000" ;
    (*Kernel::the_pdfo->info_)[ "CreationDate" ] = SimplePDF::PDF_out::newString( oss.str( ).c_str( ) );
  }

  ifstream iFile;
  if( inputName == "" )
    {
      (*Kernel::the_pdfo->info_)[ "Title" ] = SimplePDF::PDF_out::newString( "<stdin>" );
      Ast::theMetaPDFScanner.switch_streams( & cin, & cerr );
      Ast::theMetaPDFScanner.setNameOf_yyin( "stdin" );
    }
  else
    {
      (*Kernel::the_pdfo->info_)[ "Title" ] = SimplePDF::PDF_out::newString( inputName.c_str( ) );
      iFile.open( inputName.c_str( ) );
      if( ! iFile.good( ) )
	{
	  cerr << "Failed to open " << inputName << " for input." << endl ;
	  exit( 1 );
	}
      Ast::theMetaPDFScanner.switch_streams( & iFile, & cerr );
      Ast::theMetaPDFScanner.setNameOf_yyin( inputName.c_str( ) );
    }

  std::istringstream prependStreamIn;
  if( prependStreamOut.str( ).size( ) > 0 )
    {
      prependStreamIn.str( prependStreamOut.str( ) );
      Ast::theMetaPDFScanner.prependStream( & prependStreamIn );
    }

  ofstream oFile;
  
  if( outputName != "" )
    {
      oFile.open( outputName.c_str( ) );
      if( ! oFile.good( ) )
	{
	  cerr << "Failed to open " << outputName << " for output." << endl ;
	  exit( 1 );
	}
      Kernel::the_pdfo->setOutputStream( & oFile );
    }

  try
    {
      metapdfparse( );
      Ast::theProgram->analyze( );
      // The display unit is looked up after the input is scanned, so the user may use her own units
      Interaction::displayUnitFactor = Ast::theMetaPDFScanner.lookupUnitFactor( Interaction::displayUnitName );
      if( Interaction::displayUnitFactor <= 0 )
	{
	  std::cerr << "Invalid display unit: " << Interaction::displayUnitName << std::endl ;
	  exit( 1 );
	}
      performIterativeStartup( texJobName );
      Kernel::theTeXLabelManager.settexJobName( texJobName );
      Kernel::PassedEnv baseEnv = new Kernel::Environment( Kernel::theEnvironmentList );
      RefCountPtr< const Kernel::GraphicsState > graphicsState( new Kernel::GraphicsState( true ) );
      Kernel::GraphicsState initState( *graphicsState );
      Kernel::PassedDyn baseDyn( new Kernel::DynamicEnvironment( graphicsState ) );

      bool done = false;
      Kernel::EvalState evalState( Ast::theProgram,
				   baseEnv,
				   baseDyn,
				   Kernel::ContRef( new Kernel::ExitVoidContinuation( & done, Ast::theProgram->loc( ) ) ) );
      try
	{
	  while( ! done )
	    {
	      if( evalTrace )
		{
		  if( evalBackTrace )
		    {
		      evalState.cont_->backTrace( std::cerr );
		      std::cerr << "--- Bottom of trace ---" << std::endl ;
		    }
		  else
		    {
		      std::cerr << "Eval trace: Cont: " << evalState.cont_->traceLoc( ) << "  Expr: " << evalState.expr_->loc( ) << std::endl ;
		    }
		}
	      Ast::Expression * expr = evalState.expr_;
	      expr->eval( & evalState );
	    }
	}
      catch( Exceptions::Exception & ball )
	{
	  std::cout.flush( );
	  if( Interaction::debugBacktrace )
	    {
	      evalState.cont_->backTrace( std::cerr );
	    }

	  std::cerr << evalState.cont_->traceLoc( ) << ": " ;
	  ball.display( cerr );
	  abortProcedure( & oFile, outputName );
	}

      RefCountPtr< const Lang::Group2D > finalPicture = dynamic_cast< Kernel::WarmGroup2D * >( baseEnv->getStateHandle( baseEnv->findLocalStatePosition( Ast::THE_UNKNOWN_LOCATION, Lang::CANVAS_ID ) ) )->getPile( );

      // Forcing to synch is a bad thing, due to PDF version differences.  Instead, refer to the PDF documentation
      // on the graphics state dictionary (page 180 in the PDF-1.6 reference) to find out the correct default values,
      // and make sure that these are the initial values of the pdfState.
      Kernel::PageContentStates pdfState( Kernel::the_pdfo->pageResources, true );
      //      pdfState.forceSynchAll( Kernel::the_pdfo->contents->data, & initState );

      finalPicture->shipout( Kernel::the_pdfo->contents->data, & pdfState, Lang::Transform2D( 1, 0, 0, 1, 0, 0 ) );
      RefCountPtr< const Lang::ElementaryPath2D > theBBox = finalPicture->bbox( );

      if( theBBox->size( ) == 0 )
	{
	  throw Exceptions::EmptyFinalPicture( );
	}

      if( cleanupMemory )
	{
	  baseEnv->clear( );
	}
      
      delete baseEnv;
      delete Ast::theProgram;

      Concrete::Coords2D llcorner( 0, 0 );
      Concrete::Coords2D urcorner( 0, 0 );
      theBBox->boundingRectangle( & llcorner, & urcorner );
      Kernel::the_pdfo->mediabox = RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( llcorner.x_.offtype< 1, 0 >( ),
												    llcorner.y_.offtype< 1, 0 >( ),
												    urcorner.x_.offtype< 1, 0 >( ),
												    urcorner.y_.offtype< 1, 0 >( ) ) );
    }
  catch( const Exceptions::Exception & ball )
    {
      std::cout.flush( );
      ball.display( cerr );
      abortProcedure( & oFile, outputName );
    }
  catch( const NonLocalExit::DynamicBindingNotFound & ball )
    {
      cerr << "Caught DynamicBindingNotFound at top level." << endl
	   << "This should really not be possible; it is an internal error." << endl ;
      exit( 1 );
    }
  catch( const NonLocalExit::NotThisType & ball )
    {
      cerr << "Caught NotThisType at top level." << endl
	   << "This should really not be possible; it is an internal error." << endl ;
      exit( 1 );
    }
  catch( const NonLocalExit::NonLocalExitBase & ball )
    {
      cerr << "Caught an unknown descendant to NonLocalExitBase at top level." << endl
	   << "This should really not be possible; it is an internal error." << endl ;
      exit( 1 );
    }
  catch( const char * ball )
    {
      cerr << "Caught (char*) ball at top level:" << endl 
	   << "  " << ball << endl ;
      cerr << "This shit should not be thrown!  Use Exceptions::Exception derivatives!" << endl ;
      exit( 1 );
    }
  catch( const string & ball )
    {
      cerr << "Caught (string) ball at top level:" << endl 
	   << "  " << ball << endl ;
      cerr << "This shit should not be thrown!  Use Exceptions::Exception derivatives!" << endl ;
      exit( 1 );
    }
  catch( ... )
    {
      cerr << "Caught (...) ball at top level." << endl ;
      cerr << "This shit should not be thrown!  Use Exceptions::Exception derivatives!" << endl ;
      exit( 1 );
    }

  if( memoryStats )
    {
      cerr << "Summary:" << endl ;
      cerr << "Environments:  alive: " << Kernel::Environment::liveCount << "  of total: " << Kernel::Environment::createdCount
	   << "  (" << 100 * static_cast< double >( Kernel::Environment::liveCount ) / static_cast< double >( Kernel::Environment::createdCount ) << "%)" << endl ;
    }

  if( iterativeMode )
    {
      Kernel::theTeXLabelManager.iterativeFinish( );
    }

  Kernel::the_pdfo->writeData( );
 
  if( launch_xpdf )
    {
      xpdfHelper( outputName, xpdfServer, xpdfAction );
    }

  if( do_open )
    {
      openHelper( outputName, do_open_application );
    }

  destroyGlobals( );

  return 0;
}


void
argcAssertion( const char * optionSpecifier, int argc, int argcMin )
{
  if( argc < argcMin )
    {
      cerr << "The command line option " << optionSpecifier << " requires " << argcMin - 1 << " parameters." << endl ;
      exit( 1 );
    }
}


void
performIterativeStartup( const std::string & texJobName )
{
  ostringstream oldFilename;
  ostringstream labelsFilename;
  oldFilename    << texJobName << ".pdf" ;
  labelsFilename << texJobName << ".0pdf" ;
  {
    ifstream tmpFile( oldFilename.str( ).c_str( ) );
    if( ! tmpFile.good( ) )
      {
	return;
      }
  }
  {
    ostringstream mvCommand;
    mvCommand << "cp '" << oldFilename.str( ) << "' '" << labelsFilename.str( ) << "'" ;
    Interaction::systemDebugMessage( mvCommand.str( ) );
    if( system( mvCommand.str( ).c_str( ) ) != 0 )
      {
	return;
      }
  }
  RefCountPtr< ifstream > labelsFile( new std::ifstream( labelsFilename.str( ).c_str( ) ) );
  if( ! labelsFile->good( ) )
    {
      return;
    }
  try
    {
      Kernel::theTeXLabelManager.iterativeStartup( labelsFile );
    }
  catch( const char * ball )
    {
      cerr << "Caught (char*) ball from iterative startup:" << endl 
	   << "  " << ball << endl ;
      exit( 1 );
    }
  catch( const string & ball )
    {
      cerr << "Caught (string) ball from iterative startup:" << endl 
	   << "  " << ball << endl ;
      exit( 1 );
    }
  catch( const Exceptions::Exception & ball )
    {
      ball.display( cerr );
      exit( 1 );
    }
  catch( ... )
    {
      cerr << "Caught (...) ball from iterative startup." << endl ;
      exit( 1 );
    }
}


void
abortProcedure( ofstream * oFile, const std::string & outputName )
{
  cerr << "Aborting job.  Deleting output file." << endl ;
  if( outputName != "" )
    {
      oFile->close( );
    }
  {
    ifstream tmpFile( outputName.c_str( ) );
    if( tmpFile.good( ) )
      {
	tmpFile.close( );
	string rmCommand = "rm '" + outputName + "'";
	Interaction::systemDebugMessage( rmCommand );
	if( system( rmCommand.c_str( ) ) != 0 )
	  {
	    /* Never mind; we made a try. */
	  }
      }
  }
  exit( 1 );
}

namespace MetaPDF
{
  namespace Helpers
  {
    void setSelfRef( RefCountPtr< const Lang::Class > cls )
    {
      cls->setSelfRef( cls );
    }
  }
}

void
setupGlobals( )
{
  Lang::ElementaryPath2D * bbox = new Lang::ElementaryPath2D;
  bbox->push_back( new Concrete::PathPoint2D( 0, 0 ) );
  bbox->close( );
  Lang::THE_POINTPICTURE = RefCountPtr< Lang::Drawable2D >( new Lang::BBoxed2D( Lang::THE_NULL2D,
										RefCountPtr< Lang::ElementaryPath2D >( bbox ) ) );
  Helpers::setSelfRef( Lang::THE_OBJECT );

  Helpers::setSelfRef( Lang::Void::TypeID );
  Helpers::setSelfRef( Lang::Symbol::TypeID );
  Helpers::setSelfRef( Lang::Float::TypeID );
  Helpers::setSelfRef( Lang::Length::TypeID );
  Helpers::setSelfRef( Lang::Boolean::TypeID );
  Helpers::setSelfRef( Lang::String::TypeID );
  Helpers::setSelfRef( Lang::FloatPair::TypeID );
  Helpers::setSelfRef( Lang::FloatTriple::TypeID );
  Helpers::setSelfRef( Lang::Coords2D::TypeID );
  Helpers::setSelfRef( Lang::CornerCoords2D::TypeID );
  Helpers::setSelfRef( Lang::Coords3D::TypeID );

  Helpers::setSelfRef( Lang::Function::TypeID );
  Helpers::setSelfRef( Lang::Transform2D::TypeID );
  Helpers::setSelfRef( Lang::Transform3D::TypeID );

  Helpers::setSelfRef( Lang::Class::TypeID );

  Helpers::setSelfRef( Lang::TransformedInstance::TypeID );

  {
    Lang::SystemVirtualInterface * tmp = new Lang::SystemVirtualInterface( strrefdup( "Geometric2D" ) );
    Lang::Geometric2D::TypeID = RefCountPtr< const Lang::Class >( tmp );
    Helpers::setSelfRef( Lang::Geometric2D::TypeID );
    /* Note that addVirtual must not be called before the selfRef is set!
     */
  }
  {
    Lang::SystemVirtualInterface * tmp = new Lang::SystemVirtualInterface( strrefdup( "Geometric3D" ) );
    Lang::Geometric3D::TypeID = RefCountPtr< const Lang::Class >( tmp );
    Helpers::setSelfRef( Lang::Geometric3D::TypeID );
    /* Note that addVirtual must not be called before the selfRef is set!
     */
  }
  {
    Lang::SystemVirtualInterface * tmp = new Lang::SystemVirtualInterface( strrefdup( "Drawable2D" ) );
    Lang::Drawable2D::TypeID = RefCountPtr< const Lang::Class >( tmp );
    Helpers::setSelfRef( Lang::Drawable2D::TypeID );
    /* Note that addVirtual must not be called before the selfRef is set!
     */
    tmp->addVirtual( Lang::MESSAGE_DRAWABLE_DRAW_ID );
  }
  {
    Lang::SystemVirtualInterface * tmp = new Lang::SystemVirtualInterface( strrefdup( "Drawable3D" ) );
    Lang::Drawable3D::TypeID = RefCountPtr< const Lang::Class >( tmp );
    Helpers::setSelfRef( Lang::Drawable3D::TypeID );
    /* Note that addVirtual must not be called before the selfRef is set!
     */
    tmp->addVirtual( Lang::MESSAGE_DRAWABLE_DRAW_ID );
  }
  {
    Lang::SystemVirtualInterface * tmp = new Lang::SystemVirtualInterface( strrefdup( "Color" ) );
    Lang::Color::TypeID = RefCountPtr< const Lang::Class >( tmp );
    Helpers::setSelfRef( Lang::Color::TypeID );
    /* Note that addVirtual must not be called before the selfRef is set!
     */
    tmp->addVirtual( "stroking" );
    tmp->addVirtual( "nonstroking" );
  }
}

void
xpdfHelper( const std::string & filename, const std::string & server, const XpdfAction & action )
{
  pid_t xpdfProcess = fork( );
  if( xpdfProcess == -1 )
    {
      throw Exceptions::InternalError( strrefdup( "Failed to fork a process for running xpdf." ) );
    }
  
  if( xpdfProcess == 0 ) /* This is the child */
    {
      /* The exec call below never returns, so the child process never leaves this if clause.
       * Hence, there is no need to create a special else clasuse below.
       */
      switch( action )
	{
	case XPDF_RAISE:
	  execlp( "xpdf", "xpdf", "-remote", server.c_str( ), "-raise", filename.c_str( ), static_cast< const char * >( 0 ) );
	  break;
	case XPDF_RELOAD:
	  execlp( "xpdf", "xpdf", "-remote", server.c_str( ), "-reload", static_cast< const char * >( 0 ) );
	  break;
	case XPDF_QUIT:
	  execlp( "xpdf", "xpdf", "-remote", server.c_str( ), "-quit", static_cast< const char * >( 0 ) );
	  break;
	case XPDF_NOSERVER:
	  execlp( "xpdf", "xpdf", filename.c_str( ), static_cast< const char * >( 0 ) );
	  break;
	default:
	  cerr << "Internal error: XpdfAction switch out of range." << std::endl ;
	  exit( 1 );
	}
      if( errno != 0 )
	{
	  cerr << "Recieved errno = " << errno << " from execlp call to xpdf." << std::endl ;
	  exit( 1 );
	}
      cerr << "execlp call to xpdf returned with errno == 0." << std::endl ;
      exit( 1 );
    }

}

void
openHelper( const std::string & filename, const char * application )
{
  pid_t openProcess = fork( );
  if( openProcess == -1 )
    {
      throw Exceptions::InternalError( strrefdup( "Failed to fork a process for running open." ) );
    }
  
  if( openProcess == 0 ) /* This is the child */
    {
      /* The exec call below never returns, so the child process never leaves this if clause.
       * Hence, there is no need to create a special else clasuse below.
       */
      if( application != 0 )
	{
	  execlp( "open", "open", "-a", application, filename.c_str( ), static_cast< const char * >( 0 ) );
	}
      else
	{
	  execlp( "open", "open", filename.c_str( ), static_cast< const char * >( 0 ) );
	}
      if( errno != 0 )
	{
	  cerr << "Recieved errno = " << errno << " from execlp call to open." << std::endl ;
	  exit( 1 );
	}
      cerr << "execlp call to open returned with errno == 0." << std::endl ;
      exit( 1 );
    }
}


void
Interaction::systemDebugMessage( const std::string & msg )
{
  if( Interaction::debugSystem )
    {
      std::cerr << "System command: " << msg << std::endl ;
    }
}

void
addDefaultNeedPath( )
{
  char * start = getenv( "DROOLINPUTS" );
  char * tok = strsep( & start, ":" );
  while( tok != 0 )
    {
      Ast::theMetaPDFScanner.push_backNeedPath( tok );
      tok = strsep( & start, ":" );
    }
}

void
addDefaultFontMetricsPath( )
{
  char * start = getenv( "DROOLFONTMETRICS" );
  char * tok = strsep( & start, ":" );
  while( tok != 0 )
    {
      Lang::Font::push_backFontMetricsPath( tok );
      tok = strsep( & start, ":" );
    }
}

void
destroyGlobals( )
{
  Helpers::requireUTF8ToMacRomanConverter( true ); // true means "cleanup"
  Helpers::requireMacRomanToUTF8Converter( true ); // true means "cleanup"
  Helpers::requireUTF8ToWinANSIConverter( true );  // true means "cleanup"
  Helpers::requireUTF8ToASCIIConverter( true );  // true means "cleanup"
  Helpers::requireUTF8ToUCS4Converter( true );  // true means "cleanup"
  Helpers::requireUTF16BEToUCS4Converter( true );  // true means "cleanup"
  Helpers::requireGlyphList( true );  // true means "cleanup"
  Helpers::requireMacRomanEncoding( true );  // true means "cleanup"
}
