/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

#include "Shapes_Kernel_decls.h"

#include "simplepdfi.h"
#include "simplepdfo.h"
#include "globals.h"
#include "consts.h"
#include "shapesexceptions.h"
#include "hottypes.h"
#include "multipage.h"
#include "continuations.h"
#include "charconverters.h"
#include "pagecontentstates.h"
#include "autoonoff.h"
#include "shapesscanner.h"
#include "texlabelmanager.h"
#include "debuglog.h"

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
#include <iconv.h>

int shapesparse( );
extern int shapesdebug;
void printVersion( );

using namespace Shapes;
using namespace SimplePDF;

void argcAssertion( const char * optionSpecifier, int argc, int argcMin );
bool strprefixcmp( char * str, const char * prefix, char ** endp );
bool strtobool( const char * str, const char * containingString, const char * trueLabel = 0, const char * falseLabel = 0 );
std::string callDir;
std::string absoluteFilename( const char * filename );
std::string absoluteDirectory( const char * filename );
void ensureTmpDirectoryExists( const std::string & dirname, bool allowCreate );
RefCountPtr< std::ifstream > performIterativeStartup( const std::string & texJobName );
void abortProcedure( int exitCode );
void setupGlobals( );
enum XpdfAction{ XPDF_DEFAULT, XPDF_RAISE, XPDF_RELOAD, XPDF_QUIT, XPDF_NOSERVER };
void xpdfHelper( const std::string & filename, const std::string & server, const XpdfAction & action );
void openHelper( const std::string & filename, const char * application );
void addDefaultNeedPath( );
void addDefaultFontMetricsPath( );
void destroyGlobals( );
void escapeExtGlobChars( const std::string & str, std::ostream & dst );

namespace Shapes
{
	namespace Interaction
	{
		void systemDebugMessage( const std::string & msg );
	}
}

int
main( int argc, char ** argv )
{
	srand( time(NULL) );

	setupGlobals( );

	bool iterativeMode = true;
	bool useResources = true;

	{
		char * cwd = getcwd( 0, 0 );
		callDir = cwd + std::string( "/" );
		free( cwd );
	}

	std::string outDir;
	std::string tmpDir;
	bool allowCreateTmpDir = false;
	std::string baseName;
	std::string inputName;
	std::string outputName;
	std::string texJobName;
	std::string labelDBName;
	std::string fontmetricsOutputName;

	enum FilenameRequests{ FILENAME_RESOURCE, FILENAME_IN, FILENAME_OUT, FILENAME_TMP, FILENAME_TEXJOB, FILENAME_LABELDB, FILENAME_AFM, FILENAME_TEXINPUTS, FILENAME_HTMLDOC };

	std::list< int > filenameRequestList;
	std::list< const char * > resourceRequestList;

	bool evalTrace = false;
	bool evalBackTrace = false;
	bool cleanupMemory = true;
	bool memoryStats = false;
	bool launch_xpdf = false;
	SimplePDF::PDF_Version::Version pdfVersion = SimplePDF::PDF_Version::VERSION_UNDEFINED;
	SimplePDF::PDF_Version::Action pdfVersionAction = SimplePDF::PDF_Version::WARN;
	XpdfAction xpdfAction = XPDF_DEFAULT;
	std::string xpdfServer;
	bool do_open = false;
	const char * do_open_application = 0;
	std::ostringstream prependStreamOut;

	enum SplitMode{ SPLIT_NO, SPLIT_FLAT, SPLIT_DIR };
	SplitMode splitMode = SPLIT_NO;

	argc -= 1;
	argv += 1;
	while( argc > 0 )
		{
			char * optionSuffix;
			if( strcmp( *argv, "--yydebug" ) == 0 )
				{
					shapesdebug = 1;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--shapes-debug" ) == 0 )
				{
					shapesdebug = 1;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--system-debug" ) == 0 )
				{
					Interaction::debugSystem = true;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--afm-debug" ) == 0 )
				{
					Interaction::fontMetricDebug = true;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--afm-messages" ) == 0 )
				{
					Interaction::fontMetricMessages = true;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--tex-debug" ) == 0 )
				{
					Interaction::pdfLaTeXInteractionTo_stderr = true;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--log-globals" ) == 0 )
				{
					Interaction::logGlobals = true;
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--bytecolumn=", & optionSuffix ) )
				{
					Interaction::characterColumnInBytes = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--debugstep=", & optionSuffix ) )
				{
					char * endp;
					int tmp = strtol( optionSuffix, & endp, 10 );
					if( tmp < 0 )
						{
							std::cerr << "The --debugstep value must be nonnegative: " << optionSuffix << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					Interaction::debugStep = static_cast< size_t >( tmp );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--debuglog" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					try
						{
							Kernel::theDebugLog.setFilename( *( argv + 1 ) );
						}
					catch( const char * ball )
						{
							std::cerr << ball << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--debuglog-stderr" ) == 0 )
				{
					try
						{
							Kernel::theDebugLog.setStream( & std::cerr );
						}
					catch( const char * ball )
						{
							std::cerr << ball << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--debuglog-stdout" ) == 0 )
				{
					try
						{
							Kernel::theDebugLog.setStream( & std::cout );
						}
					catch( const char * ball )
						{
							std::cerr << ball << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--dtminerror=", & optionSuffix ) )
				{
					Computation::dtMinIsError = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--fmguesserror=", & optionSuffix ) )
				{
					Computation::fontMetricGuessIsError = strtobool( optionSuffix, *argv );
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
			else if( strprefixcmp( *argv, "--backtrace=", & optionSuffix ) )
				{
					Interaction::debugBacktrace = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--iteration=", & optionSuffix ) )
				{
					iterativeMode = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--resources=", & optionSuffix ) )
				{
					useResources = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--stats=", & optionSuffix ) )
				{
					memoryStats = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--memclean=", & optionSuffix ) )
				{
					cleanupMemory = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--showfiles=", & optionSuffix ) )
				{
					Ast::theShapesScanner.setShowFiles( strtobool( optionSuffix, *argv ) );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--pdf-version=", & optionSuffix ) || /* Note that we use that || shortcuts! */
							 strprefixcmp( *argv, "-v", & optionSuffix ) )
				{
					if( pdfVersion != SimplePDF::PDF_Version::VERSION_UNDEFINED )
						{
							std::cerr << "Multiply defined pdf version." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					switch( *optionSuffix )
						{
						case 'e':
							pdfVersionAction = SimplePDF::PDF_Version::ERROR;
							break;
						case 'w':
							pdfVersionAction = SimplePDF::PDF_Version::WARN;
							break;
						case 's':
							pdfVersionAction = SimplePDF::PDF_Version::SILENT;
							break;
						default:
							std::cerr << "The only allowed action-characters in the pdf version specification are: \"e\" (error), \"w\" (warn), and \"s\" (silent).  You said \"" << *optionSuffix << "\", being the first character in \"" << optionSuffix << "\"." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					++optionSuffix;
					if( strncmp( optionSuffix, "1.", 2 ) == 0 &&
							'1' <= optionSuffix[2] && optionSuffix[2] <= '6' )
						{
							SimplePDF::PDF_Version::Version versions[] = { SimplePDF::PDF_Version::PDF_1_1, SimplePDF::PDF_Version::PDF_1_2, SimplePDF::PDF_Version::PDF_1_3, SimplePDF::PDF_Version::PDF_1_4, SimplePDF::PDF_Version::PDF_1_5, SimplePDF::PDF_Version::PDF_1_6 };
							pdfVersion = versions[ optionSuffix[2] - '1' ];
						}
					else if( strcmp( optionSuffix, "X" ) == 0 )
						{
							std::cerr << "Restriction to PDF-X is not implemented, please try using a low version number, such as 1.1 instead." << std::endl ;
						}
					else
						{
							std::cerr << "Unsupported pdf version specification: " << optionSuffix << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--tp=", & optionSuffix ) )
				{
					Kernel::allowTransparency = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--spot-pair=", & optionSuffix ) )
				{
					Kernel::allowSingletonPaths = ! strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--unit=", & optionSuffix ) )
				{
					Interaction::displayUnitName = optionSuffix;

					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--splicingtol=", & optionSuffix ) )
				{
					try
						{
							Computation::theTrixelizeSplicingTol = Ast::theShapesScanner.strtoLength( optionSuffix );
						}
					catch( ... )
						{
							std::cerr << "Argument to --splicing= was not recognized as a length: " << optionSuffix << std::endl ;
							abortProcedure( Interaction::EXIT_INVOCATION_ERROR );
						}
					if( Computation::theTrixelizeSplicingTol <= 0 )
						{
							std::cerr << "Argument to --splicingtol not positive: " << Computation::theTrixelizeSplicingTol.offtype< 1, 0 >( ) << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--overlaptol=", & optionSuffix ) )
				{
					try
						{
							Computation::theTrixelizeOverlapTol = Ast::theShapesScanner.strtoLength( optionSuffix );
						}
					catch( ... )
						{
							std::cerr << "Argument to --overlaptol= was not recognized as a length: " << optionSuffix << std::endl ;
							abortProcedure( Interaction::EXIT_INVOCATION_ERROR );
						}
					if( Computation::theTrixelizeOverlapTol <= 0 )
						{
							std::cerr << "Argument to --overlaptol not positive: " << Computation::theTrixelizeOverlapTol.offtype< 1, 0 >( ) << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--needpath" ) == 0 ||
							 strncmp( *argv, "-N", 2 ) == 0 )
				{
					bool longForm = strncmp( *argv, "--", 2 ) == 0;

					const char * pth = 0;
					if( longForm )
						{
							argcAssertion( *argv, argc, 2 );
							pth = *( argv + 1 );
						}
					else
						{
							pth = (*argv) + 2;
						}

					if( strchr( pth, ':' ) != 0 )
						{
							const char * flag = 0;
							const char * shortFlag = "-N";
							if( longForm )
								{
									flag = *argv;
								}
							else
								{
									flag = shortFlag;
								}

							std::cerr << "The path separator ':' is not allowed in the " << flag << " argument.	Consider repeating " << flag ;
							if( longForm )
								{
									std::cerr << " (or " << shortFlag << ")" ;
								}
							std::cerr <<"." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					Ast::theShapesScanner.push_backNeedPath( absoluteDirectory( pth ) );

					if( longForm )
						{
							argv += 2;
							argc -= 2;
						}
					else
						{
							argv += 1;
							argc -= 1;
						}
				}
			else if( strcmp( *argv, "--fontmetricspath" ) == 0 ||
							 strncmp( *argv, "-M", 2 ) == 0 )
				{
					bool longForm = strncmp( *argv, "--", 2 ) == 0;

					const char * pth = 0;
					if( longForm )
						{
							argcAssertion( *argv, argc, 2 );
							pth = *( argv + 1 );
						}
					else
						{
							pth = (*argv) + 2;
						}

					if( strchr( pth, ':' ) != 0 )
						{
							const char * flag = 0;
							const char * shortFlag = "-M";
							if( longForm )
								{
									flag = *argv;
								}
							else
								{
									flag = shortFlag;
								}

							std::cerr << "The path separator ':' is not allowed in the " << flag << " argument.	Consider repeating " << flag ;
							if( longForm )
								{
									std::cerr << " (or " << shortFlag << ")" ;
								}
							std::cerr <<"." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					Lang::Font::push_backFontMetricsPath( absoluteDirectory( pth ) );

					if( longForm )
						{
							argv += 2;
							argc -= 2;
						}
					else
						{
							argv += 1;
							argc -= 1;
						}
				}
			else if( strprefixcmp( *argv, "--seed=", & optionSuffix ) )
				{
					char * endp;
					long s = strtol( optionSuffix, &endp, 10 );
					if( *endp != '\0' )
						{
							std::cerr << "Argument to --seed= was not an integer: " << optionSuffix << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					srand( s );

					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--arcdelta=", & optionSuffix ) )
				{
					try
						{
							Computation::the_arcdelta = Ast::theShapesScanner.strtoLength( optionSuffix );
						}
					catch( ... )
						{
							std::cerr << "Argument to --arcdelta= was not recognized as a length: " << optionSuffix << std::endl ;
							abortProcedure( Interaction::EXIT_INVOCATION_ERROR );
						}
					if( Computation::the_arcdelta <= 0 )
						{
							std::cerr << "Argument to --arcdelta= not positive: " << optionSuffix << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--dtmin=", & optionSuffix ) )
				{
					char * endp;
					Computation::the_dtMin = strtod( optionSuffix, &endp );
					if( *endp != '\0' )
						{
							std::cerr << "Argument to --dtmin= was not a float: " << optionSuffix << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					if( Computation::the_dtMin <= 0 )
						{
							std::cerr << "Argument to --dtmin= not positive: " << Computation::the_dtMin << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}

					argv += 1;
					argc -= 1;
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
							std::cerr << "The name base is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
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
			else if( strcmp( *argv, "--in" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					if( inputName != "" )
						{
							std::cerr << "The input file is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					inputName = absoluteFilename( *( argv + 1 ) );
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--which-in" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_IN );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--out" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					if( outputName != "" )
						{
							std::cerr << "The output file is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					outputName = absoluteFilename( *( argv + 1 ) );
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--which-out" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_OUT );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--texjob" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					if( texJobName != "" )
						{
							std::cerr << "The tex job name is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					texJobName = *( argv + 1 );
					if( texJobName.find( '/' ) != std::string::npos )
						{
							std::cerr << "The tex job name may not include directory specification.  Please use --tmpdir to set the directory where the tex job is carried out." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--which-texjob" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_TEXJOB );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--labeldb" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					if( labelDBName != "" )
						{
							std::cerr << "The label database file is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					labelDBName = absoluteFilename( *( argv + 1 ) );
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--which-labeldb" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_LABELDB );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--afmout" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					if( fontmetricsOutputName != "" )
						{
							std::cerr << "The font metrics output name is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					fontmetricsOutputName = absoluteFilename( *( argv + 1 ) );
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--which-afmout" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_AFM );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--which-TEXINPUTS" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_TEXINPUTS );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--which-doc" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_HTMLDOC );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--outdir" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					if( outDir != "" )
						{
							std::cerr << "The output directory is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					outDir = absoluteDirectory( *( argv + 1 ) );
					argv += 2;
					argc -= 2;
				}
			else if( strprefixcmp( *argv, "--tmp*=", & optionSuffix ) )
				{
					allowCreateTmpDir = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--tmpdir" ) == 0 )
				{
					argcAssertion( *argv, argc, 2 );
					if( tmpDir != "" )
						{
							std::cerr << "The temporaries directory is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					tmpDir = absoluteDirectory( *( argv + 1 ) );
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--which-tmp" ) == 0 )
				{
					filenameRequestList.push_back( FILENAME_TMP );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--split=", & optionSuffix ) )
				{
					if( strcmp( optionSuffix, "no" ) == 0 )
						{
							splitMode = SPLIT_NO;
						}
					else if( strcmp( optionSuffix, "flat" ) == 0 )
						{
							splitMode = SPLIT_FLAT;
						}
					else if( strcmp( optionSuffix, "dir" ) == 0 )
						{
							splitMode = SPLIT_DIR;
						}
					else
						{
							std::cerr << "The string \"" << optionSuffix << "\" in the command line argument \"" << *argv << "\" was not any of { 'no', 'flat', 'dir' }." << std::endl ;
						}
					argv += 1;
					argc -= 1;
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
					if( xpdfServer != "" )
						{
							std::cerr << "The xpdf server is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					xpdfServer = *( argv + 1 );
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--xpdf-no-server" ) == 0 )
				{
					if( xpdfAction != XPDF_DEFAULT )
						{
							std::cerr << "The xpdf action is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					xpdfAction = XPDF_NOSERVER;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--xpdf-reload" ) == 0 )
				{
					if( xpdfAction != XPDF_DEFAULT )
						{
							std::cerr << "The xpdf action is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					xpdfAction = XPDF_RELOAD;
					argv += 1;
					argc -= 1;
				}
			else if( strcmp( *argv, "--xpdf-quit" ) == 0 )
				{
					if( xpdfAction != XPDF_DEFAULT )
						{
							std::cerr << "The xpdf action is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
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
					exit( Interaction::EXIT_OK );
				}
			else if( argc == 1 )
				{
					if( baseName != "" )
						{
							std::cerr << "The name base is multiply specified." << std::endl ;
							exit( Interaction::EXIT_INVOCATION_ERROR );
						}
					struct stat theStat;
					if( stat( *argv, & theStat ) == 0 &&
							( theStat.st_mode & S_IFDIR ) == 0 ) /* We are not interested in directories here. */
						{
							inputName = *argv;
							char * ext = *argv + strlen( *argv ) - 6;
							if( ext <= *argv )
								{
									std::cerr << "The file name \"" << *argv << "\" is unexpectedly short (it should include the \".shape\" suffix)." << std::endl ;
									exit( Interaction::EXIT_INVOCATION_ERROR );
								}
							if( strcmp( ext, ".shape" ) != 0 )
								{
									std::cerr << "Expected \".shape\" suffix in the file name \"" << *argv << "\"." << std::endl ;
									exit( Interaction::EXIT_INVOCATION_ERROR );
								}
						}
					else
						{
							if( (*argv)[ strlen( *argv ) - 1 ] == '.' )
								{
									inputName = std::string( *argv ) + "shape";
								}
							else
								{
									inputName = std::string( *argv ) + ".shape";
								}
							if( ! stat( inputName.c_str( ), & theStat ) == 0 )
								{
									/* It is not entirely clear what is the best error message here,
									 * as the source file may be specified in several different ways.
									 * This should cause the least confusion.
									 */
									std::cerr << "Failed to locate input file: " << *argv << std::endl ;
									exit( Interaction::EXIT_INPUT_FILE_ERROR );
								}
						}

					const char * slash = strrchr( inputName.c_str( ), '/' );
					if( slash == 0 )
						{
							slash = inputName.c_str( );
						}
					else
						{
							++slash;
						}
					size_t skipCount = slash - inputName.c_str( );
					baseName = inputName.substr( skipCount, inputName.length( ) - skipCount - 6 );

					argv += 1;
					argc -= 1;
				}
			else
				{
					std::cerr << "Illegal command line option: " << *argv << std::endl ;
					exit( Interaction::EXIT_INVOCATION_ERROR );
				}
		}

	if( outDir == "" )
		{
			outDir = absoluteDirectory( "" );
		}
	if( tmpDir == "" )
		{
			char * start = getenv( "SHAPESTMPDIR" );
			if( start != 0 )
				{
					tmpDir = absoluteDirectory( start );
				}
			else
				{
					tmpDir = absoluteDirectory( "" );
				}
		}
	ensureTmpDirectoryExists( tmpDir, allowCreateTmpDir );

	if( baseName == "" )
		{
			if( texJobName == "" )
				{
					texJobName = "#shapes.labels";
				}
		}
	else
		{
			if( inputName == "" )
				{
					inputName = absoluteFilename( ( baseName + ".shape" ).c_str( ) );
				}
			if( outputName == "" )
				{
					if( splitMode == SPLIT_NO )
						{
							outputName = outDir + baseName + ".pdf";
						}
					else
						{
							outputName = outDir + baseName;
						}
				}
			if( texJobName == "" )
				{
					texJobName = baseName + ".labels";
				}
			if( labelDBName == "" )
				{
					labelDBName = outDir + baseName + ".labels.pdf";
				}
			if( fontmetricsOutputName == "" )
				{
					fontmetricsOutputName = outDir + baseName + ".afm";
				}
		}

	if( outputName == "" )
		{
			if( ! filenameRequestList.empty( ) )
				{
					/* The output name will never really be used, so it's rather harmless to assign a dummy value.
					 */
					outputName = "?.pdf" ;
				}
			else
				{
					std::cerr << "The output file is undetermined.  Consider specifying it using \"--out <filename>\"." << std::endl ;
					exit( Interaction::EXIT_INVOCATION_ERROR );
				}
		}

	if( labelDBName == "" )
		{
			iterativeMode = false;
		}

	if( ! Kernel::theDebugLog.initialized( ) )
		{
			std::string::size_type suffixSep = outputName.rfind( '.' );
			if( suffixSep != std::string::npos && outputName.find( '/', suffixSep ) == std::string::npos )
				{
					/* If there would have been a slash after the '.', the dot would have been part of a directory name.
					 * Otherwise, we conclude that we have found the extension of a filename, and replace that extension
					 * by ".log".
					 */
					Kernel::theDebugLog.setFilename( outputName.substr( 0, suffixSep ) + ".log" );
				}
			else if( baseName != "" )
				{
					Kernel::theDebugLog.setFilename( outDir + baseName + ".log" );
				}
			else
				{
					Kernel::theDebugLog.setFilename( outDir + "#shapes.log" );
				}
		}

	{
		std::string inDir;
		std::string inPath = inputName;
		std::string::size_type slash = inPath.rfind( '/' );
		if( slash == std::string::npos )
			{
				inDir = absoluteDirectory( "" );
			}
		else
			{
				inDir = absoluteDirectory( inPath.substr( 0, slash ).c_str( ) );
			}
		Ast::theShapesScanner.setSourceDir( inDir );
		Kernel::theTeXLabelManager.setup( inDir, tmpDir, texJobName );
	}

	if( Computation::theTrixelizeSplicingTol >= Computation::theTrixelizeOverlapTol )
		{
			std::cerr << "The splicing tolerance (" << Concrete::Length::offtype( Computation::theTrixelizeSplicingTol ) << "bp) must be less than the overlap tolerance (" << Concrete::Length::offtype( Computation::theTrixelizeOverlapTol ) << "bp)." << std::endl ;
			exit( Interaction::EXIT_INVOCATION_ERROR );
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

#ifdef RESOURCES_DIR
	if( useResources )
		{
			Ast::theShapesScanner.push_backNeedPath( std::string( RESOURCES_DIR ) + "/extensions/" );
			Lang::Font::push_backFontMetricsPath( std::string( RESOURCES_DIR ) + "/fontmetrics/" );
		}
#endif

	if( ! filenameRequestList.empty( ) )
		{
			std::list< const char * >::const_iterator resource = resourceRequestList.begin( );
			for( std::list< int >::const_iterator i = filenameRequestList.begin( );
					 i != filenameRequestList.end( );
					 ++i )
				{
					switch( *i )
						{
						case FILENAME_IN:
							if( inputName == "" )
								{
									std::cout << "<stdin>" ;
								}
							else
								{
									std::cout << inputName ;
								}
							break;
						case FILENAME_OUT:
							std::cout << outputName ;
							break;
						case FILENAME_TMP:
							std::cout << tmpDir ;
							break;
						case FILENAME_TEXJOB:
							std::cout << tmpDir << texJobName ;
							break;
						case FILENAME_LABELDB:
							std::cout << labelDBName ;
							break;
						case FILENAME_AFM:
							std::cout << fontmetricsOutputName ;
							break;
						case FILENAME_TEXINPUTS:
							{
								std::cout << getenv( "TEXINPUTS" ) ;
							}
							break;
						case FILENAME_HTMLDOC:
							{
								std::cout << HTMLDIR << "/index.html" ;
							}
							break;
						case FILENAME_RESOURCE:
							{
								try
									{
										std::cout << Ast::theShapesScanner.searchFile( *resource ) ;
									}
								catch( const Exceptions::Exception & ball )
									{
										std::cout.flush( );
										ball.display( std::cerr );
										exit( Interaction::EXIT_INVOCATION_ERROR );
									}
								++resource;
							}
							break;
						default:
							std::cerr << "Internal error:	filename request switch in main out of range." << std::endl ;
							exit( Interaction::EXIT_INTERNAL_ERROR );
						}
					std::cout << std::endl ;
				}
			exit( Interaction::EXIT_OK );
		}

	if( pdfVersion == SimplePDF::PDF_Version::VERSION_UNDEFINED )
		{
			pdfVersion = SimplePDF::PDF_Version::PDF_1_4;
		}

	Kernel::the_PDF_version.setVersion( pdfVersion );
	Kernel::the_PDF_version.setAction( pdfVersionAction );

	{
		std::ostringstream oss;
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
		Kernel::theDocInfo.addInfo( "CreationDate", SimplePDF::newString( oss.str( ).c_str( ) ) );
	}

	std::ifstream iFile;
	if( inputName == "" )
		{
			Kernel::theDocInfo.addInfo( "Title", SimplePDF::newString( "<stdin>" ) );
			Ast::theShapesScanner.switch_streams( & std::cin, & std::cerr );
			Ast::theShapesScanner.setNameOf_yyin( "stdin" );
		}
	else
		{
			Kernel::theDocInfo.addInfo( "Title", SimplePDF::newString( inputName.c_str( ) ) );
			iFile.open( inputName.c_str( ) );
			if( ! iFile.good( ) || ! iFile.is_open( ) )
				{
					std::cerr << "Failed to open " << inputName << " for input." << std::endl ;
					exit( Interaction::EXIT_INPUT_FILE_ERROR );
				}
			Ast::theShapesScanner.switch_streams( & iFile, & std::cerr );
			Ast::theShapesScanner.setNameOf_yyin( inputName.c_str( ) );
		}

	std::istringstream prependStreamIn;
	if( ! prependStreamOut.str( ).empty( ) )
		{
			prependStreamIn.str( prependStreamOut.str( ) );
			Ast::theShapesScanner.prependStream( & prependStreamIn );
		}

	RefCountPtr< std::ifstream > labelDBFile = RefCountPtr< std::ifstream >( NullPtr< std::ifstream >( ) );
	Kernel::WarmCatalog::ShipoutList documents;

	try
		{
			shapesparse( );
			Kernel::theGlobalEnvironment = new Kernel::Environment( Kernel::theEnvironmentList );
			Kernel::registerGlobals( Kernel::theGlobalEnvironment );
			Kernel::registerDynamic( Kernel::theGlobalEnvironment );
			Kernel::registerHot( Kernel::theGlobalEnvironment );
			Kernel::registerClasses( Kernel::theGlobalEnvironment );
			Kernel::registerCore_elem( Kernel::theGlobalEnvironment );
			Kernel::registerCore_point( Kernel::theGlobalEnvironment );
			Kernel::registerCore_path( Kernel::theGlobalEnvironment );
			Kernel::registerCore_draw( Kernel::theGlobalEnvironment );
			Kernel::registerCore_construct( Kernel::theGlobalEnvironment );
			Kernel::registerCore_font( Kernel::theGlobalEnvironment );
			Kernel::registerCore_misc( Kernel::theGlobalEnvironment );
			Kernel::registerCore_state( Kernel::theGlobalEnvironment );
			Kernel::registerCore_annotation( Kernel::theGlobalEnvironment );
			Ast::theGlobalAnalysisEnvironment = Kernel::theGlobalEnvironment->newAnalysisEnvironment( );
			Ast::theProgram->analyze( 0, Ast::theGlobalAnalysisEnvironment );
			if( ! Ast::theAnalysisErrorsList.empty( ) )
				{
					std::cout.flush( );
					typedef typeof Ast::theAnalysisErrorsList ListType;
					for( ListType::const_iterator i = Ast::theAnalysisErrorsList.begin( ); i != Ast::theAnalysisErrorsList.end( ); ++i )
						{
							{
								typedef const Exceptions::StaticInconsistency ErrorType;
								ErrorType * err = dynamic_cast< ErrorType * >( *i );
								if( err != 0 )
									{
										std::cerr << err->loc( ) << ": " ;
										err->display( std::cerr );
										continue;
									}
							}
							std::cerr << "(Bad exception type)" << ": " ;
							(*i)->display( std::cerr );
						}
					abortProcedure( Interaction::EXIT_USER_ERROR );
				}

			// The display unit is looked up after the input is scanned, so the user may use her own units
			Interaction::displayUnitFactor = Ast::theShapesScanner.lookupUnitFactor( Interaction::displayUnitName );
			if( Interaction::displayUnitFactor <= 0 )
				{
					std::cerr << "Invalid display unit: " << Interaction::displayUnitName << std::endl ;
					abortProcedure( Interaction::EXIT_INVOCATION_ERROR );
				}
			labelDBFile = performIterativeStartup( labelDBName );
			RefCountPtr< const Kernel::GraphicsState > graphicsState( new Kernel::GraphicsState( true ) );
			Kernel::PassedDyn baseDyn( new Kernel::DynamicEnvironment( graphicsState ) );

			bool done = false;
			Kernel::EvalState evalState( Ast::theProgram,
																	 Kernel::theGlobalEnvironment,
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
											std::cerr << "Eval trace: Cont: " << evalState.cont_->traceLoc( ) << "	Expr: " << evalState.expr_->loc( ) << std::endl ;
										}
								}
							Ast::Expression * expr = evalState.expr_;
							expr->eval( & evalState );
						}
				}
			catch( const Exceptions::StaticInconsistency & ball )
				{
					std::cout.flush( );
					std::cerr << ball.loc( ) << ": " ;
					ball.display( std::cerr );
					abortProcedure( ball.exitCode( ) );
				}
			catch( Exceptions::Exception & ball )
				{
					std::cout.flush( );
					if( Interaction::debugBacktrace )
						{
							evalState.cont_->backTrace( std::cerr );
						}

					std::cerr << evalState.cont_->traceLoc( ) << Exceptions::Exception::locsep ;
					ball.display( std::cerr );
					abortProcedure( ball.exitCode( ) );
				}

			Kernel::WarmCatalog * catalog = dynamic_cast< Kernel::WarmCatalog * >( Kernel::theGlobalEnvironment->getStateHandle( Ast::theGlobalAnalysisEnvironment->findLocalStatePosition( Ast::THE_UNKNOWN_LOCATION, Lang::CATALOG_ID ) ) );
			RefCountPtr< const Lang::Group2D > finalPicture = dynamic_cast< Kernel::WarmGroup2D * >( Kernel::theGlobalEnvironment->getStateHandle( Ast::theGlobalAnalysisEnvironment->findLocalStatePosition( Ast::THE_UNKNOWN_LOCATION, Lang::CANVAS_ID ) ) )->getPile( );
			if( catalog->isEmpty( ) && finalPicture->isNull( ) )
				{
					throw Exceptions::EmptyFinalPicture( );
				}
			if( catalog->isEmpty( ) )
				{
					catalog->tackOnPage( baseDyn, finalPicture, Ast::THE_UNKNOWN_LOCATION );
				}

			catalog->shipout( splitMode != SPLIT_NO, & documents );

			if( cleanupMemory )
				{
					Kernel::theGlobalEnvironment->clear( );
				}

			delete Kernel::theGlobalEnvironment;
			delete Ast::theProgram;
		}
	catch( const Exceptions::StaticInconsistency & ball )
		{
			std::cout.flush( );
			std::cerr << ball.loc( ) << ": " ;
			ball.display( std::cerr );
			abortProcedure( ball.exitCode( ) );
		}
	catch( const Exceptions::Exception & ball )
		{
			std::cout.flush( );
			ball.display( std::cerr );
			abortProcedure( ball.exitCode( ) );
		}
	catch( const NonLocalExit::DynamicBindingNotFound & ball )
		{
			std::cerr << "Caught DynamicBindingNotFound at top level." << std::endl
								<< "This should really not be possible; it is an internal error." << std::endl ;
			exit( Interaction::EXIT_INTERNAL_ERROR );
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			std::cerr << "Caught NotThisType at top level." << std::endl
								<< "This should really not be possible; it is an internal error." << std::endl ;
			exit( Interaction::EXIT_INTERNAL_ERROR );
		}
	catch( const NonLocalExit::NonLocalExitBase & ball )
		{
			std::cerr << "Caught an unknown descendant to NonLocalExitBase at top level." << std::endl
								<< "This should really not be possible; it is an internal error." << std::endl ;
			exit( Interaction::EXIT_INTERNAL_ERROR );
		}
	catch( const char * ball )
		{
			std::cerr << "Caught (char*) ball at top level:" << std::endl
								<< "	" << ball << std::endl ;
			exit( Interaction::EXIT_GENERIC_ERROR );
		}
	catch( const std::string & ball )
		{
			std::cerr << "Caught (string) ball at top level:" << std::endl
								<< "	" << ball << std::endl ;
			exit( Interaction::EXIT_GENERIC_ERROR );
		}
	catch( ... )
		{
			std::cerr << "Caught (...) ball at top level." << std::endl ;
			exit( Interaction::EXIT_GENERIC_ERROR );
		}

	if( ! Kernel::thePostCheckErrorsList.empty( ) )
		{
			abortProcedure( Interaction::EXIT_USER_ERROR );
		}

	if( memoryStats )
		{
			std::cerr << "Summary:" << std::endl ;
			std::cerr << "Environments:	alive: " << Kernel::Environment::liveCount << "	of total: " << Kernel::Environment::createdCount
					 << "	(" << 100 * static_cast< double >( Kernel::Environment::liveCount ) / static_cast< double >( Kernel::Environment::createdCount ) << "%)" << std::endl ;
		}

	switch( splitMode )
		{
		case SPLIT_NO:
			{
				if( documents.size( ) != 1 )
					{
						std::cerr << "Internal error: Failed to produce exactly one document of output although --split=no." << std::endl ;
					}
				std::ofstream oFile;
				oFile.open( outputName.c_str( ) );
				if( ! oFile.good( ) )
					{
						/* If this is because the output directory does not exist, we shall inform the user about this. */
						std::string::size_type slash = outputName.rfind( '/' );
						if( slash != std::string::npos )
							{
								std::string outputDir = outputName.substr( 0, slash );
								struct stat theStat;
								if( stat( outputDir.c_str( ), & theStat ) == 0 )
									{
										if( ( theStat.st_mode & S_IFDIR ) == 0 )
											{
												std::cerr << "The prefix " << outputDir << " of the output name must be a directory." << std::endl ;
												exit( Interaction::EXIT_NO_DIRECTORY_ERROR );
											}
										/* In case we reach here, the directory exists, so we fall back on the generic error message below. */
									}
								else
									{
										std::cerr << "The file " << outputName << " cannot be opened for output since the directory " << outputDir << " does not exist." << std::endl ;
										exit( Interaction::EXIT_NO_DIRECTORY_ERROR );
									}
							}
						std::cerr << "Failed to open " << outputName << " for output." << std::endl ;
						exit( Interaction::EXIT_OUTPUT_FILE_ERROR );
					}
				documents.front( ).writeFile( oFile, Kernel::the_PDF_version );
			}
			break;
		case SPLIT_FLAT:
			{
				std::ostringstream rmCommand;
				rmCommand << "sh -O extglob -c 'rm -f " ;
				escapeExtGlobChars( outputName, rmCommand );
				rmCommand << "-+([0-9]).pdf'" ;
				Interaction::systemDebugMessage( rmCommand.str( ) );
				if( system( rmCommand.str( ).c_str( ) ) != 0 )
					{
						/* Never mind; we made a try, and this probably means that there were no files to remove. */
					}
				size_t physicalPageNo = 1;
				for( Kernel::WarmCatalog::ShipoutList::iterator i = documents.begin( ); i != documents.end( ); ++i, ++physicalPageNo )
					{
						std::ostringstream tmpFilename;
						tmpFilename << outputName << "-" << physicalPageNo << ".pdf" ;
						std::ofstream oFile;
						oFile.open( tmpFilename.str( ).c_str( ) );
						if( ! oFile.good( ) )
							{
								std::cerr << "Failed to open " << tmpFilename.str( ) << " for output." << std::endl ;
								exit( Interaction::EXIT_OUTPUT_FILE_ERROR );
							}
						i->writeFile( oFile, Kernel::the_PDF_version );
					}
			}
			break;
		case SPLIT_DIR:
			{
				struct stat theStat;
				if( stat( outputName.c_str( ), & theStat ) == 0 )
					{
						if( ( theStat.st_mode & S_IFDIR ) == 0 )
							{
								std::cerr << "The path " << outputName << " was expected to reference a directory." << std::endl ;
								exit( Interaction::EXIT_NO_DIRECTORY_ERROR );
							}
					}
				else
					{
						if( mkdir( outputName.c_str( ), S_IRWXU | S_IRWXG | S_IRWXO ) != 0 )
							{
								std::cerr << "Failed to create directory for split document files (errno=" << errno << "): " << outputName << std::endl ;
								exit( Interaction::EXIT_OUTPUT_FILE_ERROR );
							}
					}
				std::ostringstream rmCommand;
				rmCommand << "sh -O extglob -c 'rm -f " ;
				escapeExtGlobChars( outputName, rmCommand );
				rmCommand << "/+([0-9]).pdf'" ;
				Interaction::systemDebugMessage( rmCommand.str( ) );
				if( system( rmCommand.str( ).c_str( ) ) != 0 )
					{
						/* Never mind; we made a try, and this probably means that there were no files to remove. */
					}
				size_t physicalPageNo = 1;
				for( Kernel::WarmCatalog::ShipoutList::iterator i = documents.begin( ); i != documents.end( ); ++i, ++physicalPageNo )
					{
						std::ostringstream tmpFilename;
						tmpFilename << outputName << "/" << physicalPageNo << ".pdf" ;
						std::ofstream oFile;
						oFile.open( tmpFilename.str( ).c_str( ) );
						if( ! oFile.good( ) )
							{
								std::cerr << "Failed to open " << tmpFilename.str( ) << " for output." << std::endl ;
								exit( Interaction::EXIT_OUTPUT_FILE_ERROR );
							}
						i->writeFile( oFile, Kernel::the_PDF_version );
					}
			}
			break;
		}

	/* This must be done after the output has been written, and before iterativeFinish writes to the labels database file.
	 */
	Kernel::thePDFImporter.free( );

	if( labelDBFile != NullPtr< std::ifstream >( ) )
		{
			if( labelDBFile->is_open( ) )
				{
					labelDBFile->close( );
				}
			labelDBFile = NullPtr< std::ifstream >( ); // Free the reference.
		}

	if( iterativeMode )
		{
			Kernel::theTeXLabelManager.iterativeFinish( labelDBName );
		}

	if( launch_xpdf )
		{
			if( splitMode != SPLIT_NO )
				{
					std::cerr << "Warning: not launching viewer since the documet was split" << std::endl ;
				}
			else
				{
					xpdfHelper( outputName, xpdfServer, xpdfAction );
				}
		}

	if( do_open )
		{
			if( splitMode != SPLIT_NO )
				{
					std::cerr << "Warning: not launching viewer since the documet was split" << std::endl ;
				}
			else
				{
					openHelper( outputName, do_open_application );
				}
		}

	destroyGlobals( );

	return 0;
}


void
argcAssertion( const char * optionSpecifier, int argc, int argcMin )
{
	if( argc < argcMin )
		{
			std::cerr << "The command line option " << optionSpecifier << " requires " << argcMin - 1 << " parameters." << std::endl ;
			exit( Interaction::EXIT_INVOCATION_ERROR );
		}
}

bool
strprefixcmp( char * str, const char * prefix, char ** endp )
{
	int len = strlen( prefix );
	bool res = ( strncmp( str, prefix, len ) == 0 );
	*endp = str + len;
	return res;
}

bool
strtobool( const char * str, const char * containingString, const char * trueLabel, const char * falseLabel )
{
	if( trueLabel != 0 &&
			strcmp( str, trueLabel ) == 0 )
		{
			return true;
		}
	if( falseLabel != 0 &&
			strcmp( str, falseLabel ) == 0 )
		{
			return false;
		}
	if( strcmp( str, "yes" ) == 0 ||
			strcmp( str, "true" ) == 0 ||
			strcmp( str, "on" ) == 0 )
		{
			return true;
		}
	if( strcmp( str, "no" ) == 0 ||
			strcmp( str, "false" ) == 0 ||
			strcmp( str, "off" ) == 0)
		{
			return false;
		}
	std::cerr << "The string \"" << str << "\" in the command line argument \"" << containingString << "\" was not recognized as a boolean value." << std::endl ;
	exit( Interaction::EXIT_INVOCATION_ERROR );
}


RefCountPtr< std::ifstream >
performIterativeStartup( const std::string & labelDBName )
{
	{
		struct stat theStat;
		if( stat( labelDBName.c_str( ), & theStat ) != 0 )
			{
				return RefCountPtr< std::ifstream >( NullPtr< std::ifstream >( ) );
			}
	}
//	 {
//		 ostringstream mvCommand;
//		 mvCommand << "cp '" << oldFilename.str( ) << "' '" << labelDBName.str( ) << "'" ;
//		 Interaction::systemDebugMessage( mvCommand.str( ) );
//		 if( system( mvCommand.str( ).c_str( ) ) != 0 )
//			 {
//				 return RefCountPtr< std::ifstream >( NullPtr< std::ifstream >( ) );
//			 }
//	 }
	RefCountPtr< std::ifstream > labelsFile( new std::ifstream( labelDBName.c_str( ) ) );
	if( ! labelsFile->good( ) )
		{
			return RefCountPtr< std::ifstream >( NullPtr< std::ifstream >( ) );
		}
	try
		{
			Kernel::theTeXLabelManager.iterativeStartup( labelsFile );
			return labelsFile;
		}
	catch( const char * ball )
		{
			std::cerr << "Caught (char*) ball from iterative startup:" << std::endl
								<< "	" << ball << std::endl ;
			exit( Interaction::EXIT_GENERIC_ERROR );
		}
	catch( const std::string & ball )
		{
			std::cerr << "Caught (string) ball from iterative startup:" << std::endl
								<< "	" << ball << std::endl ;
			exit( Interaction::EXIT_GENERIC_ERROR );
		}
	catch( const Exceptions::Exception & ball )
		{
			ball.display( std::cerr );
			exit( ball.exitCode( ) );
		}
	catch( ... )
		{
			std::cerr << "Caught (...) ball from iterative startup." << std::endl ;
			exit( Interaction::EXIT_GENERIC_ERROR );
		}
}


void
abortProcedure( int exitCode )
{
	if( ! Kernel::thePostCheckErrorsList.empty( ) )
		{
			std::cout.flush( );
			while( ! Kernel::thePostCheckErrorsList.empty( ) )
				{
					Exceptions::Exception * e = Kernel::thePostCheckErrorsList.front( );
					Kernel::thePostCheckErrorsList.pop_front( );
					{
						typedef const Exceptions::PostCondition ErrorType;
						ErrorType * err = dynamic_cast< ErrorType * >( e );
						if( err != 0 )
							{
								std::cerr << err->loc( ) << ": " ;
								err->display( std::cerr );
								continue;
							}
					}
					{
						typedef const Exceptions::RuntimeError ErrorType;
						ErrorType * err = dynamic_cast< ErrorType * >( e );
						if( err != 0 )
							{
								std::cerr << err->getLoc( ) << " (runtime): " ;
								err->display( std::cerr );
								continue;
							}
					}
					std::cerr << "(Bad post-exception type)" << ": " ;
					e->display( std::cerr );
				}
		}
	std::cerr << "Aborting job.	 Output files are left unchanged." << std::endl ;
	exit( exitCode );
}

namespace Shapes
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
					std::cerr << "Internal error: XpdfAction switch out of range." << std::endl ;
					exit( Interaction::EXIT_INTERNAL_ERROR );
				}
			if( errno != 0 )
				{
					std::cerr << "Recieved errno = " << errno << " from execlp call to xpdf." << std::endl ;
					exit( Interaction::EXIT_EXTERNAL_ERROR );
				}
			std::cerr << "execlp call to xpdf returned with errno == 0." << std::endl ;
			exit( Interaction::EXIT_INTERNAL_ERROR );
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
					std::cerr << "Recieved errno = " << errno << " from execlp call to open." << std::endl ;
					exit( Interaction::EXIT_EXTERNAL_ERROR );
				}
			std::cerr << "execlp call to open returned with errno == 0." << std::endl ;
			exit( Interaction::EXIT_INTERNAL_ERROR );
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
	char * start = getenv( "SHAPESINPUTS" );
	if( start == 0 )
		{
			Ast::theShapesScanner.push_backNeedPath( "./" );
			return;
		}
	char * tok = strsep( & start, ":" );
	while( tok != 0 )
		{
			Ast::theShapesScanner.push_backNeedPath( tok );
			tok = strsep( & start, ":" );
		}
}

void
addDefaultFontMetricsPath( )
{
	char * start = getenv( "SHAPESFONTMETRICS" );
	if( start == 0 )
		{
			return;
		}
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
	Helpers::requireUTF8ToWinANSIConverter( true );	// true means "cleanup"
	Helpers::requireUTF8ToASCIIConverter( true );	// true means "cleanup"
	Helpers::requireUTF8ToUCS4Converter( true );	// true means "cleanup"
	Helpers::requireUTF16BEToUCS4Converter( true );	// true means "cleanup"
	Helpers::requireGlyphList( true );	// true means "cleanup"
	Helpers::requireMacRomanEncoding( true );	// true means "cleanup"
}

std::string
absoluteFilename( const char * filename )
{
	if( *filename == '/' )
		{
			return filename;
		}
	return callDir + filename;
}

std::string
absoluteDirectory( const char * filename )
{
	if( *filename == '\0' )
		{
			return callDir;
		}
	if( filename[ strlen( filename ) - 1 ] != '/' )
		{
			if( *filename == '/' )
				{
					return filename + std::string( "/" );
				}
			return callDir + filename + "/";
		}
	if( *filename == '/' )
		{
			return filename;
		}
	return callDir + filename;
}

#include <iomanip>

void
ensureTmpDirectoryExists( const std::string & dirname, bool allowCreate )
{
	struct stat theStat;
	if( stat( dirname.c_str( ), & theStat ) == 0 )
		{
			if( ( theStat.st_mode & S_IFDIR ) == 0 )
				{
					std::cerr << "The path " << dirname << " was expected to reference a directory." << std::endl ;
					exit( Interaction::EXIT_NO_DIRECTORY_ERROR );
				}
			//				if( ( theStat.st_mode & S_IWOTH ) == 0 )
			//					{
			//						std::cerr << "The directory " << dirname << " was expected have write permission for others." << std::endl ;
			//						exit( Interaction::EXIT_FILE_PERMISSION_ERROR );
			//					}
			return;
		}

	if( ! allowCreate )
		{
			std::cerr << "The directory for temporaries, " << dirname << ", does not exist and is not allowed to be created.  Consider using --tmpdir+ instead of --tmpdir ." << std::endl ;
			exit( Interaction::EXIT_NO_DIRECTORY_ERROR );
		}

	size_t i2 = 0; /* We know there's a slash at the first position */
	i2 = dirname.find( '/', i2 + 1 );
	bool atRoot = true;
	while( stat( dirname.substr( 0, i2 ).c_str( ), & theStat ) == 0 )
		{
			atRoot = false;
			i2 = dirname.find( '/', i2 + 1 );
		}
	if( atRoot )
		{
			std::cerr << "Shapes will not create directories for temporary files at the root: " << dirname << std::endl ;
			exit( Interaction::EXIT_INVOCATION_ERROR );
		}

	mode_t oldUmask = umask( 0 ); /* We want to be able to create directories with any permissions. */
	while( i2 != std::string::npos )
		{
			if( mkdir( dirname.substr( 0, i2 ).c_str( ), theStat.st_mode & ( S_IRWXU | S_IRWXG | S_IRWXO ) ) != 0 )
				{
					std::cerr << "Failed to create directory for temporary files (errno=" << errno << "): " << dirname.substr( 0, i2 ) << std::endl ;
					exit( Interaction::EXIT_OUTPUT_FILE_ERROR );
				}
			i2 = dirname.find( '/', i2 + 1 );
		}
	umask( oldUmask );
}

void
escapeExtGlobChars( const std::string & str, std::ostream & dst )
{
	const char * special = "*?[+@!";
	for( std::string::const_iterator i = str.begin( ); i != str.end( ); ++i )
		{
			if( strchr( special, *i ) != 0 )
				{
					dst << '\\' ;
				}
			dst << *i ;
		}
}
