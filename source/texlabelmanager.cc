#include "texlabelmanager.h"
#include "dynamicenvironment.h"
#include "simplepdfo.h"
#include "simplepdfi.h"
#include "shapesexceptions.h"
#include "globals.h"
#include "texscanner.h"

#include <fstream>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <iomanip>

using namespace std;
using namespace Shapes;

namespace Shapes
{
	namespace Interaction
	{
		void systemDebugMessage( const std::string & msg );
	}
}


Kernel::TeXLabelManager::TeXLabelManager( )
	: anyLabelMiss( false ), loadCount( 0 ), jobNumber( 0 ), documentclass( "article" ), lmodernT1( true ), utf8( true ), setupFinalized( false )
{ }


Kernel::TeXLabelManager::~TeXLabelManager( )
{ }


std::string
Kernel::TeXLabelManager::stringWithJobNumber( const std::string & str ) const
{
	ostringstream res;
	res << str ;
	if( jobNumber >= 0 )
		{
			res << jobNumber ;
		}
	return res.str( );
}


void
Kernel::TeXLabelManager::settexJobName( const std::string & _texJobName )
{
	texJobName = _texJobName;
}


void
Kernel::TeXLabelManager::announce( const std::string & str, const Ast::SourceLocation & loc )
{
	if( isAllBlank( str.c_str( ) ) )
		{
			return;
		}

	if( availableLabels.find( safeSourceHash( str ) ) == availableLabels.end( ) )
		{
			typedef typeof currentRequests RequestMapType;
			currentRequests.insert( RequestMapType::value_type( str, RequestLocation( true, loc ) ) );
		}
}


RefCountPtr< const Lang::Value >
Kernel::TeXLabelManager::request( const std::string & str, const Ast::SourceLocation & loc, Kernel::PassedDyn dyn )
{
	if( isAllBlank( str.c_str( ) ) )
		{
			return static_cast< RefCountPtr< const Lang::Geometric2D > >( Lang::THE_POINTPICTURE );
		}

	allRequests.insert( str );
	string hash = safeSourceHash( str );
	MapType::iterator i = availableLabels.find( hash );
	if( i == availableLabels.end( ) )
		{
			++jobNumber;
			anyLabelMiss = true;
			typedef typeof currentRequests RequestMapType;
			currentRequests.insert( RequestMapType::value_type( str, RequestLocation( false, loc ) ) );
			processRequests( );
			string extendedName = stringWithJobNumber( texJobName ) + ".pdf";
			RefCountPtr< ifstream > iFile = RefCountPtr< ifstream >( new ifstream( extendedName.c_str( ) ) );
			if( ! iFile->good( ) )
				{
					throw Exceptions::InternalError( strrefdup( "Failed to open " + extendedName + " for input." ) );
				}
			loadLabels( iFile );
			i = availableLabels.find( hash );
		}
	if( i == availableLabels.end( ) )
		{
			throw Exceptions::InternalError( strrefdup( "Failed to find generated label: " + str ) );
		}
	return static_cast< RefCountPtr< const Lang::Geometric2D > >( i->second->cloneWithState( dyn->getGraphicsState( ) ) );
}

namespace Shapes
{
	namespace Kernel
	{
		typedef std::pair< std::string, Kernel::TeXLabelManager::RequestLocation > T;
		class RequestsOrder : public std::binary_function< T, T, bool >
		{
		public:
			result_type operator () ( const first_argument_type & x1, const second_argument_type & x2 )
			{
				return x1.first < x2.first;
			}
		};
	}
}

void
Kernel::TeXLabelManager::iterativeStartup( RefCountPtr< std::istream > labelsFile )
{
	try
		{
			loadLabels( labelsFile );

			/* Remove the labels already generated from the request set.
				 Since the available set is hashed, one must search the request set for elements in the available set, rather than vice versa.
				 Process in two steps:	1) find the intersection 2) remove it.
			*/
			typedef typeof currentRequests T;
			T removeSet;
			for( T::iterator i = currentRequests.begin( ); i != currentRequests.end( ); ++i )
				{
					if( availableLabels.find( safeSourceHash( i->first ) ) != availableLabels.end( ) )
						{
							removeSet.insert( *i );
						}
				}
			T tmp;
			set_difference( currentRequests.begin( ), currentRequests.end( ),
											removeSet.begin( ), removeSet.end( ),
											insert_iterator< T >( tmp, tmp.begin( ) ),
											Kernel::RequestsOrder( ) );
			currentRequests = tmp;
		}
	catch( const Exceptions::TeXSetupHasChanged & ball )
		{
			/* never mind */
		}
}


void
Kernel::TeXLabelManager::iterativeFinish( const std::string & labelDBFilename )
{
	if( ! anyLabelMiss )
		{
			return;
		}
	if( loadCount == 1 )
		{
			if( jobNumber == 0 )
				{
					// We have only used old labels.
					return;
				}
			// Otherwise, the only generated pdf file with labels is a good database.

			string lastJobFilename = stringWithJobNumber( texJobName ) + ".pdf";

			{
				ostringstream mvCommand;
				{
					// Avoid invoking the mv command if the file does not exist.
					struct stat theStat;
					if( stat( lastJobFilename.c_str( ), & theStat ) != 0 )
						{
							return;
						}
				}

				if( lastJobFilename == labelDBFilename )
					{
						// Move would yield a warning since we're not really moving anything.
						return;
					}

				mvCommand << "mv '" << lastJobFilename << "' '" << labelDBFilename << "'";
				Interaction::systemDebugMessage( mvCommand.str( ) );
				if( system( mvCommand.str( ).c_str( ) ) != 0 )
					{
						// Never mind
					}
			}
			return;
		}

	for( std::set< std::string >::const_iterator i = allRequests.begin( );
			 i != allRequests.end( );
			 ++i )
		{
			typedef typeof currentRequests RequestMapType;
			currentRequests.insert( RequestMapType::value_type( *i, RequestLocation( true, Ast::THE_UNKNOWN_LOCATION ) ) );
		}
	jobNumber = -1;
	processRequests( );

	{
		string finalJobFilename = stringWithJobNumber( texJobName ) + ".pdf";
		ostringstream mvCommand;
		{
			// Avoid invoking the mv command if the file does not exist.
			struct stat theStat;
			if( stat( finalJobFilename.c_str( ), & theStat ) != 0 )
				{
					return;
				}
		}

		if( finalJobFilename == labelDBFilename )
			{
				// Move would yield a warning since we're not really moving anything.
				return;
			}

		mvCommand << "mv '" << finalJobFilename << "' '" << labelDBFilename << "'";
		Interaction::systemDebugMessage( mvCommand.str( ) );
		if( system( mvCommand.str( ).c_str( ) ) != 0 )
			{
				// Never mind
			}
	}
}

namespace Shapes
{
	namespace Kernel
	{
		template< class T >
		class ClearOnExit
		{
			T * container_;
		public:
			ClearOnExit( T * container )
				: container_( container )
			{	}
			~ClearOnExit( )
			{
				container_->clear( );
			}
		};
	}
}

void
Kernel::TeXLabelManager::processRequests( )
{
	string extendedName = stringWithJobNumber( texJobName ) + ".tex";
	ofstream texFile( extendedName.c_str( ) );
	if( ! texFile.good( ) )
		{
			throw Exceptions::InternalError( strrefdup( "Failed to open the TeX source file for write." ) );
		}

	if( ! setupFinalized )
		{
			compileSetupCode( );
		}

	texFile << setupCode ;
	texFile << "\\btexetexthing{" << "Shapes setup info" << "}{" << safeSourceHash( setupCode ) << "}{-1}" << endl ;

	{
		size_t labelIndex = 0;
		typedef typeof currentRequests RequestMapType;
		for( RequestMapType::const_iterator i = currentRequests.begin( );
				 i != currentRequests.end( );
				 ++i, ++labelIndex )
			{
				assertBalanced( i->first, i->second );
				texFile << "\\btexetexthing{" << i->first << "}{" << safeSourceHash( i->first ) << "}{" << labelIndex << "}" << endl ;
			}
	}
	ClearOnExit< typeof currentRequests > autoClear( & currentRequests );

	texFile << "\\end{document}" << endl ;

	pid_t latexProcess = fork( );
	if( latexProcess == -1 )
		{
			throw Exceptions::InternalError( strrefdup( "Failed to fork a process for running LaTeX." ) );
		}

	if( latexProcess == 0 ) /* This is the child */
		{
			/* The exec call below never returns, so the child process never leaves this if clause.
			 * Hence, there is no need to create a special else clasuse below.
			 */
			string extendedName = stringWithJobNumber( texJobName );
			const char * lastSlashPtr = strrchr( extendedName.c_str( ), '/' );
			if( lastSlashPtr != 0 )
				{
					size_t lastSlashPos = lastSlashPtr - extendedName.c_str( );
					const char * filenameStart = lastSlashPtr + 1;
					if( *filenameStart == '\0' )
						{
							ostringstream oss;
							oss << "The TeX jobname looks like a directory: " << extendedName;
							throw Exceptions::InternalError( strrefdup( oss ) );
						}
					string dirPart = extendedName.substr( 0, lastSlashPos );
					Interaction::systemDebugMessage( "cd " + dirPart );
					if( chdir( dirPart.c_str( ) ) != 0 )
						{
							switch( errno )
								{
								case ENOENT:
									throw Exceptions::InternalError( strrefdup( "Attempt to change to TeX-job directory resulted in the no-entry failure." ) );
								case ENOTDIR:
									throw Exceptions::InternalError( strrefdup( "Attempt to change to TeX-job directory resulted in the not-a-directory failure." ) );
								case EACCES:
									throw Exceptions::InternalError( strrefdup( "Attempt to change to TeX-job directory resulted in the no-search-permission failure." ) );
								case ELOOP:
									throw Exceptions::InternalError( strrefdup( "Attempt to change to TeX-job directory resulted in the symbolic-link-loop failure." ) );
								case EIO:
									throw Exceptions::InternalError( strrefdup( "Attempt to change to TeX-job directory resulted in the I/O failure." ) );
								default:
									throw Exceptions::InternalError( strrefdup( "Attempt to change to TeX-job directory resulted in an unclassified failure." ) );
								}
						}
					extendedName = extendedName.substr( lastSlashPos + 1 );
				}
			if( Interaction::pdfLaTeXInteractionTo_stderr )
				{
					dup2( 2, 1 );
				}
			else
				{
					std::string stdout_filename = ( extendedName + ".stdout" );
					int stdout_file = open( stdout_filename.c_str( ), O_WRONLY | O_CREAT,
																	S_IRUSR | S_IWUSR );
					if( stdout_file == -1 )
						{
							throw Exceptions::ExternalError( strrefdup( "Failed to open file for what pdfLaTeX writes to stdout: " + stdout_filename ) );
						}
					dup2( stdout_file, 1 );
					close( stdout_file );
				}
			execlp( "pdflatex", "pdflatex", "-interaction", "nonstopmode", extendedName.c_str( ), static_cast< const char * >( 0 ) );
			if( errno != 0 )
				{
					ostringstream oss;
					oss << "Recieved errno = " << errno << " from execlp call to pdfLaTeX." ;
					throw Exceptions::InternalError( strrefdup( oss ) );
				}
			throw Exceptions::InternalError( strrefdup( "execlp call to pdfLaTeX returned with errno == 0." ) );
		}

	while( true )
		{
			int status;
			pid_t wpid = waitpid( latexProcess, & status, 0 );
			if( wpid == 0 )
				{
					continue;
				}
			if( wpid == -1 )
				{
					if( errno == EINTR )
						{
							throw Exceptions::InternalError( strrefdup( "Signal recieved while waiting for pdfLaTeX process." ) );
						}
					else
						{
							throw Exceptions::InternalError( strrefdup( "waitpid failure when waiting for pdfLaTeX process." ) );
						}
				}

			if( WIFEXITED( status ) )
				{
					if( WEXITSTATUS( status ) != 0 )
						{
							if( Interaction::pdfLaTeXInteractionTo_stderr )
								{
									throw Exceptions::TeXLabelError( false, "(--tex-debug)", strrefdup( "The output from pdfLaTeX was written to stderr." ), RefCountPtr< const char >( NullPtr< const char >( ) ), Ast::THE_UNKNOWN_LOCATION );
								}

							std::string stdoutFilename = stringWithJobNumber( texJobName ) + ".stdout";
							std::ifstream stdoutFile( stdoutFilename.c_str( ) );
							if( ! stdoutFile.good( ) )
								{
									throw Exceptions::InternalError( strrefdup( "Failed to open the file where pdfLaTeX's output to stdout should have been saved: " + stdoutFilename ) );
								}
							parseTeXErrors( stdoutFile );
						}
					break;
				}
			else if( WIFSIGNALED( status ) )
				{
					ostringstream oss;
					oss << "pdfLaTeX was killed by the signal " << WTERMSIG( status ) << "." ;
					throw Exceptions::InternalError( strrefdup( oss ) );
				}
			else if( WIFSTOPPED( status ) )
				{
					ostringstream oss;
					oss << "pdfLaTeX was stopped by the signal " << WSTOPSIG( status ) << "." ;
					throw Exceptions::InternalError( strrefdup( oss ) );
				}
			else
				{		/* Non-standard case -- may never happen */
					ostringstream oss;
					oss << "waitpid for the pdfLaTeX process resulted in the unexpected status 0x" << ios::hex << status << "." ;
					throw Exceptions::InternalError( strrefdup( oss ) );
				}
		}

}

void
Kernel::TeXLabelManager::loadLabels( RefCountPtr< std::istream > labelsFile )
{
	if( ! setupFinalized )
		{
			compileSetupCode( );
		}
	++loadCount;
	RefCountPtr< SimplePDF::PDF_in > pdfi = RefCountPtr< SimplePDF::PDF_in >( new SimplePDF::PDF_in( labelsFile ) );

	Kernel::the_pdfo->importBtexEtexThings( pdfi, & availableLabels, setupCodeHash );
}

void
Kernel::TeXLabelManager::setDocumentClass( const Ast::SourceLocation & loc, const char * str )
{
	assertNotSetupFinalized( loc );
	documentclass = str;
}

void
Kernel::TeXLabelManager::addDocumentOption( const Ast::SourceLocation & loc, const char * str )
{
	assertNotSetupFinalized( loc );
	documentoptions.push_back( str );
}

void
Kernel::TeXLabelManager::setlmodernT1( const Ast::SourceLocation & loc, bool val )
{
	assertNotSetupFinalized( loc );
	lmodernT1 = val;
}

void
Kernel::TeXLabelManager::setutf8( const Ast::SourceLocation & loc, bool val )
{
	assertNotSetupFinalized( loc );
	utf8 = val;
}

void
Kernel::TeXLabelManager::addPreambleLine( const Ast::SourceLocation & loc, const char * line )
{
	assertNotSetupFinalized( loc );
	preamble << line << endl ;
}

void
Kernel::TeXLabelManager::addDocumentTopLine( const Ast::SourceLocation & loc, const char * line )
{
	assertNotSetupFinalized( loc );
	documentTop << line << endl ;
}

void
Kernel::TeXLabelManager::assertNotSetupFinalized( const Ast::SourceLocation & loc ) const
{
	if( setupFinalized )
		{
			throw Exceptions::TeXSetupTooLate( loc );
		}
}


std::string
Kernel::TeXLabelManager::safeSourceHash( const std::string & str )
{
	ostringstream oss;
	char tmp[3];
	oss << setfill( '0' );
	for( const char * src = str.c_str( ); *src != '\0'; ++src )
		{
			if( isspace( *src ) )
				{
					oss << "Eb" ;
					continue;
				}
			if( *src == 'E' )
				{
					oss << "EE" ;
					continue;
				}
			if( isalnum( *src ) )
				{
					oss << *src ;
					continue;
				}
			sprintf( tmp, "%02x", *reinterpret_cast< const unsigned char * >( src ) );
			oss << "Ex" << tmp ;
		}
	return oss.str( );
}


void
Kernel::TeXLabelManager::compileSetupCode( )
{
	setupFinalized = true;

	ostringstream res;
	res << "\\documentclass" ;
	if( ! documentoptions.empty( ) )
		{
			res << "[" ;
			std::list< std::string >::const_iterator i = documentoptions.begin( );
			res << *i ;
			for( ++i; i != documentoptions.end( ); ++i )
				{
					res << "," << *i ;
				}
			res << "]" ;
		}
	res << "{" << documentclass << "}" << endl ;
	if( lmodernT1 )
		{
			res << "\\usepackage{lmodern}" << endl ;
			res << "\\usepackage[T1]{fontenc}" << endl ;
		}
	if( utf8 )
		{
			res << "\\usepackage[utf8]{inputenc}" << endl ;
		}
	res << preamble.str( ) ;
	res << "\\newcommand{\\btexetexthing}[3]{%" << endl ;
	res << "  \\message{[SHAPES LABEL: #3]}" << endl ;
	res << "	\\immediate\\pdfobj stream {#2}" << endl ;
	res << "	\\setbox0 = \\hbox{#1}%" << endl ;
	res << "	\\pdfxform" << endl ;
	res << "		attr {/TeXht /\\the\\ht0 \\space /TeXdp /\\the\\dp0 \\space /TeXwd /\\the\\wd0 \\space /TeXsrc \\the\\pdflastobj \\space 0 R}" << endl ;
	res << "		resources{ }" << endl ;
	res << "		0" << endl ;
	res << "	\\shipout\\hbox{\\pdfrefxform\\pdflastxform}" << endl ;
	res << "}" << endl ;

	res << "\\begin{document}" << endl ;
	res << documentTop.str( ) ;
	setupCode = res.str( );
	setupCodeHash = safeSourceHash( setupCode );
}


bool
Kernel::TeXLabelManager::isAllBlank( const char * str )
{
	for( ; *str != '\0'; ++str )
		{
			if( ! isspace( *str ) )
				{
					return false;
				}
		}
	return true;
}

void
Kernel::TeXLabelManager::assertBalanced( const std::string & str, const Kernel::TeXLabelManager::RequestLocation & loc ) const
{
	static TeXScanner scanner;
	try
		{
			scanner.check( str );
		}
	catch( RefCountPtr< const char > msg )
		{
			if( loc.literal_ )
				{
					throw Exceptions::StaticTeXLabelError( false, "label", msg, RefCountPtr< const char >( NullPtr< const char >( ) ), loc.loc_ );
				}
			else
				{
					throw Exceptions::TeXLabelError( false, "label", msg, RefCountPtr< const char >( NullPtr< const char >( ) ), loc.loc_ );
				}
		}
}

void
Kernel::TeXLabelManager::parseTeXErrors( std::istream & interaction )
{
	int lastLabel = -2;
	const char KEY[] = "[SHAPES LABEL: ";
	size_t KEY_LEN = strlen( KEY );
	std::string line;
	for( std::getline( interaction, line ); ! interaction.eof( ); std::getline( interaction, line ) )
		{
			if( line.compare( 0, 1, "!" ) == 0 )
				{
					line = line.substr( 2 );
					goto found;
				}
			if( line.compare( 0, KEY_LEN, KEY ) == 0 )
				{
					char * endp;
					lastLabel = strtol( line.c_str( ) + KEY_LEN, & endp, 10 );
					if( *endp != ']' )
						{
							throw Exceptions::InternalError( strrefdup( "Failed to scan the label number in: " + line ) );
						}
				}
		}
	throw Exceptions::InternalError( "Failed to find an error message in the output from the failing call to pdfLaTeX." );

 found:
	std::ostringstream message;
	{
		std::string tmp;
		size_t lineSize = 0;
		for( std::getline( interaction, tmp ); ! interaction.eof( ); std::getline( interaction, tmp ) )
			{
				if( lineSize > 0 )
					{
						message << tmp.substr( lineSize ) << std::endl ;
						break;
					}
				if( tmp.compare( 0, 1, "!" ) == 0 )
					{
						break;
					}
				if( tmp.compare( 0, KEY_LEN, KEY ) == 0 )
					{
						break;
					}
				if( tmp.compare( 0, 2, "l." ) == 0 )
					{
					char * endp;
					//					int dummyLineNo =
					strtol( tmp.c_str( ) + 2, & endp, 10 );
					if( *endp != ' ' )
						{
							throw Exceptions::InternalError( "Expected a space after the line number specification in the output frmo pdfLaTeX." );
						}
					++endp;
					lineSize = endp - tmp.c_str( );
					message << endp << std::endl ;
					continue;
					}
				message << tmp << std::endl ;
			}
	}

	if( lastLabel == -2 )
		{
			throw Exceptions::StaticTeXLabelError( true, "preamble", strrefdup( line ), strrefdup( message ), Ast::THE_UNKNOWN_LOCATION );
		}
	if( lastLabel == -1 )
		{
			throw Exceptions::InternalError( strrefdup( "The TeX setup information caused an error: " + line ) );
		}
	{
		int labelIndex = 0;
		typedef typeof currentRequests RequestMapType;
		for( RequestMapType::const_iterator i = currentRequests.begin( );
				 i != currentRequests.end( );
				 ++i, ++labelIndex )
			{
				if( labelIndex == lastLabel )
					{
						if( i->second.literal_ )
							{
								throw Exceptions::StaticTeXLabelError( true, "label", strrefdup( line ), strrefdup( message ), i->second.loc_ );
							}
						else
							{
								throw Exceptions::TeXLabelError( true, "label", strrefdup( line ), strrefdup( message ), i->second.loc_ );
							}
					}
			}
		{
			/* Reaching here is an error.
			 */
			std::ostringstream msg;
			msg << "The label index " << lastLabel << " was out of the range (" << currentRequests.size( ) << ")" ;
			throw Exceptions::InternalError( strrefdup( msg ) );
		}
	}
}
