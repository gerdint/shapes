#include "ssiscanner.h"

#include <fstream>
#include <iostream>
#include <cstdlib>


int
main( int argc, char ** argv )
{
	bool onlyDependencies = false;
	bool doEndl = false;
	char* includebase = "";

	--argc;
	++argv;
	while( argc > 0 )
		{
			if( strncmp( *argv, "-d", 2 ) == 0 )
				{
					char * name = *argv + 2;
					char * asgn = strchr( name, '=' );
					if( asgn == 0 )
						{
							std::cerr << "Missing '=' in command line option '-d'." << std::endl ;
							exit( 1 );
						}
					*asgn = '\0';
					setenv( name, asgn + 1, 1 );
					++argv;
					--argc;
				}
			else if( strcmp( *argv, "--deps" ) == 0 )
				{
					onlyDependencies = true;
					++argv;
					--argc;
				}
			else if( strcmp( *argv, "--head" ) == 0 )
				{
					std::cout << *( argv + 1 ) << ":" ;
					doEndl = true;
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--includebase" ) == 0 )
				{
					size_t len = strlen( argv[ 1 ] );
					includebase = new char[ len + 2 ];
					strcpy( includebase, argv [ 1 ] );
					if ( includebase[ len - 1 ] != '/' )
						{
							includebase[ len ] = '/';
							includebase[ len + 1 ] = '\0';
						}
					argv += 2;
					argc -= 2;
				}
			else
				{
					std::cerr << "Illegal command line option: " << *argv << std::endl ;
					exit( 1 );
				}
		}

	SSIScanner scanner( onlyDependencies, & std::cin, & std::cout, includebase );
	scanner.yylex( );

	if( doEndl )
		{
			std::cout << std::endl ;
		}

	return 0;
}
