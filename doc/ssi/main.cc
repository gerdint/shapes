#include "ssiscanner.h"

#include <fstream>
#include <iostream>

int
main( int argc, char ** argv )
{
	bool onlyDependencies = false;


	--argc;
	++argv;
	while( argc > 1 )
		{
			if( strcmp( *argv, "--deps" ) == 0 )
				{
					onlyDependencies = true;
					++argv;
					--argc;
				}
			else
				{
					std::cerr << "Illegal command line option: " << *argv << std::endl ;
					exit( 1 );
				}
		}
	if( argc < 1 )
		{
			std::cerr << "Missing input filename as final command line argument." << std::endl ;
			exit( 1 );
		}
	const char * filename = *argv;

	std::ifstream iFile( filename );
	if( ! iFile.good( ) )
		{
			std::cerr << "Failed to open main file: " << filename << std::endl ;
			exit( 1 );
		}

	SSIScanner scanner( filename, onlyDependencies, & iFile, & std::cout );
	scanner.yylex( );

	return 0;
}
