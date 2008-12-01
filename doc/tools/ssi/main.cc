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

#include "ssiscanner.h"

#include <fstream>
#include <iostream>
#include <cstdlib>
#include <string>
#include <vector>


bool strprefixcmp( char * str, const char * prefix, char ** endp );
bool strtobool( const char * str, const char * containingString, const char * trueLabel = 0, const char * falseLabel = 0 );


int
main( int argc, char ** argv )
{
	bool onlyDependencies = false;
	bool absolutePath = false;
	bool doEndl = false;
	char * inputFilename = 0;
	std::vector< std::string > includePath;
	includePath.push_back( "./" );

	--argc;
	++argv;
	while( argc > 0 )
		{
			char * optionSuffix;
			if( strprefixcmp( *argv, "-d", & optionSuffix ) )
				{
					char * name = optionSuffix;
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
					std::cout << argv[ 1 ] << ":" ;
					doEndl = true;
					argv += 2;
					argc -= 2;
				}
			else if( strcmp( *argv, "--in" ) == 0 )
				{
					inputFilename = argv[ 1 ];
					argv += 2;
					argc -= 2;
				}
			else if( strprefixcmp( *argv, "-I", & optionSuffix ) )
				{
					std::string tmp( optionSuffix );
					if( tmp[ tmp.size() - 1 ] != '/' )
						tmp += "/";
					includePath.push_back( tmp );
					argv += 1;
					argc -= 1;
				}
			else if( strprefixcmp( *argv, "--absolute=", & optionSuffix ) )
				{
					absolutePath = strtobool( optionSuffix, *argv );
					argv += 1;
					argc -= 1;
				}
			else
				{
					std::cerr << "Illegal command line option: " << *argv << std::endl ;
					exit( 1 );
				}
		}

	if( inputFilename == 0 )
		{
			std::cerr << "The input file was not specified.  Please use \"--in <filename>\"." << std::endl ;
			exit( 1 );
		}
	std::ifstream iFile( inputFilename );
	if( ! iFile.good( ) )
		{
			std::cerr << "Failed to open main file: " << inputFilename << std::endl ;
			exit( 1 );
		}

	std::string initDir;
	{
		/* Make a copy of the input filename, and remove everything after the last slash, or
		 * the whole string, if there is no slash, so that the result can be appended to the
		 * current working directory to construct the full name of the directory containing the input.
		 */
		char * tmp = strdup( inputFilename );
		char * slash = strrchr( tmp, '/' );
		if( slash == 0 )
			{
				slash = tmp;
			}
		else
			{
				++slash;
			}
		*slash = '\0';
		if( tmp[ 0 ] == '/' || ! absolutePath )
			{
				initDir = tmp;
			}
		else
			{
				char * cwd = getcwd( 0, 0 );
				initDir = cwd + std::string( "/" ) + tmp;
				free( cwd );
			}
		free( tmp );
	}

	SSIScanner scanner( onlyDependencies, initDir, includePath, & iFile, & std::cout );
	scanner.yylex( );

	if( doEndl )
		{
			std::cout << std::endl;
		}

	return 0;
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
	exit( 1 );
}
