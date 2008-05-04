#include <stdio.h>

// This file uses the c functions to make it compile as fast as
// possible, so that this solution will never be considered a waste of
// time.

void
printVersion( )
{
	printf( "Version:    " VERSION_NUMBER "\n" );
	printf( "Build date: " VERSION_DATE "\n" );
}
