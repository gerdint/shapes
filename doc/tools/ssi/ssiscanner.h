#ifndef ssiscanner_h
#define ssiscanner_h

#include <sys/types.h>
#include <sys/stat.h>
#include <stack>
#include <list>

#ifndef FLEXINT_H								// Else *FlexLexer will be defined twice
#	undef yyFlexLexer
#	define yyFlexLexer ssiFlexLexer
#	include <FlexLexer.h>
#endif

class SSIScanner : public ssiFlexLexer
{
	struct FileID
	{
		dev_t st_dev;
		ino_t st_ino;
		FileID( const struct stat & stat ) : st_dev( stat.st_dev ), st_ino( stat.st_ino ) { }
		bool operator < ( const FileID & other ) const
		{
			if( st_dev < other.st_dev )
				{
					return true;
				}
			if( st_dev > other.st_dev )
				{
					return false;
				}
			return st_ino < other.st_ino;
		}
	};
	bool onlyDependencies_;
	std::stack< yy_buffer_state * > stateStack_;
	char * includeFilename_;
	void more( );
public:
	SSIScanner( bool onlyDependencies, std::istream * yyin = 0, std::ostream * yyout = 0 );
	virtual ~SSIScanner( );
	virtual int yylex( );
 private:
	void doInclusion( );
};



#endif
