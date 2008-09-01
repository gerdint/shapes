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
	std::stack< std::string > dirStack_;
	std::string includeFilename_;
	std::stack< size_t > depthLimitStack_;
	size_t currentDepthLimit_;
	std::stack< bool > metaInclusionStack_;
	bool currentMeta_;
	void more( );
public:
	SSIScanner( bool onlyDependencies, const std::string & initDir, std::istream * yyin = 0, std::ostream * yyout = 0 );
	virtual ~SSIScanner( );
	virtual int yylex( );
 private:
	char * expandDefines( const char * str ); // Returns newly allocated memory.
	void doInclusion( );
};



#endif
