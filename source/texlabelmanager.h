#ifndef labels_h
#define labels_h

#include "SimplePDF_decls.h"
#include "Shapes_Lang_decls.h"
#include "Shapes_Kernel_decls.h"

#include "shapestypes.h"

#include <set>
#include <string>
#include <map>
#include <iostream>
#include <sstream>

namespace Shapes
{
	namespace Kernel
	{

	class TeXLabelManager
	{
	public:
		class RequestLocation
		{
		public:
			bool literal_;
			Ast::SourceLocation loc_;
			RequestLocation( bool literal, const Ast::SourceLocation & loc )
				: literal_( literal ), loc_( loc )
			{ }
		};
	private:
		std::set< std::string > allRequests;
		std::map< std::string, RequestLocation > currentRequests;

	public:
		typedef std::map< std::string, RefCountPtr< const Lang::XObject > > MapType;
	private:
		MapType availableLabels;
		bool anyLabelMiss;
		int loadCount;

		std::string texJobName;

		int jobNumber;
		std::string stringWithJobNumber( const std::string & str ) const;

		std::string documentclass;
		std::list< std::string > documentoptions;
		bool lmodernT1;
		bool utf8;
		std::ostringstream preamble;
		std::ostringstream documentTop;

		bool setupFinalized;
		std::string setupCode;
		std::string setupCodeHash;

	public:
		TeXLabelManager( );
		~TeXLabelManager( );

		void settexJobName( const std::string & _texJobName );

		void announce( const std::string & str, const Ast::SourceLocation & loc );
		RefCountPtr< const Lang::Value > request( const std::string & str, const Ast::SourceLocation & loc, Kernel::PassedDyn dyn );

		void iterativeStartup( RefCountPtr< std::istream > labelsFile );
		void iterativeFinish( const std::string & labelDBFilename );

		void setDocumentClass( const Ast::SourceLocation & loc, const char * str );
		void addDocumentOption( const Ast::SourceLocation & loc, const char * str );
		void setlmodernT1( const Ast::SourceLocation & loc, bool val );
		void setutf8( const Ast::SourceLocation & loc, bool val );
		void addPreambleLine( const Ast::SourceLocation & loc, const char * line );
		void addDocumentTopLine( const Ast::SourceLocation & loc, const char * line );

	private:
		void assertNotSetupFinalized( const Ast::SourceLocation & loc ) const;
		void processRequests( );
		void loadLabels( RefCountPtr< std::istream > labelsFile );
		static std::string safeSourceHash( const std::string & str );
		void compileSetupCode( );
		static bool isAllBlank( const char * str );
		void assertBalanced( const std::string & str, const RequestLocation & loc ) const;
		void parseTeXErrors( std::istream & interaction );
	};

	}
}

#endif
