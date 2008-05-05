#include "pdfversion.h"
#include "shapesexceptions.h"

#include <algorithm>

using namespace SimplePDF;

SimplePDF::PDF_Version::PDF_Version( )
	: version_( SimplePDF::PDF_Version::PDF_1_4 ),
		maxRequestVersion_( SimplePDF::PDF_Version::PDF_1_1 ),
		versionAction_( SimplePDF::PDF_Version::WARN )
{ }

void
SimplePDF::PDF_Version::setVersion( Version version )
{
	version_ = version;
}

void
SimplePDF::PDF_Version::setAction( Action action )
{
	versionAction_ = action;
}

bool
SimplePDF::PDF_Version::greaterOrEqual( Version required, bool justCurious )
{
	if( ! justCurious )
		{
			maxRequestVersion_ = std::max( maxRequestVersion_, required );
		}
	return version_ >= required;
}

bool
SimplePDF::PDF_Version::greaterOrEqualOrX( Version required, bool justCurious )
{
	if( ! justCurious )
		{
			maxRequestVersion_ = std::max( maxRequestVersion_, required );
		}
	return version_ >= required || version_ == PDF_X;
}

const char *
SimplePDF::PDF_Version::maxRequestVersionString( ) const
{
	if( version_ == PDF_X )
		{
			return toString( PDF_X );
		}
	return toString( maxRequestVersion_ );
}

void
SimplePDF::PDF_Version::message( Version required, const char * message ) const
{
	using namespace Shapes;

	switch( versionAction_ )
		{
		case ERROR:
			throw Exceptions::PDFVersionError( version_, required, message );
			break;
		case WARN:
			std::cerr << toString( version_ ) << " warning: " << message << std::endl ;
			break;
		case SILENT:
			// Just be quiet.
			break;
		default:
			throw Exceptions::InternalError( "PDF_out versionAction_ out of range." );
		}
}

const char *
SimplePDF::PDF_Version::toString( SimplePDF::PDF_Version::Version version )
{
	switch( version )
		{
		case PDF_X:
			return "PDF-X" ;
		case PDF_1_1:
			return "PDF-1.1" ;
		case PDF_1_2:
			return "PDF-1.2" ;
		case PDF_1_3:
			return "PDF-1.3" ;
		case PDF_1_4:
			return "PDF-1.4" ;
		case PDF_1_5:
			return "PDF-1.5" ;
		case PDF_1_6:
			return "PDF-1.6" ;
		default:
			throw Shapes::Exceptions::InternalError( "PDF version out of range." );
		}
}
