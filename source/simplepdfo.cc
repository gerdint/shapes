#include <iomanip>

#include "simplepdfo.h"
#include "simplepdfi.h"
#include "pdfversion.h"
#include "shapesexceptions.h"
#include "texlabelmanager.h"
#include "shapestypes.h"
#include "globals.h"

using namespace std;

using namespace SimplePDF;

PDF_Resources::PDF_Resources( )
	: counter( 0 ),
		xobject( new PDF_Dictionary ),
		graphicsStates( new PDF_Dictionary ),
		colorSpaces( new PDF_Dictionary ),
		fonts( new PDF_Dictionary ),
		shadings( new PDF_Dictionary ),
		procSetsVector( new PDF_Vector )
{ }

PDF_Resources::~PDF_Resources( )
{ }

void
PDF_Resources::writeTo( std::ostream & os, SimplePDF::PDF_xref * xref, const RefCountPtr< const PDF_Object > & self ) const
{
	RefCountPtr< PDF_Dictionary > dic( new PDF_Dictionary );
	if( ! xobject->dic.empty( ) )
		{
			(*dic)[ "XObject" ] = SimplePDF::indirect( xobject, & Shapes::Kernel::theIndirectObjectCount );
		}
	if( ! graphicsStates->dic.empty( ) )
		{
			(*dic)[ "ExtGState" ] = SimplePDF::indirect( graphicsStates, & Shapes::Kernel::theIndirectObjectCount );
		}
	if( ! colorSpaces->dic.empty( ) )
		{
			(*dic)[ "ColorSpaces" ] = SimplePDF::indirect( colorSpaces, & Shapes::Kernel::theIndirectObjectCount );
		}
	if( !fonts->dic.empty( ) )
		{
			(*dic)[ "Font" ] = SimplePDF::indirect( fonts, & Shapes::Kernel::theIndirectObjectCount );
		}
	if( ! procSetsVector->vec.empty( ) )
		{
			(*dic)[ "ProcSet" ] = procSetsVector;
		}
	if( ! shadings->dic.empty( ) )
		{
			(*dic)[ "Shading" ] = SimplePDF::indirect( shadings, & Shapes::Kernel::theIndirectObjectCount );
		}
	dic->writeTo( os, xref, dic );
}

const SimplePDF::PDF_Name &
PDF_Resources::nameof( const RefCountPtr< PDF_Object > & obj, ReverseMap * reverseMap, RefCountPtr< PDF_Dictionary > * dic, const char * prefix, size_t * counter )
{
	ReverseMap::iterator i = reverseMap->find( obj.getPtr( ) );
	if( i == reverseMap->end( ) )
		{
			ostringstream reference;
			reference << prefix << *counter ;
			++*counter;

			(*dic)->dic[ reference.str( ) ] = obj;
			reverseMap->insert( ReverseMap::value_type( obj.getPtr( ), PDF_Name( reference.str( ) ) ) );
			i = reverseMap->find( obj.getPtr( ) );
		}
	return i->second;
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofXObject( const RefCountPtr< PDF_Object > & obj )
{
	return nameof( obj, & reverse_xobject, & xobject, "x", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofGraphicsState( const RefCountPtr< PDF_Object > & obj )
{
	return nameof( obj, & reverse_graphicsStates, & graphicsStates, "g", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofColorSpace( const RefCountPtr< PDF_Object > & obj )
{
	return nameof( obj, & reverse_colorSpaces, & colorSpaces, "c", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofFont( const RefCountPtr< PDF_Object > & obj )
{
	return nameof( obj, & reverse_fonts, & fonts, "f", & counter );
}

const PDF_Name &
SimplePDF::PDF_Resources::nameofShading( const RefCountPtr< PDF_Object > & obj )
{
	return nameof( obj, & reverse_shadings, & shadings, "s", & counter );
}


void
SimplePDF::PDF_Resources::requireProcedureSet( ProcSet procSet )
{
	if( procSets.find( procSet ) == procSets.end( ) )
		{
			switch( procSet )
				{
				case PROC_SET_PDF:
					procSetsVector->vec.push_back( SimplePDF::newName( "PDF" ) );
					break;
				case PROC_SET_TEXT:
					procSetsVector->vec.push_back( SimplePDF::newName( "Text" ) );
					break;
				case PROC_SET_IMAGE_GRAY:
					procSetsVector->vec.push_back( SimplePDF::newName( "ImageB" ) );
					break;
				case PROC_SET_IMAGE_COLOR:
					procSetsVector->vec.push_back( SimplePDF::newName( "ImageC" ) );
					break;
				case PROC_SET_IMAGE_INDEXED:
					procSetsVector->vec.push_back( SimplePDF::newName( "ImageI" ) );
					break;
				default:
					throw "SimplePDF::PDF_Resources: Internal error: An unexpected procedure set was requested.";
				}
			procSets.insert( procSets.begin( ), procSet );
		}
}

SimplePDF::DocumentInfo::DocumentInfo( )
	: finalized_( false ),
		info_( new PDF_Dictionary ),
		i_info_( NullPtr< PDF_Indirect_out >( ) )
{
	(*info_)[ "Producer" ] = newString( "Shapes" );
}

void
SimplePDF::DocumentInfo::addExtensionAuthorString( const std::string & str )
{
	if( finalized_ )
		{
			throw "Internal error: DocumentInfo::addExtensionAuthorString: Already finalized.";
		}
	extensionAuthorStrings.push_back( str );
}

bool
SimplePDF::DocumentInfo::addInfo( const char * key, const RefCountPtr< PDF_Object > & datum )
{
	if( finalized_ )
		{
			throw "Internal error: DocumentInfo::addInfo: Already finalized.";
		}

	if( info_->hasKey( key ) )
		{
			return false;
		}
	(*info_)[ key ] = datum;
	return true;

}

RefCountPtr< SimplePDF::PDF_Indirect_out >
SimplePDF::DocumentInfo::getIndirect( size_t * indirectObjectCounter )
{
	if( ! finalized_ )
		{
			if( ! extensionAuthorStrings.empty( ) )
				{
					std::ostringstream oss;
					typedef typeof extensionAuthorStrings ListType;
					ListType::const_iterator i = extensionAuthorStrings.begin( );
					oss << *i ;
					++i;
					for( ; i != extensionAuthorStrings.end( ); ++i )
						{
							oss << "; " << *i ;
						}
					(*info_)[ "ExtensionAuthors" ] = newString( oss.str( ).c_str( ) );
				}
			finalized_ = true;
			i_info_ = SimplePDF::indirect( info_, indirectObjectCounter );
		}
	return i_info_;
}



SimplePDF::PDF_out::PDF_out( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_root, const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_info, const RefCountPtr< const char > & firstPageLabel )
	: i_root_( i_root ),
		i_info_( i_info ),
		firstPageLabel_( firstPageLabel )
{ }

SimplePDF::PDF_out::~PDF_out( )
{ }

RefCountPtr< const char >
SimplePDF::PDF_out::getFirstPageLabel( ) const
{
	return firstPageLabel_;
}

void
SimplePDF::PDF_out::writeFile( std::ostream & os, SimplePDF::PDF_Version & pdfVersion )
{
	std::streamoff os_start = static_cast< streamoff >( os.tellp( ) );
	SimplePDF::PDF_xref my_xref;

	try
		{
			os << std::fixed ;
			os << "%" << pdfVersion.maxRequestVersionString( ) << endl
				 << "%"
				 << static_cast< char >( 129 ) << static_cast< char >( 130 )
				 << static_cast< char >( 131 ) << static_cast< char >( 132 )
				 << " Treat as binary" << endl ;

			my_xref.enqueue( i_root_ );
			my_xref.enqueue( i_info_ );
			my_xref.writeRecursive( os );

			streamoff xref( static_cast< streamoff >( os.tellp( ) ) );
			my_xref.writeTable( os );
			os << "trailer" << endl ;
			{
				RefCountPtr< PDF_Dictionary > trailer( new PDF_Dictionary );
				trailer->dic[ "Size" ] = newInt( my_xref.size( ) );
				trailer->dic[ "Root" ] = i_root_;
				trailer->dic[ "Info" ] = i_info_;
				trailer->writeTo( os, & my_xref, trailer ); /* This will not affect the xref, since the indirect objects were put in queue before calling writeTable. */
			}
			os << endl << "startxref" << endl ;
			os << xref - os_start << endl ;
			os << "%%EOF" << endl ;
		}
	catch( const char * ball )
		{
			std::ostringstream msg;
			msg << "SimplePDF::PDF_out::writeFile: Caught (char*) ball at top level:" << endl
					<< "	" << ball << endl ;
			throw msg.str( );
		}
	catch( const string & ball )
		{
			std::ostringstream msg;
			msg << "SimplePDF::PDF_out::writeFile: Caught (string) ball at top level:" << endl
					<< "	" << ball << endl ;
			throw msg.str( );
		}
	catch( ... )
		{
			throw "SimplePDF::PDF_out::writeFile: Caught (...) ball at top level.";
		}
}


RefCountPtr<PDF_Object>
SimplePDF::newName( const char * str )
{
	return RefCountPtr<PDF_Name>( new PDF_Name( str ) );
}

RefCountPtr<PDF_Object>
SimplePDF::newString( const char * str )
{
	return RefCountPtr< PDF_String >( new PDF_LiteralString( str ) );
}

RefCountPtr<PDF_Object>
SimplePDF::newInt( PDF_Int::ValueType val )
{
	return RefCountPtr<PDF_Int>( new PDF_Int( val ) );
}

RefCountPtr<PDF_Object>
SimplePDF::newBoolean( PDF_Boolean::ValueType val )
{
	if( val )
		{
			return theTrue;
		}
	return theFalse;
}

RefCountPtr<PDF_Object>
SimplePDF::newFloat( PDF_Float::ValueType val )
{
	return RefCountPtr<PDF_Float>( new PDF_Float( val ) );
}


RefCountPtr<PDF_Object> SimplePDF::theTrue( new PDF_Boolean( true ) );
RefCountPtr<PDF_Object> SimplePDF::theFalse( new PDF_Boolean( false ) );


SimplePDF::OutlineItem::OutlineItem( const RefCountPtr< PDF_Object > & destination, const RefCountPtr< const char > & title, bool isOpen, bool fontBold, bool fontItalic, const Shapes::Concrete::RGB & color )
	: destination_( destination ), title_( title ), isOpen_( isOpen ), fontBold_( fontBold ), fontItalic_( fontItalic ), color_( color )
{ }

SimplePDF::OutlineItem::~OutlineItem( )
{ }

void
SimplePDF::OutlineItem::addKid( const RefCountPtr< OutlineItem > & kid )
{
	kids_.push_back( kid );
}

bool
SimplePDF::OutlineItem::hasKids( ) const
{
	return ! kids_.empty( );
}

RefCountPtr< SimplePDF::PDF_Indirect_out >
SimplePDF::OutlineItem::getTopIndirectDictionary( SimplePDF::PDF_Version & pdfVersion ) const
{
	RefCountPtr< SimplePDF::PDF_Dictionary > res( new SimplePDF::PDF_Dictionary );
	RefCountPtr< SimplePDF::PDF_Indirect_out > i_res = SimplePDF::indirect( res, & Shapes::Kernel::theIndirectObjectCount );
	res->dic[ "Type"	] = SimplePDF::newName( "Outlines" );

	if( ! kids_.empty( ) )
		{
			size_t openCount = 0;

			typedef typeof kids_ ListType;
			ListType::const_iterator i = kids_.begin( );

			RefCountPtr< SimplePDF::PDF_Dictionary > newKid( new SimplePDF::PDF_Dictionary );
			newKid->dic[ "Parent" ] = i_res;
			RefCountPtr< SimplePDF::PDF_Indirect_out > i_newKid = SimplePDF::indirect( newKid, & Shapes::Kernel::theIndirectObjectCount );
			openCount += (*i)->fillInDictionary( newKid, i_newKid, pdfVersion );

			RefCountPtr< SimplePDF::PDF_Indirect_out > i_first = i_newKid;

			++i;
			for( ; i != kids_.end( ); ++i )
				{
					RefCountPtr< SimplePDF::PDF_Dictionary > lastKid = newKid;
					newKid = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );
					newKid->dic[ "Prev" ] = i_newKid;
					i_newKid = SimplePDF::indirect( newKid, & Shapes::Kernel::theIndirectObjectCount );
					lastKid->dic[ "Next" ] = i_newKid;
					newKid->dic[ "Parent" ] = i_res;
					openCount += (*i)->fillInDictionary( newKid, i_newKid, pdfVersion );
				}

			res->dic[ "First"	] = i_first;
			res->dic[ "Last"	] = i_newKid;

			if( openCount > 0 )
				{
					res->dic[ "Count"	] = SimplePDF::newInt( openCount );
				}
		}

	return i_res;
}

size_t
SimplePDF::OutlineItem::fillInDictionary( RefCountPtr< SimplePDF::PDF_Dictionary > dstDic, const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_dstDic, SimplePDF::PDF_Version & pdfVersion ) const
{
	dstDic->dic[ "Title"	] = SimplePDF::newString( title_.getPtr( ) );
	dstDic->dic[ "Dest"	] = destination_;
	const SimplePDF::PDF_Version::Version FANCY_OUTLINE_VERSION = SimplePDF::PDF_Version::PDF_1_3;
	if( fontBold_ || fontItalic_ )
		{
			if( pdfVersion.greaterOrEqual( FANCY_OUTLINE_VERSION ) )
				{
					dstDic->dic[ "F"	] = SimplePDF::newInt( ( fontBold_ ? 2 : 0 ) + ( fontItalic_ ? 1 : 0 ) );
				}
			else
				{
					pdfVersion.message( FANCY_OUTLINE_VERSION, "The outline item font flags were ignored." );
				}
		}
	if( color_.mean( ) > 0 )
		{
			if( pdfVersion.greaterOrEqual( FANCY_OUTLINE_VERSION ) )
				{
					dstDic->dic[ "C"	] = color_.componentVector( );
				}
			else
				{
					pdfVersion.message( FANCY_OUTLINE_VERSION, "The outline item color was ignored." );
				}
		}


	size_t openCount = 0;

	if( ! kids_.empty( ) )
		{
			typedef typeof kids_ ListType;
			ListType::const_iterator i = kids_.begin( );

			RefCountPtr< SimplePDF::PDF_Dictionary > newKid( new SimplePDF::PDF_Dictionary );
			newKid->dic[ "Parent" ] = i_dstDic;
			RefCountPtr< SimplePDF::PDF_Indirect_out > i_newKid = SimplePDF::indirect( newKid, & Shapes::Kernel::theIndirectObjectCount );
			openCount += (*i)->fillInDictionary( newKid, i_newKid, pdfVersion );

			RefCountPtr< SimplePDF::PDF_Indirect_out > i_first = i_newKid;

			++i;
			for( ; i != kids_.end( ); ++i )
				{
					RefCountPtr< SimplePDF::PDF_Dictionary > lastKid = newKid;
					newKid = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );
					newKid->dic[ "Prev" ] = i_newKid;
					i_newKid = SimplePDF::indirect( newKid, & Shapes::Kernel::theIndirectObjectCount );
					lastKid->dic[ "Next" ] = i_newKid;
					newKid->dic[ "Parent" ] = i_dstDic;
					openCount += (*i)->fillInDictionary( newKid, i_newKid, pdfVersion );
				}

			dstDic->dic[ "First"	] = i_first;
			dstDic->dic[ "Last"	] = i_newKid;

		}

	if( openCount > 0 )
		{
			if( isOpen_ )
				{
					dstDic->dic[ "Count"	] = SimplePDF::newInt( openCount );
				}
			else
				{
					dstDic->dic[ "Count"	] = SimplePDF::newInt( -openCount );
				}
		}

	if( isOpen_ )
		{
			return openCount + 1;
		}
	return 1;
}
