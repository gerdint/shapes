#include "pdfimport.h"
#include "globals.h"
#include "shapesexceptions.h"
#include "pathtypes.h"
#include "drawabletypes.h"
#include "texlabelmanager.h"

using namespace Shapes;
using namespace SimplePDF;

Kernel::Import::Import( )
{ }

RefCountPtr< const std::vector< RefCountPtr< const Shapes::Lang::XObject > > >
Kernel::Import::addPagesAsXObjects( RefCountPtr< PDF_in > pdfi )
{
	using namespace Shapes;

	importSources_.push_back( pdfi ); // Keep the source alive so that it can be used when finally producing output
	IndirectRemapType indirectRemap;
	std::vector< RefCountPtr< const Lang::XObject > > * res = new std::vector< RefCountPtr< const Lang::XObject > >;
	for( size_t pageNumber( 0 ); pageNumber < pdfi->getPageCount( ); ++pageNumber )
		{
			RefCountPtr< PDF_Dictionary > pageDic( pdfi->getPage( pageNumber ) );
			RefCountPtr< PDF_Vector > cropBox = down_cast_follow< PDF_Vector >( pageDic->getInheritable( "CropBox" ) );
			if( cropBox == NullPtr< PDF_Vector >( ) )
				{
					cropBox = down_cast_follow< PDF_Vector >( pageDic->getInheritable( "MediaBox" ) );
				}
			if( cropBox == NullPtr< PDF_Vector >( ) )
				{
					throw Exceptions::InternalError( strrefdup( "Failed to find crop box of imported page.	(Searched to the page tree root.)" ) );
				}
			RefCountPtr< PDF_Stream_in > original( pdfi->follow< PDF_Stream_in >( (*pageDic)[ "Contents" ] ) );

//			 RefCountPtr< PDF_Dictionary > xobjectDic( pdfi->follow< PDF_Dictionary >( (*pdfi->follow< PDF_Dictionary >( (*pageDic)[ "Resources" ] ))[ "XObject" ] ) );
//			 if( xobjectDic->dic.size( ) != 1 )
//				 {
//					 throw Exceptions::InternalError( strrefdup( "Expected exactly 1 XObject on a TeX label input page" ) );
//				 }
//			 RefCountPtr< PDF_Stream_in > original( pdfi->follow< PDF_Stream_in >( xobjectDic->dic.begin( )->second ) );
			RefCountPtr< PDF_Stream_in > newObj( new PDF_Stream_in( original->is, original->dataStart ) );
			(*newObj)[ "Subtype" ] = newName( "Form" );
			(*newObj)[ "FormType" ] = newInt( 1 );
			(*newObj)[ "BBox" ] = cropBox;							 // ->rectangleIntersection( down_cast_follow< PDF_Vector >( (*original)[ "BBox" ] ) );
			(*newObj)[ "Matrix" ] = RefCountPtr< PDF_Object >( new PDF_Vector( 1, 0, 0, 1, 0, 0 ) );
			(*newObj)[ "Filter" ] = (*original)[ "Filter" ];
			(*newObj)[ "Length" ] = RefCountPtr< PDF_Object >( new PDF_Int( pdfi->follow< PDF_Int >( (*original)[ "Length" ] )->value( ) ) );
			(*newObj)[ "Resources" ] = deepCopy( (*pageDic)[ "Resources" ], & Kernel::theIndirectObjectCount, & indirectRemap );

			Concrete::Length xmin;
			Concrete::Length xmax;
			Concrete::Length ymin;
			Concrete::Length ymax;
			try
				{
					RefCountPtr< PDF_Vector > bboxTyped = pdfi->follow< PDF_Vector >( (*newObj)[ "BBox" ] );
					if( bboxTyped->vec.size( ) != 4 )
						{
							throw Exceptions::InternalError( strrefdup( "The bbox of the imported page was not of size 4." ) );
						}
					xmin = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 0 ] )->value( ) );
					xmax = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 2 ] )->value( ) );
					ymin = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 1 ] )->value( ) );
					ymax = Concrete::Length( pdfi->follow< PDF_Float >( bboxTyped->vec[ 3 ] )->value( ) );
				}
			catch( Exceptions::Exception & ball )
				{
					throw;
				}
			catch( const char * ball )
				{
					std::ostringstream oss;
					oss << "An error occurred while evaluating the bbox of the imported page: " << ball ;
					throw Exceptions::InternalError( strrefdup( oss.str( ).c_str( ) ) );
				}
			catch( ... )
				{
					throw Exceptions::InternalError( strrefdup( "An error occurred while evaluating the bbox of the imported page." ) );
				}
			Lang::ElementaryPath2D * bboxpath = new Lang::ElementaryPath2D;
			bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymin ) );
			bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymax ) );
			bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymax ) );
			bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymin ) );
			bboxpath->close( );

			RefCountPtr< PDF_Object > indirection = SimplePDF::indirect( newObj, & Kernel::theIndirectObjectCount );
			res->push_back( RefCountPtr< Lang::XObject >( new Lang::XObject( indirection, RefCountPtr< const Lang::ElementaryPath2D >( bboxpath ) ) ) );
		}
	return RefCountPtr< const std::vector< RefCountPtr< const Lang::XObject > > >( res );
}

void
Kernel::Import::importBtexEtexThings( RefCountPtr< PDF_in > pdfi, Shapes::Kernel::TeXLabelManager::MapType * dstMap, const std::string & setupCodeHash )
{
	using namespace Shapes;

	importSources_.push_back( pdfi ); // Keep the source alive so that it can be used when finally producing output
	IndirectRemapType indirectRemap;

	PDF_in::PageIterator theEnd = pdfi->endPages( );
	for( PDF_in::PageIterator i = pdfi->beginPages( ); i != theEnd; ++i )
		{
			RefCountPtr< PDF_Dictionary > xobjectDic( pdfi->follow< PDF_Dictionary >( (*pdfi->follow< PDF_Dictionary >( (**i)[ "Resources" ] ))[ "XObject" ] ) );
			if( xobjectDic->dic.size( ) != 1 )
				{
					throw Exceptions::InternalError( strrefdup( "Expected exactly 1 XObject on a TeX label input page" ) );
				}
			RefCountPtr< PDF_Stream_in > original( pdfi->follow< PDF_Stream_in >( xobjectDic->dic.begin( )->second ) );

			std::string texStr;
			{
				RefCountPtr< PDF_Stream_in > texStream( pdfi->follow< PDF_Stream_in >( (*original)[ "TeXsrc" ] ) );
				std::ostringstream tmp;
				texStream->writeDataDefilteredTo( tmp );
				texStr = tmp.str( );
			}
			if( i == pdfi->beginPages( ) )
				{
					/* This page only contains information about the TeX context */
					if( texStr != setupCodeHash )
						{
							throw Exceptions::TeXSetupHasChanged( );
						}
					continue;
				}
			Concrete::Length height( pdfNameToDouble( (*original)[ "TeXht" ] ) );
			Concrete::Length depth( pdfNameToDouble( (*original)[ "TeXdp" ] ) );
			Concrete::Length width( pdfNameToDouble( (*original)[ "TeXwd" ] ) );

			const Concrete::Length bboxAddY = 0.08 * ( depth + height );
			const Concrete::Length xmin = Concrete::ZERO_LENGTH;
			const Concrete::Length xmax = width;
			const Concrete::Length ymin = -depth - bboxAddY;
			const Concrete::Length ymax = height + bboxAddY;

			/* The bbox we get from pdfLaTeX is too small!
				 It has to be grown a little.
				 (*newObj)[ "BBox" ] = (*original)[ "BBox" ];
			*/
			/*
			cerr << "minmax: " << xmin << " " << ymin << " " << xmax << " " << ymax << endl ;

			cerr << "texbbox: " ;
			RefCountPtr< PDF_Vector > texbboxref = pdfi->follow< PDF_Vector >( (*original)[ "BBox" ] );
			double texbbox[ 4 ];
			for( size_t i = 0; i < 4; ++i )
				{
					texbbox[ i ] = pdfi->follow< PDF_Float >( texbboxref->vec[ i ] )->value( );
					cerr << texbbox[i] << " " ;
				}
			cerr << endl ;
			*/
			RefCountPtr< PDF_Stream_in > newObj( new PDF_Stream_in( original->is, original->dataStart ) );
			(*newObj)[ "Subtype" ] = newName( "Form" );
			(*newObj)[ "FormType" ] = newInt( 1 );
			(*newObj)[ "BBox" ] = RefCountPtr< PDF_Object >( new PDF_Vector( 0.0,
																																			 - bboxAddY.offtype< 1, 0 >( ),
																																			 ( xmax - xmin ).offtype< 1, 0 >( ),
																																			 ( ymax - ymin ).offtype< 1, 0 >( ) ) );
			//			(*newObj)[ "BBox" ] = (*original)[ "BBox" ];
			(*newObj)[ "Matrix" ] = RefCountPtr< PDF_Object >( new PDF_Vector( 1, 0, 0, 1, 0, - depth.offtype< 1, 0 >( ) ) );
			(*newObj)[ "Filter" ] = (*original)[ "Filter" ];
			(*newObj)[ "Length" ] = RefCountPtr< PDF_Object >( new PDF_Int( pdfi->follow< PDF_Int >( (*original)[ "Length" ] )->value( ) ) );
			(*newObj)[ "Resources" ] = deepCopy( (*original)[ "Resources" ], & Kernel::theIndirectObjectCount, & indirectRemap );

			Lang::ElementaryPath2D * bboxpath = new Lang::ElementaryPath2D;
			bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymin ) );
			bboxpath->push_back( new Concrete::PathPoint2D( xmin, ymax ) );
			bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymax ) );
			bboxpath->push_back( new Concrete::PathPoint2D( xmax, ymin ) );
			bboxpath->close( );

			if( dstMap->find( texStr ) != dstMap->end( ) )
				{
					throw Exceptions::InternalError( strrefdup( "Multiply generated TeX label: " + texStr ) );
				}

			RefCountPtr< PDF_Object > indirection = SimplePDF::indirect( newObj, & Kernel::theIndirectObjectCount );
			dstMap->insert( Shapes::Kernel::TeXLabelManager::MapType::value_type( texStr, RefCountPtr< const Lang::XObject >( new Lang::XObject( indirection, RefCountPtr< const Lang::ElementaryPath2D >( bboxpath ) ) ) ) );
		}
}

void
Kernel::Import::free( )
{
	while( ! importSources_.empty( ) )
		{
			importSources_.pop_back( );
		}
}

double
Kernel::Import::pdfNameToDouble( RefCountPtr< PDF_Object > nameObject )
{
	RefCountPtr< PDF_Name > name( nameObject.down_cast< PDF_Name >( ) );
	if( name == NullPtr< PDF_Name >( ) )
		{
			throw( "PDF_out::pdfNameToDouble: The object was not a name" );
		}
	const double pointToBigPointFactor = 72 / 72.27;
	char * end;
	double res = pointToBigPointFactor * strtod( name->name( ).c_str( ), & end );
	if( strcmp( end, "pt" ) != 0 )
		{
			throw( "PDF_out::pdfNameToDouble: Expected \"pt\" to follow length number" );
		}
	return res;
}

