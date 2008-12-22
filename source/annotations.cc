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

#include "Shapes_Helpers_decls.h"

#include "annotations.h"
#include "pdfstructure.h"
#include "classtypes.h"
#include "drawabletypes.h"
#include "pathtypes.h"
#include "globals.h"
#include "shapesexceptions.h"

using namespace Shapes;


Lang::AnnotationSite::AnnotationSite( const RefCountPtr< const Lang::Drawable2D > & target, const RefCountPtr< const char > & contentText, const RefCountPtr< const char > & identifier, size_t flags,
																			char borderStyle, const Concrete::Length & borderWidth, const RefCountPtr< const Lang::Dash > & borderDash, double borderCloudy, const Concrete::RGB & color,
																			const RefCountPtr< const Lang::XObject > & appearanceNormal, const RefCountPtr< const Lang::XObject > & appearanceRollover, const RefCountPtr< const Lang::XObject > & appearanceDown )
	: target_( target ), contentText_( contentText ), identifier_( identifier ), flags_( flags ),
		borderStyle_( borderStyle ), borderWidth_( borderWidth ), borderDash_( borderDash ), borderCloudy_( borderCloudy ), color_( color ),
		appearanceNormal_( appearanceNormal ), appearanceRollover_( appearanceRollover ), appearanceDown_( appearanceDown )
{ }

Lang::AnnotationSite::~AnnotationSite( )
{ }

RefCountPtr< const Lang::Class > Lang::AnnotationSite::TypeID( new Lang::SystemFinalClass( strrefdup( "AnnotationSite" ) ) );
TYPEINFOIMPL( AnnotationSite );

RefCountPtr< const Lang::AnnotationSite >
Lang::AnnotationSite::typed_transformed( const Lang::Transform2D & transform ) const
{
	return RefCountPtr< const Lang::AnnotationSite >
		( new AnnotationSite( target_->typed_transformed( transform, target_ ), contentText_, identifier_, flags_,
													borderStyle_, borderWidth_, borderDash_, borderCloudy_, color_,
													appearanceNormal_, appearanceRollover_, appearanceDown_ ) );
}

RefCountPtr< const Lang::Geometric2D >
Lang::AnnotationSite::transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	return typed_transformed( transform );
}

RefCountPtr< const Lang::Geometric3D >
Lang::AnnotationSite::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	throw Exceptions::NotImplemented( "Annotation sites in 3D." );
}

void
Lang::AnnotationSite::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Drawable2D * >( target_.getPtr( ) )->gcMark( marked );
	if( borderDash_ != NullPtr< const Lang::Dash >( ) )
		{
			const_cast< Lang::Dash * >( borderDash_.getPtr( ) )->gcMark( marked );
		}
	if( appearanceNormal_ != NullPtr< const Lang::XObject >( ) )
		{
			const_cast< Lang::XObject * >( appearanceNormal_.getPtr( ) )->gcMark( marked );
		} if( appearanceRollover_ != NullPtr< const Lang::XObject >( ) )
		{
			const_cast< Lang::XObject * >( appearanceRollover_.getPtr( ) )->gcMark( marked );
		}
	if( appearanceDown_ != NullPtr< const Lang::XObject >( ) )
		{
			const_cast< Lang::XObject * >( appearanceDown_.getPtr( ) )->gcMark( marked );
		}
}

RefCountPtr< SimplePDF::PDF_Dictionary >
Lang::AnnotationSite::getDictionary( const char * subtype, const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page ) const
{
	const SimplePDF::PDF_Version::Version FANCY_VERSION = SimplePDF::PDF_Version::PDF_1_2;

	RefCountPtr< SimplePDF::PDF_Dictionary > res = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );

	res->dic[ "Type" ] = SimplePDF::newName( "Annot" );
	res->dic[ "Subtype" ] = SimplePDF::newName( subtype );
	{
		Concrete::Coords2D llcorner( 0, 0 );
		Concrete::Coords2D urcorner( 0, 0 );
		RefCountPtr< const Lang::ElementaryPath2D > theBBox = target_->bbox( Lang::Drawable2D::BOUNDING );
		if( theBBox->empty( ) )
			{
				throw Exceptions::MiscellaneousRequirement( "The annotation target produced an empty bounding box." );
			}
		theBBox->boundingRectangle( & llcorner, & urcorner );
		res->dic[ "Rect" ] = RefCountPtr< SimplePDF::PDF_Vector >
			( new SimplePDF::PDF_Vector( llcorner.x_.offtype< 1, 0 >( ),
																	 llcorner.y_.offtype< 1, 0 >( ),
																	 urcorner.x_.offtype< 1, 0 >( ),
																	 urcorner.y_.offtype< 1, 0 >( ) ) );
	}
	if( contentText_ != NullPtr< const char >( ) )
		{
			res->dic[ "Contents" ] = SimplePDF::newString( contentText_.getPtr( ) );
		}
	res->dic[ "P" ] = i_page;
	if( identifier_ != NullPtr< const char >( ) )
		{
			const SimplePDF::PDF_Version::Version ELEMENT_NAME_VERSION = SimplePDF::PDF_Version::PDF_1_4;
			if( Kernel::the_PDF_version.greaterOrEqual( ELEMENT_NAME_VERSION ) )
				{
					res->dic[ "NM" ] = SimplePDF::newString( identifier_.getPtr( ) );
				}
			else
				{
					Kernel::the_PDF_version.message( ELEMENT_NAME_VERSION, "Annotation identifiier was ignored." );
				}
		}
	res->dic[ "F" ] = SimplePDF::newInt( flags_ );
	{
		RefCountPtr< SimplePDF::PDF_Dictionary > bs = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );

		if( static_cast< double >( borderWidth_.offtype< 1, 0 >( ) ) != 1 )
			{
				bs->dic[ "W" ] = SimplePDF::newFloat( borderWidth_.offtype< 1, 0 >( ) );
			}
		if( borderStyle_ != 'S' )
			{
				char buf[2];
				buf[0] = borderStyle_;
				buf[1] = '\0';
				bs->dic[ "S" ] = SimplePDF::newName( buf );
			}
		if( borderDash_ != NullPtr< const Lang::Dash >( ) )
			{
				bs->dic[ "D" ] = borderDash_->getDashArray( );
			}
		if( borderCloudy_ >= 0 )
			{
				RefCountPtr< SimplePDF::PDF_Dictionary > be = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );
				be->dic[ "S" ] = SimplePDF::newName( "C" );
				be->dic[ "I" ] = SimplePDF::newFloat( borderCloudy_ );
				bs->dic[ "BE" ] = be;
			}

		if( ! bs->dic.empty( ) )
			{
				if( Kernel::the_PDF_version.greaterOrEqual( FANCY_VERSION ) )
					{
						res->dic[ "BS" ] = bs;
					}
				else
					{
						Kernel::the_PDF_version.message( FANCY_VERSION, "Annotation border style was ignored." );
					}
			}
	}

	{
		RefCountPtr< SimplePDF::PDF_Dictionary > ap = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );

		if( appearanceNormal_ != NullPtr< const Lang::XObject >( ) )
			{
				ap->dic[ "N" ] = appearanceNormal_->getResource( );
			}
		if( appearanceRollover_ != NullPtr< const Lang::XObject >( ) )
			{
				ap->dic[ "R" ] = appearanceRollover_->getResource( );
			}
		if( appearanceDown_ != NullPtr< const Lang::XObject >( ) )
			{
				ap->dic[ "D" ] = appearanceDown_->getResource( );
			}

		if( ! ap->dic.empty( ) )
			{
				if( Kernel::the_PDF_version.greaterOrEqual( FANCY_VERSION ) )
					{
						res->dic[ "AP" ] = ap;
					}
				else
					{
						Kernel::the_PDF_version.message( FANCY_VERSION, "Annotation appearance was ignored." );
					}
			}
	}

	if( color_.mean( ) > 0 )
		{
			if( Kernel::the_PDF_version.greaterOrEqual( FANCY_VERSION ) )
				{
					res->dic[ "C"	] = color_.componentVector( );
				}
			else
				{
					Kernel::the_PDF_version.message( FANCY_VERSION, "Annotation color was ignored." );
				}
		}


	return res;
}


Lang::AnnotationBase::AnnotationBase( const RefCountPtr< const Lang::AnnotationSite > & site )
	: site_( site )
{ }

Lang::AnnotationBase::~AnnotationBase( )
{ }

RefCountPtr< const Lang::Class > Lang::AnnotationBase::TypeID( new Lang::SystemFinalClass( strrefdup( "Annotation" ) ) );
TYPEINFOIMPL( AnnotationBase );

void
Lang::AnnotationBase::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< AnnotationSite * >( site_.getPtr( ) )->gcMark( marked );
	this->subtypeGcMark( marked );
}


RefCountPtr< const Lang::Geometric3D >
Lang::AnnotationBase::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	throw Exceptions::NotImplemented( "Annotations in 3D." );
}


Lang::TextAnnotation::TextAnnotation( const RefCountPtr< const Lang::AnnotationSite > & site, const RefCountPtr< const char > & title, bool open, const RefCountPtr< const char > & icon )
	: Lang::AnnotationBase( site ), title_( title ), open_( open ), icon_( icon )
{ }

Lang::TextAnnotation::~TextAnnotation( )
{ }

RefCountPtr< const Lang::Geometric2D >
Lang::TextAnnotation::transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	return RefCountPtr< const Lang::Geometric2D >
		( new TextAnnotation( site_->typed_transformed( transform ), title_, open_, icon_ ) );
}

RefCountPtr< SimplePDF::PDF_Dictionary >
Lang::TextAnnotation::getDictionary( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, const std::map< RefCountPtr< const char >, RefCountPtr< SimplePDF::PDF_Vector >, charRefPtrLess > & namedDestinations ) const
{
	RefCountPtr< SimplePDF::PDF_Dictionary > res = site_->getDictionary( "Text", i_page );
	if( title_ != NullPtr< const char >( ) )
		{
			res->dic[ "T" ] = SimplePDF::newString( title_.getPtr( ) );
		}
	if( open_ )
		{
			res->dic[ "Open" ] = SimplePDF::theTrue;
		}
	if( icon_ != NullPtr< const char >( ) )
		{
			res->dic[ "Name" ] = SimplePDF::newString( icon_.getPtr( ) );
		}

	return res;
}

void
Lang::TextAnnotation::subtypeGcMark( Kernel::GCMarkedSet & marked )
{
	/* Nothing to mark.
	 */
}


Lang::LinkAnnotation::LinkAnnotation( const RefCountPtr< const Lang::AnnotationSite > & site, const Ast::SourceLocation & loc, char highlight, const RefCountPtr< const char > & destination, Lang::LinkAnnotation::Kind kind )
	: Lang::AnnotationBase( site ), loc_( loc ), highlight_( highlight ), destination_( destination ), kind_( kind )
{ }

Lang::LinkAnnotation::~LinkAnnotation( )
{ }

RefCountPtr< const Lang::Geometric2D >
Lang::LinkAnnotation::transformed( const Lang::Transform2D & transform, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	return RefCountPtr< const Lang::Geometric2D >
		( new LinkAnnotation( site_->typed_transformed( transform ), loc_, highlight_, destination_, kind_ ) );
}

RefCountPtr< SimplePDF::PDF_Dictionary >
Lang::LinkAnnotation::getDictionary( const RefCountPtr< SimplePDF::PDF_Indirect_out > & i_page, const std::map< RefCountPtr< const char >, RefCountPtr< SimplePDF::PDF_Vector >, charRefPtrLess > & namedDestinations ) const
{
	RefCountPtr< SimplePDF::PDF_Dictionary > res = site_->getDictionary( "Link", i_page );
	if( highlight_ != 'I' )
		{
			char buf[2];
			buf[0] = highlight_;
			buf[1] = '\0';
			res->dic[ "H" ] = SimplePDF::newName( buf );
		}

	switch( kind_ )
		{
		case LAUNCH_FILE:
			{
				RefCountPtr< SimplePDF::PDF_Dictionary > action = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );
				res->dic[ "A" ] = action;
				action->dic[ "S" ] = SimplePDF::newName( "Launch" );
				action->dic[ "F" ] = SimplePDF::newString( destination_.getPtr( ) );
			}
			break;
		case LAUNCH_URI:
			{
				RefCountPtr< SimplePDF::PDF_Dictionary > action = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );
				res->dic[ "A" ] = action;
				action->dic[ "S" ] = SimplePDF::newName( "URI" );
				action->dic[ "URI" ] = SimplePDF::newString( destination_.getPtr( ) );
				//			RefCountPtr< SimplePDF::PDF_Dictionary > filespec = RefCountPtr< SimplePDF::PDF_Dictionary >( new SimplePDF::PDF_Dictionary );
				//			action->dic[ "F" ] = filespec;
				//			filespec->dic[ "FS" ] = SimplePDF::newName( "URI" );
				//			filespec->dic[ "F" ] = SimplePDF::newString( destination_.getPtr( ) );
			}
			break;
		case DOC_LINK:
			{
				if( namedDestinations.find( destination_ ) == namedDestinations.end( ) )
					{
						throw new Exceptions::UndefinedCrossRef( loc_, destination_ );
					}
				res->dic[ "Dest" ] = SimplePDF::newString( destination_.getPtr( ) );
			}
			break;
		default:
			throw Exceptions::InternalError( "Link annotation type out of range." );
		}

	return res;
}

void
Lang::LinkAnnotation::subtypeGcMark( Kernel::GCMarkedSet & marked )
{
	/* Nothing to mark.
	 */
}
