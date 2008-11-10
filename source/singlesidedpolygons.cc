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

#include <cmath>

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "astvar.h"
#include "astclass.h"
#include "statetypes.h"
#include "lighttypes.h"
#include "shadingtypes.h"
#include "globals.h"
#include "trianglefunctions.h"

#include <list>
#include <algorithm>

using namespace Shapes;


std::string
binaryFormat( unsigned int value, size_t bits )
{
	std::ostringstream oss;
	for( int i = bits - 1; i >= 0; --i )
		{
			oss << ( ( ( ( value >> i ) & (size_t)( 1 ) ) != 0 ) ? '1' : 'o' ) ;
		}
	return oss.str( );
}


Computation::SingleSidedPolygon3DGray::SingleSidedPolygon3DGray( const RefCountPtr< const Computation::FacetInterpolatorGray > & interpolator,
																																 bool singleSided,
																																 const Concrete::UnitFloatTriple & polygonUnitNormal,
																																 Concrete::Length m,
																																 Concrete::Length tiebreaker,
																																 Concrete::Length viewResolution,
																																 Computation::FacetShadeOrder shadeOrder )
	: Computation::PaintedPolygon3D( singleSided, polygonUnitNormal, m, tiebreaker ),
		interpolator_( interpolator ),
		viewResolution_( viewResolution ),
		shadeOrder_( shadeOrder )
{ }

Computation::SingleSidedPolygon3DGray::~SingleSidedPolygon3DGray( )
{ }

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DGray::polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	// In the current implementation, the color is only computed at one point.	Later, this
	// shall be extended to nice shadings.

	Concrete::Length eyez = dyn->getEyeZ( );

	if( viewResolution_ == Concrete::HUGE_LENGTH &&
			shadeOrder_ == 0 )
		{
			return simple_polygon_to2D( eyez, tf, lights );
		}

	PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > vertexMem;
	PtrOwner_back_Access< std::list< const FacetLatticeEdge * > > edgeMem;
	PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > lattice;

	makeLattice( & lattice, & edgeMem, & vertexMem, viewResolution_, eyez, tf );

	switch( shadeOrder_ )
		{
		case 0:
			return render0( eyez, tf, lights, & lattice );
		case 1:
			{
				const SimplePDF::PDF_Version::Version SHADE_VERSION = SimplePDF::PDF_Version::PDF_1_3;
				if( Kernel::the_PDF_version.greaterOrEqual( SHADE_VERSION ) )
					{
						return render1( eyez, tf, lights, & lattice );
					}
				else
					{
						Kernel::the_PDF_version.message( SHADE_VERSION, "Replacing shade order 1 by 0." );
						return render0( eyez, tf, lights, & lattice );
					}
			}
		case 2:
			{
				const SimplePDF::PDF_Version::Version SHADE_VERSION = SimplePDF::PDF_Version::PDF_1_3;
				if( Kernel::the_PDF_version.greaterOrEqual( SHADE_VERSION ) )
					{
						return render2( eyez, tf, lights, & lattice, vertexMem );
					}
				else
					{
						Kernel::the_PDF_version.message( SHADE_VERSION, "Replacing shade order 2 by 0." );
						return render0( eyez, tf, lights, & lattice );
					}
			}
		default:
			throw Exceptions::InternalError( "SingleSidedPolygon3DGray::polygon_to2D: shadeOrder_ out of range." );
		}
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DGray::simple_polygon_to2D( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	Kernel::GraphicsState * metaStatePtr = new Kernel::GraphicsState( true );

	metaStatePtr->nonStrokingColor_ = interpolator_->compute( tf, lights, computeMean( ).transformed( tf ), eyez );

	RefCountPtr< const Kernel::GraphicsState > metaState( metaStatePtr );

	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	path->close( );
	if( tf.isIdentity( ) )
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->make2D( eyez ) ) );
				}
		}
	else
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->transformed( tf ).make2D( eyez ) ) );
				}
		}

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::PaintedPolygon2D( metaState, path ) );
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DGray::render0( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const
{
	RefCountPtr< const Lang::Group2D > contents = Lang::THE_NULL2D;

	while( ! lattice->empty( ) )
		{
			const Computation::FacetLatticeTriangle * triangle = lattice->back( );
			contents = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( triangle->paint( interpolator_, lights, eyez ),
																																						contents,
																																						Kernel::THE_DEFAULT_STATE ) );
			delete triangle;
			lattice->pop_back( );
		}

	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	path->close( );
	if( tf.isIdentity( ) )
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->make2D( eyez ) ) );
				}
		}
	else
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->transformed( tf ).make2D( eyez ) ) );
				}
		}

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::BBoxed2D( contents, path ) );
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DGray::render1( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const
{
	throw Exceptions::NotImplemented( "SingleSidedPolygon3DGray::render1" );
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DGray::render2( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice, const PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > & vertexMem ) const
{
	RefCountPtr< Lang::ElementaryPath2D > bbox = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	bbox->close( );
	if( tf.isIdentity( ) )
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					bbox->push_back( new Concrete::PathPoint2D( i->make2D( eyez ) ) );
				}
		}
	else
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					bbox->push_back( new Concrete::PathPoint2D( i->transformed( tf ).make2D( eyez ) ) );
				}
		}


	std::vector< RefCountPtr< const Lang::Gray > > vertexColors;
	{
		vertexColors.reserve( vertexMem.size( ) );
		typedef typeof vertexMem ListType;
		for( ListType::const_iterator i = vertexMem.begin( ); i != vertexMem.end( ); ++i )
			{
				vertexColors.push_back( interpolator_->compute( tf, lights, (*i)->p3D_.transformed( tf ), eyez ) );
			}
	}

	RefCountPtr< const Lang::ColorSpace > colorSpace = Lang::THE_COLOR_SPACE_DEVICE_GRAY;

	RefCountPtr< SimplePDF::PDF_Stream_out > form;

	RefCountPtr< SimplePDF::PDF_Object > indirection = SimplePDF::indirect( form, & Kernel::theIndirectObjectCount );

	(*form)[ "Subtype" ] = SimplePDF::newName( "Shading" );
	(*form)[ "ShadingType" ] = SimplePDF::newInt( 4 );
	(*form)[ "ColorSpace" ] = colorSpace->name( );
	//	(*form)[ "BBox" ] =
	//	(*form)[ "AntiAlias" ] =

	const size_t BITS_PER_COORDINATE = 32;
	const size_t BITS_PER_COMPONENT = 12;
	const size_t BITS_PER_FLAG = 4;
	const size_t NUMBER_OF_COMPONENTS = colorSpace->numberOfComponents( );
	const size_t NUMBER_OF_COORDINATES = 2;	// don't change!

	if( ( NUMBER_OF_COORDINATES * BITS_PER_COORDINATE + NUMBER_OF_COMPONENTS* BITS_PER_COMPONENT + BITS_PER_FLAG ) % 8 != 0 )
		{
			throw Exceptions::InternalError( "The sizes of thins don't add upp to a whole number of bytes!" );
		}

	(*form)[ "BitsPerCoordinate" ] = SimplePDF::newInt( BITS_PER_COORDINATE );
	(*form)[ "BitsPerComponent" ] = SimplePDF::newInt( BITS_PER_COMPONENT );
	(*form)[ "BitsPerFlag" ] = SimplePDF::newInt( BITS_PER_FLAG );

	Concrete::Coords2D x0y0( 0, 0 );
	Concrete::Coords2D x1y1( 0, 0 );

	if( ! bbox->boundingRectangle( & x0y0, & x1y1 ) )
		{
			throw Exceptions::InternalError( "SingleSidedPolygon3DGray::render2: Polygon without bounding box!" );
		}
	Concrete::Length bboxWidth = x1y1.x_ - x0y0.x_;
	Concrete::Length bboxHeight = x1y1.y_ - x0y0.y_;

	RefCountPtr< SimplePDF::PDF_Vector > decodeArray = RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( ) );

	(*form)[ "Decode" ] = decodeArray;

	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x0y0.x_ ) ) );
	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x1y1.x_ ) ) );
	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x0y0.y_ ) ) );
	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x1y1.y_ ) ) );
	for( size_t i = 0; i < NUMBER_OF_COMPONENTS; ++i )
		{
			decodeArray->vec.push_back( SimplePDF::newFloat( 0. ) );
			decodeArray->vec.push_back( SimplePDF::newFloat( 1. ) );
		}

	const Computation::FacetLatticeVertex * va = 0;
	const Computation::FacetLatticeVertex * vb = 0;
	const Computation::FacetLatticeVertex * vc = 0;

	{
		// The first triangle is chosen at random.	We have no previous vertexes to consider.
		const Computation::FacetLatticeTriangle * current = lattice->front( );
		lattice->pop_front( );
		current->getVertexes( & va, & vb, & vc );
		delete current;

		writePacked( form->data,
								 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
								 ( va->p2D_.x_ - x0y0.x_ ) / bboxWidth,
								 ( va->p2D_.y_ - x0y0.y_ ) / bboxHeight,
								 vertexColors[ va->i_ ]->components( ), 0 );
		writePacked( form->data,
								 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
								 ( vb->p2D_.x_ - x0y0.x_ ) / bboxWidth,
								 ( vb->p2D_.y_ - x0y0.y_ ) / bboxHeight,
								 vertexColors[ vb->i_ ]->components( ), 0 );
		writePacked( form->data,
								 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
								 ( vc->p2D_.x_ - x0y0.x_ ) / bboxWidth,
								 ( vc->p2D_.y_ - x0y0.y_ ) / bboxHeight,
								 vertexColors[ vc->i_ ]->components( ), 0 );
	}

	while( ! lattice->empty( ) )
		{
			typedef typeof *lattice ListType;
			bool found = false;
			for( ListType::iterator i = lattice->begin( ); i != lattice->end( ); ++i ) // we could almost have used a const_iterator here,
				// but since the last thing we do is erase at i, a non-const iterator must be used.
				{
					const Computation::FacetLatticeTriangle * current = *i;
					unsigned char flag = current->extendLattice( & va, & vb, & vc );
					if( flag != 3 )
						{
							found = true;

							lattice->erase( i );

							writePacked( form->data,
													 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
													 ( vc->p2D_.x_ - x0y0.x_ ) / bboxWidth,
													 ( vc->p2D_.y_ - x0y0.y_ ) / bboxHeight,
													 vertexColors[ vc->i_ ]->components( ), flag );

							delete current;
							break;
						}
				}

			if( ! found )
				{
					// The first triangle is chosen at random.	We have no previous vertexes to consider.
					const Computation::FacetLatticeTriangle * current = lattice->front( );
					lattice->pop_front( );
					current->getVertexes( & va, & vb, & vc );
					delete current;

					writePacked( form->data,
											 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
											 ( va->p2D_.x_ - x0y0.x_ ) / bboxWidth,
											 ( va->p2D_.y_ - x0y0.y_ ) / bboxHeight,
											 vertexColors[ va->i_ ]->components( ), 0 );
					writePacked( form->data,
											 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
											 ( vb->p2D_.x_ - x0y0.x_ ) / bboxWidth,
											 ( vb->p2D_.y_ - x0y0.y_ ) / bboxHeight,
											 vertexColors[ vb->i_ ]->components( ), 0 );
					writePacked( form->data,
											 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
											 ( vc->p2D_.x_ - x0y0.x_ ) / bboxWidth,
											 ( vc->p2D_.y_ - x0y0.y_ ) / bboxHeight,
											 vertexColors[ vc->i_ ]->components( ), 0 );
				}
		}

	RefCountPtr< const Lang::Type4Shading > contents
		= RefCountPtr< const Lang::Type4Shading >( new Lang::Type4Shading( indirection, bbox ) );

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::BBoxed2D( contents, bbox ) );

}

void
Computation::SingleSidedPolygon3DGray::writePacked( std::ostream & os,
																										const size_t BITS_PER_COORDINATE, const size_t BITS_PER_COMPONENT, const size_t BITS_PER_FLAG,
																										const double x, const double y, const Concrete::Gray & color, const unsigned char flag )
{
	char rest = 0;	// Unused bits must be zero!
	size_t restAvail = 8;		// Number of unused bits in <rest>.

	{
		size_t BITS = BITS_PER_FLAG;
		writePackedValue( os, & rest, & restAvail, flag, BITS );
	}

	{
		size_t BITS = BITS_PER_COORDINATE;
		unsigned int MAX = UINT_MAX >> ( 8 * sizeof( unsigned int ) - BITS ) ;
		writePackedValue( os, & rest, & restAvail, (unsigned int)( x * MAX ), BITS );
		writePackedValue( os, & rest, & restAvail, (unsigned int)( y * MAX ), BITS );
	}

	{
		size_t BITS = BITS_PER_COMPONENT;
		unsigned int MAX = UINT_MAX >> ( 8 * sizeof( unsigned int ) - BITS ) ;
		writePackedValue( os, & rest, & restAvail, (unsigned int)( color.gr_ * MAX ), BITS );
	}

	if( restAvail < 8 )
		{
			os.write( & rest, 1 );
		}

}

void
Computation::SingleSidedPolygon3DGray::writePackedValue( std::ostream & os, char * rest, size_t * restAvail,
																												 unsigned int val, size_t bits )
{
	if( bits >= *restAvail )
		{
			char tmp = ( val >> ( bits - *restAvail ) );
			*rest |= tmp;

			val %= ( 1 << ( bits - *restAvail ) );
			bits -= *restAvail;

			os.write( rest, 1 );
			*rest = 0;
			*restAvail = 8;

			while( bits >= 8 )
				{
					char tmp = ( val >> ( bits - 8 ) );
					os.write( & tmp, 1 );
					val %= ( 1 << ( bits - 8 ) );

					bits -= 8;
				}
		}

	// By now we know that bits <= 7
	if( bits > *restAvail )
		{
			char tmp = ( val >> ( bits - *restAvail ) );
			*rest |= tmp;

			bits -= *restAvail;

			os.write( rest, 1 );
			*rest = 0;
			*restAvail = 8;

			// This time there are no full bytes to write.
		}

	// By now we know that bits <= 6 and *restavail == 8
	if( bits > 0 )
		{
			char tmp = ( val << ( 8 - bits ) );
			*rest |= tmp;
			*restAvail -= bits;
		}
}


RefCountPtr< const Lang::Color >
Computation::SingleSidedPolygon3DGray::getColor( ) const
{
	// This color is only used for debugging, so it doesn't matter that we don't care about lightening.
	std::cerr << "Warning:	SingleSidedPolygon3DGray::getColor just returns the lightMultiply coefficients as a color." << std::endl ;
	return interpolator_->getDebugColor( );
}

void
Computation::SingleSidedPolygon3DGray::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Computation::FacetInterpolatorGray * >( interpolator_.getPtr( ) )->gcMark( marked );
}


Computation::SingleSidedPolygon3DRGB::SingleSidedPolygon3DRGB( const RefCountPtr< const Computation::FacetInterpolatorRGB > & interpolator,
																																 bool singleSided,
																																 const Concrete::UnitFloatTriple & polygonUnitNormal,
																																 Concrete::Length m,
																																 Concrete::Length tiebreaker,
																																 Concrete::Length viewResolution,
																																 Computation::FacetShadeOrder shadeOrder )
	: Computation::PaintedPolygon3D( singleSided, polygonUnitNormal, m, tiebreaker ),
		interpolator_( interpolator ),
		viewResolution_( viewResolution ),
		shadeOrder_( shadeOrder )
{ }

Computation::SingleSidedPolygon3DRGB::~SingleSidedPolygon3DRGB( )
{ }

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DRGB::polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	// In the current implementation, the color is only computed at one point.	Later, this
	// shall be extended to nice shadings.

	Concrete::Length eyez = dyn->getEyeZ( );

	if( viewResolution_ == Concrete::HUGE_LENGTH &&
			shadeOrder_ == 0 )
		{
			return simple_polygon_to2D( eyez, tf, lights );
		}

	PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > vertexMem;
	PtrOwner_back_Access< std::list< const FacetLatticeEdge * > > edgeMem;
	PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > lattice;

	makeLattice( & lattice, & edgeMem, & vertexMem, viewResolution_, eyez, tf );

	switch( shadeOrder_ )
		{
		case 0:
			return render0( eyez, tf, lights, & lattice );
		case 1:
			{
				const SimplePDF::PDF_Version::Version SHADE_VERSION = SimplePDF::PDF_Version::PDF_1_3;
				if( Kernel::the_PDF_version.greaterOrEqual( SHADE_VERSION ) )
					{
						return render1( eyez, tf, lights, & lattice );
					}
				else
					{
						Kernel::the_PDF_version.message( SHADE_VERSION, "Replacing shade order 1 by 0." );
						return render0( eyez, tf, lights, & lattice );
					}
			}
		case 2:
			{
				const SimplePDF::PDF_Version::Version SHADE_VERSION = SimplePDF::PDF_Version::PDF_1_3;
				if( Kernel::the_PDF_version.greaterOrEqual( SHADE_VERSION ) )
					{
						return render2( eyez, tf, lights, & lattice, vertexMem );
					}
				else
					{
						Kernel::the_PDF_version.message( SHADE_VERSION, "Replacing shade order 2 by 0." );
						return render0( eyez, tf, lights, & lattice );
					}
			}
		default:
			throw Exceptions::InternalError( "SingleSidedPolygon3DRGB::polygon_to2D: shadeOrder_ out of range." );
		}
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DRGB::simple_polygon_to2D( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	Kernel::GraphicsState * metaStatePtr = new Kernel::GraphicsState( true );

	metaStatePtr->nonStrokingColor_ = interpolator_->compute( tf, lights, computeMean( ).transformed( tf ), eyez );

	RefCountPtr< const Kernel::GraphicsState > metaState( metaStatePtr );

	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	path->close( );
	if( tf.isIdentity( ) )
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->make2D( eyez ) ) );
				}
		}
	else
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->transformed( tf ).make2D( eyez ) ) );
				}
		}

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::PaintedPolygon2D( metaState, path ) );
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DRGB::render0( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const
{
	RefCountPtr< const Lang::Group2D > contents = Lang::THE_NULL2D;

	while( ! lattice->empty( ) )
		{
			const Computation::FacetLatticeTriangle * triangle = lattice->back( );
			contents = RefCountPtr< const Lang::Group2D >( new Lang::GroupPair2D( triangle->paint( interpolator_, lights, eyez ),
																																						contents,
																																						Kernel::THE_DEFAULT_STATE ) );
			delete triangle;
			lattice->pop_back( );
		}

	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	path->close( );
	if( tf.isIdentity( ) )
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->make2D( eyez ) ) );
				}
		}
	else
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					path->push_back( new Concrete::PathPoint2D( i->transformed( tf ).make2D( eyez ) ) );
				}
		}

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::BBoxed2D( contents, path ) );
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DRGB::render1( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const
{
	throw Exceptions::NotImplemented( "SingleSidedPolygon3DRGB::render1" );
}

RefCountPtr< const Lang::PaintedPolygon2D >
Computation::SingleSidedPolygon3DRGB::render2( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice, const PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > & vertexMem ) const
{
	RefCountPtr< Lang::ElementaryPath2D > bbox = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );
	bbox->close( );
	if( tf.isIdentity( ) )
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					bbox->push_back( new Concrete::PathPoint2D( i->make2D( eyez ) ) );
				}
		}
	else
		{
			typedef typeof points_ ListType;
			for( ListType::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
				{
					bbox->push_back( new Concrete::PathPoint2D( i->transformed( tf ).make2D( eyez ) ) );
				}
		}


	std::vector< RefCountPtr< const Lang::RGB > > vertexColors;
	{
		vertexColors.reserve( vertexMem.size( ) );
		typedef typeof vertexMem ListType;
		for( ListType::const_iterator i = vertexMem.begin( ); i != vertexMem.end( ); ++i )
			{
				vertexColors.push_back( interpolator_->compute( tf, lights, (*i)->p3D_.transformed( tf ), eyez ) );
			}
	}

	RefCountPtr< const Lang::ColorSpace > colorSpace = Lang::THE_COLOR_SPACE_DEVICE_RGB;

	RefCountPtr< SimplePDF::PDF_Stream_out > form;

	RefCountPtr< SimplePDF::PDF_Object > indirection = SimplePDF::indirect( form, & Kernel::theIndirectObjectCount );

	(*form)[ "Subtype" ] = SimplePDF::newName( "Shading" );
	(*form)[ "ShadingType" ] = SimplePDF::newInt( 4 );
	(*form)[ "ColorSpace" ] = colorSpace->name( );
	//	(*form)[ "BBox" ] =
	//	(*form)[ "AntiAlias" ] =

	const size_t BITS_PER_COORDINATE = 32;
	const size_t BITS_PER_COMPONENT = 12;
	const size_t BITS_PER_FLAG = 4;
	const size_t NUMBER_OF_COMPONENTS = colorSpace->numberOfComponents( );
	const size_t NUMBER_OF_COORDINATES = 2;	// don't change!

	if( ( NUMBER_OF_COORDINATES * BITS_PER_COORDINATE + NUMBER_OF_COMPONENTS* BITS_PER_COMPONENT + BITS_PER_FLAG ) % 8 != 0 )
		{
			throw Exceptions::InternalError( "The sizes of thins don't add upp to a whole number of bytes!" );
		}

	(*form)[ "BitsPerCoordinate" ] = SimplePDF::newInt( BITS_PER_COORDINATE );
	(*form)[ "BitsPerComponent" ] = SimplePDF::newInt( BITS_PER_COMPONENT );
	(*form)[ "BitsPerFlag" ] = SimplePDF::newInt( BITS_PER_FLAG );

	Concrete::Coords2D x0y0( 0, 0 );
	Concrete::Coords2D x1y1( 0, 0 );

	if( ! bbox->boundingRectangle( & x0y0, & x1y1 ) )
		{
			throw Exceptions::InternalError( "SingleSidedPolygon3DRGB::render2: Polygon without bounding box!" );
		}
	Concrete::Length bboxWidth = x1y1.x_ - x0y0.x_;
	Concrete::Length bboxHeight = x1y1.y_ - x0y0.y_;

	RefCountPtr< SimplePDF::PDF_Vector > decodeArray = RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( ) );

	(*form)[ "Decode" ] = decodeArray;

	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x0y0.x_ ) ) );
	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x1y1.x_ ) ) );
	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x0y0.y_ ) ) );
	decodeArray->vec.push_back( SimplePDF::newFloat( Concrete::Length::offtype( x1y1.y_ ) ) );
	for( size_t i = 0; i < NUMBER_OF_COMPONENTS; ++i )
		{
			decodeArray->vec.push_back( SimplePDF::newFloat( 0. ) );
			decodeArray->vec.push_back( SimplePDF::newFloat( 1. ) );
		}

	const Computation::FacetLatticeVertex * va = 0;
	const Computation::FacetLatticeVertex * vb = 0;
	const Computation::FacetLatticeVertex * vc = 0;

	{
		// The first triangle is chosen at random.	We have no previous vertexes to consider.
		const Computation::FacetLatticeTriangle * current = lattice->front( );
		lattice->pop_front( );
		current->getVertexes( & va, & vb, & vc );
		delete current;

		writePacked( form->data,
								 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
								 ( va->p2D_.x_ - x0y0.x_ ) / bboxWidth,
								 ( va->p2D_.y_ - x0y0.y_ ) / bboxHeight,
								 vertexColors[ va->i_ ]->components( ), 0 );
		writePacked( form->data,
								 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
								 ( vb->p2D_.x_ - x0y0.x_ ) / bboxWidth,
								 ( vb->p2D_.y_ - x0y0.y_ ) / bboxHeight,
								 vertexColors[ vb->i_ ]->components( ), 0 );
		writePacked( form->data,
								 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
								 ( vc->p2D_.x_ - x0y0.x_ ) / bboxWidth,
								 ( vc->p2D_.y_ - x0y0.y_ ) / bboxHeight,
								 vertexColors[ vc->i_ ]->components( ), 0 );
	}

	while( ! lattice->empty( ) )
		{
			typedef typeof *lattice ListType;
			bool found = false;
			for( ListType::iterator i = lattice->begin( ); i != lattice->end( ); ++i ) // we could almost have used a const_iterator here,
				// but since the last thing we do is erase at i, a non-const iterator must be used.
				{
					const Computation::FacetLatticeTriangle * current = *i;
					unsigned char flag = current->extendLattice( & va, & vb, & vc );
					if( flag != 3 )
						{
							found = true;

							lattice->erase( i );

							writePacked( form->data,
													 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
													 ( vc->p2D_.x_ - x0y0.x_ ) / bboxWidth,
													 ( vc->p2D_.y_ - x0y0.y_ ) / bboxHeight,
													 vertexColors[ vc->i_ ]->components( ), flag );

							delete current;
							break;
						}
				}

			if( ! found )
				{
					// The first triangle is chosen at random.	We have no previous vertexes to consider.
					const Computation::FacetLatticeTriangle * current = lattice->front( );
					lattice->pop_front( );
					current->getVertexes( & va, & vb, & vc );
					delete current;

					writePacked( form->data,
											 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
											 ( va->p2D_.x_ - x0y0.x_ ) / bboxWidth,
											 ( va->p2D_.y_ - x0y0.y_ ) / bboxHeight,
											 vertexColors[ va->i_ ]->components( ), 0 );
					writePacked( form->data,
											 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
											 ( vb->p2D_.x_ - x0y0.x_ ) / bboxWidth,
											 ( vb->p2D_.y_ - x0y0.y_ ) / bboxHeight,
											 vertexColors[ vb->i_ ]->components( ), 0 );
					writePacked( form->data,
											 BITS_PER_COORDINATE, BITS_PER_COMPONENT, BITS_PER_FLAG,
											 ( vc->p2D_.x_ - x0y0.x_ ) / bboxWidth,
											 ( vc->p2D_.y_ - x0y0.y_ ) / bboxHeight,
											 vertexColors[ vc->i_ ]->components( ), 0 );
				}
		}

	RefCountPtr< const Lang::Type4Shading > contents
		= RefCountPtr< const Lang::Type4Shading >( new Lang::Type4Shading( indirection, bbox ) );

	return RefCountPtr< const Lang::PaintedPolygon2D >( new Lang::BBoxed2D( contents, bbox ) );

}

void
Computation::SingleSidedPolygon3DRGB::writePacked( std::ostream & os,
																										const size_t BITS_PER_COORDINATE, const size_t BITS_PER_COMPONENT, const size_t BITS_PER_FLAG,
																										const double x, const double y, const Concrete::RGB & color, const unsigned char flag )
{
	char rest = 0;	// Unused bits must be zero!
	size_t restAvail = 8;		// Number of unused bits in <rest>.

	{
		size_t BITS = BITS_PER_FLAG;
		writePackedValue( os, & rest, & restAvail, flag, BITS );
	}

	{
		size_t BITS = BITS_PER_COORDINATE;
		unsigned int MAX = UINT_MAX >> ( 8 * sizeof( unsigned int ) - BITS ) ;
		writePackedValue( os, & rest, & restAvail, (unsigned int)( x * MAX ), BITS );
		writePackedValue( os, & rest, & restAvail, (unsigned int)( y * MAX ), BITS );
	}

	{
		size_t BITS = BITS_PER_COMPONENT;
		unsigned int MAX = UINT_MAX >> ( 8 * sizeof( unsigned int ) - BITS ) ;
		writePackedValue( os, & rest, & restAvail, (unsigned int)( color.r_ * MAX ), BITS );
		writePackedValue( os, & rest, & restAvail, (unsigned int)( color.g_ * MAX ), BITS );
		writePackedValue( os, & rest, & restAvail, (unsigned int)( color.b_ * MAX ), BITS );
	}

	if( restAvail < 8 )
		{
			os.write( & rest, 1 );
		}

}

void
Computation::SingleSidedPolygon3DRGB::writePackedValue( std::ostream & os, char * rest, size_t * restAvail,
																												 unsigned int val, size_t bits )
{
	if( bits >= *restAvail )
		{
			char tmp = ( val >> ( bits - *restAvail ) );
			*rest |= tmp;

			val %= ( 1 << ( bits - *restAvail ) );
			bits -= *restAvail;

			os.write( rest, 1 );
			*rest = 0;
			*restAvail = 8;

			while( bits >= 8 )
				{
					char tmp = ( val >> ( bits - 8 ) );
					os.write( & tmp, 1 );
					val %= ( 1 << ( bits - 8 ) );

					bits -= 8;
				}
		}

	// By now we know that bits <= 7
	if( bits > *restAvail )
		{
			char tmp = ( val >> ( bits - *restAvail ) );
			*rest |= tmp;

			bits -= *restAvail;

			os.write( rest, 1 );
			*rest = 0;
			*restAvail = 8;

			// This time there are no full bytes to write.
		}

	// By now we know that bits <= 6 and *restavail == 8
	if( bits > 0 )
		{
			char tmp = ( val << ( 8 - bits ) );
			*rest |= tmp;
			*restAvail -= bits;
		}
}


RefCountPtr< const Lang::Color >
Computation::SingleSidedPolygon3DRGB::getColor( ) const
{
	// This color is only used for debugging, so it doesn't matter that we don't care about lightening.
	std::cerr << "Warning:	SingleSidedPolygon3DRGB::getColor just returns the lightMultiply coefficients as a color." << std::endl ;
	return interpolator_->getDebugColor( );
}

void
Computation::SingleSidedPolygon3DRGB::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Computation::FacetInterpolatorRGB * >( interpolator_.getPtr( ) )->gcMark( marked );
}
