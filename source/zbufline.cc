#include <cmath>

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "astvar.h"
#include "astclass.h"
#include "statetypes.h"
#include "lighttypes.h"
#include "shadingtypes.h"
#include "globals.h"
#include "trianglefunctions.h"
#include "constructorrepresentation.h"

#include <ctype.h>
#include <list>
#include <algorithm>

using namespace Shapes;


#define SPLICEDEBUG( code ) // code

Computation::ZBufLine::ZMap::ZMap( Concrete::Coords3D p0, const Concrete::UnitFloatTriple & d,
														 Concrete::Length eyez )
	: p0_( p0 ), d_( d ),
		k_x_( d.x_ ), k_y_( d.y_ ), k_z_( d.z_ ),
		eyez_( eyez )
{ }

// It is assumed that this funciton is only called with points that actually are on the line.
Concrete::Length
Computation::ZBufLine::ZMap::operator () ( const Concrete::Coords2D & p ) const
{
	// The two (overdetermined) equations are written
	//	 ax alpha = bx
	//	 ay alpha = by
	// where the three dimensional coordinates along the line are given by
	//	 p0_ + alpha d_

	const Concrete::Length ax = eyez_ * k_x_ + p.x_ * k_z_;
	const Concrete::Length ay = eyez_ * k_y_ + p.y_ * k_z_;
	const Physical< 2, 0 > bx = p.x_ * ( eyez_ - p0_.z_ ) - eyez_ * p0_.x_;
	const Physical< 2, 0 > by = p.y_ * ( eyez_ - p0_.z_ ) - eyez_ * p0_.y_;
	const Concrete::Length alpha = ( ax * bx + ay * by ) / ( ax * ax + ay * ay );
	return p0_.z_ + alpha * k_z_;
}

Concrete::Coords3D
Computation::ZBufLine::ZMap::intersection( const Computation::ZBufTriangle::ZMap & plane ) const
{
	// The triangle plane is given by a normal equation:
	//	 < n, x > == b
	// Inserting our point gives
	//	 < n, p0_ + k ( p1_ - p0_ ) > == b
	//	 k < n, p1_ - p0_ > == b - < n, p0_ >

	double a = Concrete::inner( plane.getNormal( ), d_ );
	Concrete::Length b = plane.getM( ) - Concrete::inner( plane.getNormal( ), p0_ );

	if( a == 0 )
		{
			throw "No intersection";
		}

	return p0_ + ( b / a ) * d_;
}


Computation::ZBufLine::ZBufLine( const Computation::StrokedLine3D * painter, const RefCountPtr< const Computation::ZBufLine::ZMap > & zMap, const RefCountPtr< const Bezier::PolyCoeffs< Concrete::Coords2D > > & bezierView, const Concrete::Coords2D & p0, const Concrete::Coords2D & p1 )
	: painter_( painter ), bezierView_( bezierView ), p0_( p0 ), p1_( p1 ), d_( p1 - p0 ), zMap_( zMap )
{ }

Concrete::Length
Computation::ZBufLine::zAt( const Concrete::Coords2D & p ) const
{
	return (*zMap_)( p );
}

double
Computation::ZBufLine::intersectionTime( Concrete::Coords2D p0, Concrete::Coords2D p1 ) const
{
	Concrete::Coords2D d = p1 - p0;

	// The equation to solve is
	//	 p0_ + t d_ == p0 + s d
	// That is,
	//	 ( d_ -d ) ( t s ) == p0 - p0_

	Concrete::Coords2D rhs = p0 - p0_;

	// Borrowing an idea from ZBufTriangle, we don't test for singularity using the determinant since
	// I have no good geometric interpretation of tolerance tests on its value.
	{
		double n_n1 = Concrete::innerScalar( d_, d );
		double n_n_ = Concrete::innerScalar( d_, d_ );
		double n2n2 = Concrete::innerScalar( d, d );

		if( n_n1 * n_n1 > ( 1 - 1e-8 ) * ( n_n_ * n2n2 ) ) // This corresponds to an angle of approximately 0.01 degree.
			{
				return -1;	// Any negative value means no intersection.
			}
	}

	Physical< -2, 0 > invDet = 1. / ( d_.x_ * d.y_ - d_.y_ * d.x_ );
	double t =	 invDet * (	 d.y_ * rhs.x_ - d.x_ * rhs.y_ );
	double s = - invDet * ( - d_.y_ * rhs.x_ + d_.x_ * rhs.y_ );

	if( 0 < s && s < 1 &&
			0 < t && t < 1 )
		{
			return t;
		}
	return -1;	// Any negative value means no intersection.
}

bool
Computation::ZBufLine::overlaps( const ZBufTriangle & other ) const
{
	// Compare Computation::ZBufLine::overlaps( const ZBufLine & other ) const.
	// This test is also performed by testing a set of candidate separating planes.
	// Here this set consists of this line itself together with the three sides of the triangle.

	// We rather call it an overlap than missing small overlaps, because undetected overlaps may make short
	// segments of a line shine through a surface at spurious points.

	// Beginning with this line:
	{
		Concrete::UnitFloatPair normal = p0_.normalizedOrthogonal( p1_ );

		Concrete::Length l0 = Concrete::inner( other.points_[ 0 ], normal );
		Concrete::Length m = Concrete::inner( p0_, normal );
		if( l0 < m - Computation::theTrixelizeOverlapTol )
			{
				if( Concrete::inner( other.points_[ 1 ], normal ) < m - Computation::theTrixelizeOverlapTol &&
						Concrete::inner( other.points_[ 2 ], normal ) < m - Computation::theTrixelizeOverlapTol )
					{
						return false;
					}
			}

		if( l0 > m + Computation::theTrixelizeOverlapTol )
			{
				if( Concrete::inner( other.points_[ 1 ], normal ) > m + Computation::theTrixelizeOverlapTol &&
						Concrete::inner( other.points_[ 2 ], normal ) > m + Computation::theTrixelizeOverlapTol )
					{
						return false;
					}
			}
	}

	// We now test the three sides of the triangle, remembering that only one side is the outside of a triangle side.
	// ***Important***:	Compare ZBufTriangle::oneWayOverlap.
	// Note, though, that ZBufTriangle::oneWayOverlap prefers to answer false when in doubt.
	{
		const std::vector< Concrete::Coords2D > & poly1 = other.points_;
		for( std::vector< Concrete::Coords2D >::const_iterator i0 = poly1.begin( ); i0 != poly1.end( ); ++i0 )
			{
				std::vector< Concrete::Coords2D >::const_iterator i1 = i0;
				++i1;
				if( i1 == poly1.end( ) )
					{
						i1 = poly1.begin( );
					}
				std::vector< Concrete::Coords2D >::const_iterator i2 = i1;
				++i2;
				if( i2 == poly1.end( ) )
					{
						i2 = poly1.begin( );
					}

				const Concrete::Coords2D & p0( *i0 );
				const Concrete::Coords2D & p1( *i1 );

				const Concrete::Length txUnnormed = p1.x_ - p0.x_;
				const Concrete::Length tyUnnormed = p1.y_ - p0.y_;
				const Physical< -1, 0 > invNorm = 1. / hypotPhysical( txUnnormed, tyUnnormed );
				// First we guess what's the inward direction
				double inx( - invNorm * tyUnnormed );
				double iny( invNorm * txUnnormed );
				// Then we check if it needs to be reversed
				{
					const Concrete::Length dx = i2->x_ - p0.x_;
					const Concrete::Length dy = i2->y_ - p0.y_;
					if( dx * inx + dy * iny < Concrete::ZERO_LENGTH )
						{
							inx = -inx;
							iny = -iny;
						}
				}

				bool allOutside = true;
				{
					const Concrete::Coords2D & p = p0_;
					const Concrete::Length dx = p.x_ - p0.x_;
					const Concrete::Length dy = p.y_ - p0.y_;
					if( inx * dx + iny * dy > Computation::theTrixelizeOverlapTol )
						{
							allOutside = false;
						}
				}
				if( allOutside )	// read "still allOutside"
					{
						const Concrete::Coords2D & p = p1_;
						const Concrete::Length dx = p.x_ - p0.x_;
						const Concrete::Length dy = p.y_ - p0.y_;
						if( inx * dx + iny * dy > Computation::theTrixelizeOverlapTol )
							{
								allOutside = false;
							}
					}
				if( allOutside )
					{
						return false;
					}
			}

	}

	return true;
}

bool
Computation::ZBufLine::overlaps( const ZBufLine & other ) const
{
	// Since line segments are convex polyhedra, we can determine overlap by enumerating a set of separating
	// planes which has to include one separating plane if such a plane exists.	Here the only planes to consider
	// are the lines themselves.

	// Currently, the tolerance in this test is used to ensure that a true result really indicates an intersection.
	{
		Concrete::UnitFloatPair normal = p0_.normalizedOrthogonal( p1_ );
		Concrete::Length m = Concrete::inner( normal, p0_ );
		const ZBufLine & tmpLine = other;

		Concrete::Length l0 = Concrete::inner( tmpLine.p0_, normal );
		Concrete::Length l1 = Concrete::inner( tmpLine.p1_, normal );
		if( ( l0 < m + Computation::theTrixelizeOverlapTol &&
					l1 < m + Computation::theTrixelizeOverlapTol ) ||
				( l0 > m - Computation::theTrixelizeOverlapTol &&
					l1 > m - Computation::theTrixelizeOverlapTol ) )
			{
				return false;
			}
	}
	{
		Concrete::UnitFloatPair normal = other.p0_.normalizedOrthogonal( other.p1_ );
		Concrete::Length m = Concrete::inner( normal, other.p0_ );
		const ZBufLine & tmpLine = *this;

		Concrete::Length l0 = Concrete::inner( tmpLine.p0_, normal );
		Concrete::Length l1 = Concrete::inner( tmpLine.p1_, normal );
		if( ( l0 < m + Computation::theTrixelizeOverlapTol &&
					l1 < m + Computation::theTrixelizeOverlapTol ) ||
				( l0 > m - Computation::theTrixelizeOverlapTol &&
					l1 > m - Computation::theTrixelizeOverlapTol ) )
			{
				return false;
			}
	}

	return true;
}

void
Computation::ZBufLine::splice( const ZBufLine * line1, const ZBufLine * line2 , std::list< const Computation::ZBufLine * > * line1Container, std::list< const Computation::ZBufLine * > * line2Container )
{
	// When this function is called, we know that the line segments intersect.

	// First we compute where they intersect and determine which line is in front.

	Concrete::Coords2D p01 = line1->p0_;
	Concrete::Coords2D p02 = line2->p0_;

	Concrete::Coords2D d1 = line1->d_;
	Concrete::Coords2D d2 = line2->d_;

	// What follows is similar to ZBufLine::intersectionTime.

	Concrete::Coords2D rhs = p02 - p01;

	// This time, perhaps more important than ever, the angle is computed to determine if the intersection is well-conditioned.
	// Shall the angle be used to determine how much to cut away?
	{
		double n_n1 = Concrete::innerScalar( d1, d2 );
		double n_n_ = Concrete::innerScalar( d1, d1 );
		double n2n2 = Concrete::innerScalar( d2, d2 );

		if( n_n1 * n_n1 > ( 1 - 1e-8 ) * ( n_n_ * n2n2 ) ) // This corresponds to an angle of approximately 0.01 degree.
			{
				// If the lines are very parallel I don't know what to do, so I just discard one of the lines!...
				std::cerr << "Warning: discarding one of the parallel lines!" << std::endl ;
				//				line1Container->push_back( line1 );
				line2Container->push_back( line2 );

				return;
			}
	}

	Physical< -2, 0 > invDet = 1. / ( d1.x_ * d2.y_ - d1.y_ * d2.x_ );
	double t1 =	 invDet * (	 d2.y_ * rhs.x_ - d2.x_ * rhs.y_ );
	//	double t2 = - invDet * ( - d1.y_ * rhs.x_ + d1.x_ * rhs.y_ );

	// If the lines seems not to overlap, let's assume that they at least almost overlap, and
	// that the bottom one is to be cut anyway.
	// Note that it will lead to infinite loops if we were to return the segments as they came in.
//	 if( t1 < 0 || 1 < t1 ||
//			 t2 < 0 || 1 < t2 )
//		 {
//			 // This disagrees with the presumption that the lines do intersect, but never mind...

//			 line1Container->push_back( line1 );
//			 line2Container->push_back( line2 );

//			 return;
//		 }

	Concrete::Coords2D pInt = p01 + t1 * d1;

	// First we guess...
	const Computation::ZBufLine * topLine = line1;
	const Computation::ZBufLine * botLine = line2;
	std::list< const Computation::ZBufLine * > * topContainer = line1Container;
	std::list< const Computation::ZBufLine * > * botContainer = line2Container;
	// then we test...
	if( line2->zAt( pInt ) > line1->zAt( pInt ) )
		{
			// ... and correct if needed.
			topLine = line2;
			botLine = line1;
			topContainer = line2Container;
			botContainer = line1Container;
		}

	// The line on top is kept as is.
	topContainer->push_back( topLine );

	Concrete::Length cutLength =
		0.5 * topLine->painter_->getMetaState( )->width_ +
		0.5 * botLine->painter_->getMetaState( )->width_;

	Concrete::UnitFloatPair d = botLine->d_.direction( );
	Concrete::Length l0 = ( botLine->p0_ - pInt ).norm( ) - cutLength;
	if( l0 > Computation::theTrixelizeOverlapTol )
		{
			botContainer->push_back( new Computation::ZBufLine( botLine->painter_, botLine->zMap_, botLine->bezierView_, botLine->p0_, pInt - cutLength * d ) );
		}

	Concrete::Length l1 = ( botLine->p1_ - pInt ).norm( ) - cutLength;
	if( l1 > Computation::theTrixelizeOverlapTol )
		{
			botContainer->push_back( new Computation::ZBufLine( botLine->painter_, botLine->zMap_, botLine->bezierView_, pInt + cutLength * d, botLine->p1_ ) );
		}

	// Now we're finished with botLine.
	delete botLine;
}

void
Computation::ZBufLine::splice( const ZBufTriangle & triangle, std::list< const Computation::ZBufLine * > * myLines ) const
{
	//	std::cerr << "Triangle color: " ;
	//	dynamic_cast< const Computation::FilledPolygon3D * >( triangle.painter_ )->getColor( )->show( std::cerr );
	//	std::cerr << std::endl ;

	// Generally, a line may cross a triangle edge at two points, and may penetrate the triangle at one point.
	// If these points are sorted, we'll end up with at most four segments, and visibility can be tested for each.
	// Finally, adjacent visible segments are joined and pushed in the destination container.

	// The points on this segment are parameterized as
	//	 p0_ + t ( p1_ - p0_ )
	// where t obviously is in the range [ 0, 1 ].

	double tIn = HUGE_VAL;	 // "time" when line enters triangle
	double tOut = -HUGE_VAL; // "time" when line exits triangle
	std::vector< double > times;	// All interesting times.
	times.reserve( 6 );
	times.push_back( 0. );
	times.push_back( 1. );

	for( std::vector< Concrete::Coords2D >::const_iterator i0 = triangle.points_.begin( ); i0 != triangle.points_.end( ); ++i0 )
		{
			std::vector< Concrete::Coords2D >::const_iterator i1 = i0;
			++i1;
			if( i1 == triangle.points_.end( ) )
				{
					i1 = triangle.points_.begin( );
				}

			double tmp = intersectionTime( *i0, *i1 );	// the return value is non-positive if there is no intersection
			if( tmp > 0 )
				{
					times.push_back( tmp );
					tIn = std::min( tIn, tmp );
					tOut = std::max( tOut, tmp );
				}
		}

	if( tIn == tOut )
		{
			/* Only one of the times can be the enter/exit time.
			 * Note that the test below really needs to be this robust.
			 */
			if( tIn > 0.5 ? triangle.contains( p0_ + 0.5 * tIn * d_ )
					: ! triangle.contains( p0_ + 0.5 * ( tIn + 1 ) * d_ ) )
				{
					tIn = -HUGE_VAL;
				}
			else
				{
					tOut = HUGE_VAL;
				}
		}

	if( tIn > tOut )
		{
			// This means that the line is enclosed in the triangle.
			tIn = -HUGE_VAL;
			tOut = HUGE_VAL;
		}

	{
		// We now compute the intersection of the triangle and the line.
		// It is no harm to add this point whether or not it lies inside the triangle,
		// since the processing below will join adjacent segments that are visible.

		try
			{
				Concrete::Coords2D pInt = zMap_->intersection( *(triangle.zMap_) ).make2DAutomatic( zMap_->eyez( ) );

				double tmp = Concrete::inner( d_, pInt - p0_ ) / Concrete::inner( d_, d_ );
				if( 0 < tmp && tmp < 1 )
					{
						times.push_back( tmp );
					}
			}
		catch( const char * noIntersectionBall )
			{
				// Never mind.
			}
	}

	std::sort( times.begin( ), times.end( ) );

	{
		// We now have the times in increasing order, and it is time to identify visible segments.

		Concrete::Length halfWidth = painter_->getMetaState( )->width_;

		bool visible = false;
		Concrete::Coords2D pStart( 0, 0 );
		typedef typeof times TimesType;
		TimesType::const_iterator i = times.begin( );
		TimesType::const_iterator iLast = i;
		++i;
		for( ; i != times.end( ); iLast = i, ++i )
			{
				double tMid = 0.5 * ( *iLast + *i );
				bool newVisible = true;
				if( tIn < tMid && tMid < tOut )
					{
						Concrete::Coords2D pMid = p0_ + tMid * d_;
						newVisible = zAt( pMid ) + halfWidth > triangle.zAt( pMid );	// the halfWidth is like a tiebreaker for a line.
					}
				if( newVisible )
					{
						if( ! visible )
							{
								pStart = p0_ + *iLast * d_;
							}
					}
				else
					{
						if( visible )
							{
								Concrete::Coords2D pEnd = p0_ + *iLast * d_;
								if( ( pEnd - pStart ).norm( ) > Computation::theTrixelizeOverlapTol )
									{
										myLines->push_back( new Computation::ZBufLine( painter_, zMap_, bezierView_, pStart, pEnd ) );
									}
							}
					}
				visible = newVisible;
			}

		if( visible )
			{
				Concrete::Coords2D pEnd = p0_ + *iLast * d_;
				if( ( pEnd - pStart ).norm( ) > Computation::theTrixelizeOverlapTol )
					{
						myLines->push_back( new Computation::ZBufLine( painter_, zMap_, bezierView_, pStart, pEnd ) );
					}
			}
	}
}

RefCountPtr< const Lang::PaintedPath2D >
Computation::ZBufLine::stroke2D( ) const
{
	RefCountPtr< Lang::ElementaryPath2D > path = RefCountPtr< Lang::ElementaryPath2D >( new Lang::ElementaryPath2D( ) );

	if( bezierView_ != NullPtr< typeof *bezierView_ >( ) )
		{
			double t0 = 0;
			{
				double t[4];
				bezierView_->hyperplaneIntersections( t, d_, Concrete::innerScalar( d_, p0_ ) );
				if( t[0] == HUGE_VAL )
					{
						throw Exceptions::InternalError( "ZBufLine::stroke2D: Failed to locate intersection for t0." );
					}
				if( t[1] != HUGE_VAL )
					{
						throw Exceptions::MiscellaneousRequirement( "ZBufLine::stroke2D: Problem due to non-monotonicity of the spline." );
					}
				t0 = t[0];
			}
			double t1 = 1;
			{
				double t[4];
				bezierView_->hyperplaneIntersections( t, d_, Concrete::innerScalar( d_, p1_ ) );
				if( t[0] == HUGE_VAL )
					{
						throw Exceptions::InternalError( "ZBufLine::stroke2D: Failed to locate intersection for t1." );
					}
				if( t[1] != HUGE_VAL )
					{
						throw Exceptions::MiscellaneousRequirement( "ZBufLine::stroke2D: Problem due to non-monotonicity of the spline." );
					}
				t1 = t[0];
			}
			Bezier::ControlPoints< Concrete::Coords2D > controls( bezierView_->subSection( t0, t1 ) );
			path->push_back( new Concrete::PathPoint2D( p0_.x_, p0_.y_ ) );
			path->back( )->front_ = new Concrete::Coords2D( controls.p1_ );
			path->push_back( new Concrete::PathPoint2D( p1_.x_, p1_.y_ ) );
			path->back( )->rear_ = new Concrete::Coords2D( controls.p2_ );
		}
	else
		{
			path->push_back( new Concrete::PathPoint2D( p0_.x_, p0_.y_ ) );
			path->push_back( new Concrete::PathPoint2D( p1_.x_, p1_.y_ ) );
		}

	// If we don't cast path to const, the contructor call becomes ambiguous.
	return RefCountPtr< const Lang::PaintedPath2D >( new Lang::PaintedPath2D( painter_->getMetaState( ),
																																						static_cast< RefCountPtr< const Lang::ElementaryPath2D > >( path ),
																																						"S" ) );
}

