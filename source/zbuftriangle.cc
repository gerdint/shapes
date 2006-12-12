#include <cmath>

#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "metapdfastexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "metapdfastvar.h"
#include "metapdfastclass.h"
#include "statetypes.h"
#include "lighttypes.h"
#include "shadingtypes.h"
#include "globals.h"
#include "trianglefunctions.h"

#include <ctype.h>
#include <list>
#include <algorithm>

using namespace MetaPDF;


#define SPLICEDEBUG( code ) // code

void
assertNotOverlapping( const std::list< Computation::ZBufTriangle > & disjointTriangles )
{
  
  typedef typeof disjointTriangles ListType;
  ListType::const_iterator last = disjointTriangles.end( );
  --last;
  for( ListType::const_iterator i = disjointTriangles.begin( ); i != last; ++i )
    {
      if( last->overlaps( *i ) )
	{
	  throw Exceptions::InternalError( "assertNotOverlapping triggered" );
	}
    }
}

void
assertCounterClockwiseConvex( const Lang::ZBuf::PolyIndices & poly, const std::vector< Concrete::Coords2D > & cornerMem )
{
  for( Lang::ZBuf::PolyIndices::const_iterator i0 = poly.begin( ); i0 != poly.end( ); ++i0 )
    {
      Lang::ZBuf::PolyIndices::const_iterator i1 = i0;
      ++i1;
      if( i1 == poly.end( ) )
	{
	  i1 = poly.begin( );
	}
      Lang::ZBuf::PolyIndices::const_iterator i2 = i1;
      ++i2;
      if( i2 == poly.end( ) )
	{
	  i2 = poly.begin( );
	}
      Concrete::UnitFloatPair inHat = cornerMem[ *i0 ].normalizedOrthogonal( cornerMem[ *i1 ] );
      if( Concrete::inner( inHat, cornerMem[ *i2 ] - cornerMem[ *i1 ] ) < - Computation::theTrixelizeSplicingTol )
	{
	  throw Exceptions::InternalError( "assertCounterClockwiseConvex triggered" );
	}
    }
}


Computation::ZBufTriangle::ZMap::ZMap( const Concrete::UnitFloatTriple & normal,
				       Concrete::Length m,
				       Concrete::Length tiebreaker,
				       Concrete::Length eyez )
  : normal_( normal ), k_x_( normal_.x_ ), k_y_( normal_.y_ ), k_z_( normal_.z_ ), m_( m ),
    eyezInv_( 1. / eyez ), tiebreaker_( tiebreaker ), eyez_( eyez )
{ }

Concrete::Length
Computation::ZBufTriangle::ZMap::operator () ( const Concrete::Coords2D & p ) const
{
  Concrete::Length tmp = k_x_ * p.x_ + k_y_ * p.y_;
  return ( m_ - tmp ) / ( k_z_ - eyezInv_ * tmp );
}

void
Computation::ZBufTriangle::ZMap::writeToMatrices( double a[3], Concrete::Length * b ) const
{
  a[0] = normal_.x_;
  a[1] = normal_.y_;
  a[2] = normal_.z_;
  *b = m_;
}

Computation::ZBufTriangle::ZBufTriangle( const Computation::PaintedPolygon3D * painter, const RefCountPtr< const Computation::ZBufTriangle::ZMap > & zMap, const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 )
  : painter_( painter ), zMap_( zMap )
{
  points_.reserve( 3 );
  points_.push_back( p1 );
  points_.push_back( p2 );
  points_.push_back( p3 );
}

Concrete::Length
Computation::ZBufTriangle::zAt( const Concrete::Coords2D & p ) const
{
  return (*zMap_)( p );
}

bool
Computation::ZBufTriangle::isOnTopOfAt( const Computation::ZBufTriangle & other, const Concrete::Coords2D & p ) const
{
  Concrete::Length zThis = zAt( p );
  Concrete::Length zOther = other.zAt( p );
  
  Concrete::Length tThis = zMap_->getTiebreaker( );
  Concrete::Length tOther = other.zMap_->getTiebreaker( );
  if( ( zThis - zOther ).abs( ) < std::max( tThis, tOther ) )
    {
      return zThis + tThis > zOther + tOther;
    }
  
  return zThis > zOther;
}


// This function does the test with the tolerance interpretation such that a true return value
// means that the triangles really overlaps by some positive amount.
bool
Computation::ZBufTriangle::overlaps( const Computation::ZBufTriangle & other ) const
{
  const std::vector< Concrete::Coords2D > & pointSet_a( points_ );
  const std::vector< Concrete::Coords2D > & pointSet_b( other.points_ ); 

  /* Do the circle-circle check
   */
  {
    Concrete::Coords2D ca( 0, 0 );
    for( std::vector< Concrete::Coords2D >::const_iterator i = pointSet_a.begin( );
	 i != pointSet_a.end( );
	 ++i )
      {
	ca = ca + *i;
      }
    ca = ca * (1./3);

    Concrete::Coords2D cb( 0, 0 );
    for( std::vector< Concrete::Coords2D >::const_iterator i = pointSet_b.begin( );
	 i != pointSet_b.end( );
	 ++i )
      {
	cb = cb + *i;
      }
    cb = cb * (1./3);

    Concrete::Length ra( 0 );
    for( std::vector< Concrete::Coords2D >::const_iterator i = pointSet_a.begin( );
	 i != pointSet_a.end( );
	 ++i )
      {
	ra = std::max( ra, hypotPhysical( ca.x_ - i->x_, ca.y_ - i->y_ ) );
      }

    Concrete::Length rb( 0 );
    for( std::vector< Concrete::Coords2D >::const_iterator i = pointSet_b.begin( );
	 i != pointSet_b.end( );
	 ++i )
      {
	rb = std::max( rb, hypotPhysical( cb.x_ - i->x_, cb.y_ - i->y_ ) );
      }
    
    Concrete::Length rSum = ra + rb;
    if( rSum <= Computation::theTrixelizeOverlapTol )
      {
	return false;
      }
    if( ( rSum - Computation::theTrixelizeOverlapTol ) * ( rSum - Computation::theTrixelizeOverlapTol ) < ( ca.x_ - cb.x_ ) * ( ca.x_ - cb.x_ ) + ( ca.y_ - cb.y_ ) * ( ca.y_ - cb.y_ ) )
      {
	return false;
      }
  }

  /* Now to the search for a separating line.
   */
  if( ! oneWayOverlap( pointSet_a, pointSet_b ) )
    {
      return false;
    }
  if( ! oneWayOverlap( pointSet_b, pointSet_a ) )
    {
      return false;
    }
  return true;
}


// To be consistent with the tolerance interpretation in ZBufTriangle::overlaps, which returns true only if the triangles
// overlaps by some positive amount, this function shall rather return false than true.
// This means that we should prefer not to unflag allOutside.
bool
Computation::ZBufTriangle::oneWayOverlap( const std::vector< Concrete::Coords2D > & poly1, const std::vector< Concrete::Coords2D > & poly2 )
{
  // If a bug is fixed here, remember to fix ZBufLine::overlaps too!

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
      for( std::vector< Concrete::Coords2D >::const_iterator p = poly2.begin( ); p != poly2.end( ); ++p )
	{
	  const Concrete::Length dx = p->x_ - p0.x_;
	  const Concrete::Length dy = p->y_ - p0.y_;
	  if( inx * dx + iny * dy > Computation::theTrixelizeOverlapTol )
	    {
	      allOutside = false;
	      break;
	    }
	}
      if( allOutside )
	{
	  return false;
	}
    }

  /* Note that this is only one part of the test, only if both symmetric tests return true do the
   * polygons overlap.
   */
  return true;
}

std::ostream &
Computation::operator << ( std::ostream & os, const Computation::ZBufTriangle & self )
{
  std::vector< Concrete::Coords2D >::const_iterator i = self.points_.begin( );
  os << *i << " -- " ;
  ++i;
  os << *i << " -- " ;
  ++i;
  os << *i ;
  return os;
}


bool
Computation::ZBufTriangle::contains( const Concrete::Coords2D & p ) const
{
  for( std::vector< Concrete::Coords2D >::const_iterator i0 = points_.begin( ); i0 != points_.end( ); ++i0 )
    {
      std::vector< Concrete::Coords2D >::const_iterator i1 = i0;
      ++i1;
      if( i1 == points_.end( ) )
	{
	  i1 = points_.begin( );
	}
      std::vector< Concrete::Coords2D >::const_iterator i2 = i1;
      ++i2;
      if( i2 == points_.end( ) )
	{
	  i2 = points_.begin( );
	}

      const Concrete::Coords2D & p0( *i0 );
      const Concrete::Coords2D & p1( *i1 );

      const double tx = ( p1.x_ - p0.x_ ).offtype< 1, 0 >( );
      const double ty = ( p1.y_ - p0.y_ ).offtype< 1, 0 >( );
      bool counterClockwise; // true when ( p0, p1 ) are ordered counter-clockwise around the interior.
      {
	const Concrete::Length dx = i2->x_ - p0.x_;
	const Concrete::Length dy = i2->y_ - p0.y_;
	counterClockwise = ( ty * dx - tx * dy < Concrete::ZERO_LENGTH );
      }

      {
	const Concrete::Length dx = p.x_ - p0.x_;
	const Concrete::Length dy = p.y_ - p0.y_;
	if( ( ty * dx - tx * dy > Concrete::ZERO_LENGTH ) == counterClockwise )
	  {
	    return false;
	  }
      }
    }

  return true;
}


// The larger <tol>, the more often will this function return true.
bool
Computation::ZBufTriangle::contains( const Concrete::Coords2D & p, Concrete::Length tol ) const
{
  for( std::vector< Concrete::Coords2D >::const_iterator i0 = points_.begin( ); i0 != points_.end( ); ++i0 )
    {
      std::vector< Concrete::Coords2D >::const_iterator i1 = i0;
      ++i1;
      if( i1 == points_.end( ) )
	{
	  i1 = points_.begin( );
	}
      std::vector< Concrete::Coords2D >::const_iterator i2 = i1;
      ++i2;
      if( i2 == points_.end( ) )
	{
	  i2 = points_.begin( );
	}

      const Concrete::Coords2D & p0( *i0 );
      const Concrete::Coords2D & p1( *i1 );

      const Concrete::UnitFloatPair d = p0.normalizedOrthogonal( p1 );
      if( Concrete::inner( d, *i2 - p0 ) < Concrete::ZERO_LENGTH )
	{
	  // d points out
	  if( Concrete::inner( d, p - p0 ) > tol )
	    {
	      return false;
	    }
	}
      else
	{
	  // d points in
	  if( Concrete::inner( d, p - p0 ) < - tol )
	    {
	      return false;
	    }
	}
    }

  return true;
}

namespace MetaPDF
{
  namespace Computation
  {

  class SplicingLine
  {
  public:
    bool isTriangleSide_;
    // The line is defined in several ways.  First, by p0_ -- (p0_+lengt_*d_), where d_ is unit:
    Concrete::Coords2D p0_;
    Concrete::UnitFloatPair d_;
    Concrete::Length length_;

    // The line through p0 -- p1 is given by x: <x,n_> == r_, where n_ is unit.
    Concrete::UnitFloatPair n_;
    Concrete::Length r_;

    std::vector< const Concrete::Coords2D * > intersections_;

    SplicingLine( const Concrete::Coords2D & p0, const Concrete::Coords2D & p1_sub_p0, bool isTriangleSide )
      : isTriangleSide_( isTriangleSide ),
	p0_( p0 ),
	d_( p1_sub_p0.direction( ) ),
	length_( p1_sub_p0.norm( ) ),
	n_( Concrete::UnitFloatPair( -p1_sub_p0.y_.offtype< 1, 0 >( ), p1_sub_p0.x_.offtype< 1, 0 >( ) ) ),  // Note that this will normalize n_.  What if this fails?
	r_( Concrete::inner( n_, p0 ) )
    { }

    Concrete::Coords2D intersection( const Computation::SplicingLine & other ) const
    {
      Concrete::Length x;
      Concrete::Length y;

      const Concrete::UnitFloatPair & n2 = other.n_;
      const Concrete::Length & r2 = other.r_;

      {
	double n_n1 = Concrete::inner( n_, n2 );
	double n_n_ = Concrete::inner( n_, n_ );
	double n2n2 = Concrete::inner( n2, n2 );

	if( n_n1 * n_n1 > ( 1 - 1e-8 ) * ( n_n_ * n2n2 ) ) // This corresponds to an angle of approximately 0.01 degree.
	  {
	    throw "no intersection";
	  }
      }
      
      double invDet = static_cast< double >( 1 ) / ( n_.x_ * n2.y_ - n_.y_ * n2.x_ );
      x = invDet * (   n2.y_ * r_ - n_.y_ * r2 );
      y = invDet * ( - n2.x_ * r_ + n_.x_ * r2 );

      Concrete::Coords2D res( x, y );
      {
	Concrete::Length c1 = Concrete::inner( d_, res - p0_ );
	Concrete::Length c2 = Concrete::inner( other.d_, res - other.p0_ );
	if( ( isTriangleSide_ && ( c1 < -Computation::theTrixelizeSplicingTol || c1 > length_ + Computation::theTrixelizeSplicingTol ) ) &&
	    ( other.isTriangleSide_ && ( c2 < -Computation::theTrixelizeSplicingTol || c2 > other.length_ + Computation::theTrixelizeSplicingTol ) ) )
	  {
	    throw "no intersection";
	  }
      }

      return res;
    }
    size_t nextLine( const Concrete::Coords2D * p, const Concrete::UnitFloatPair & n, const std::vector< Computation::SplicingLine > & lines ) const
    {
      // This algorithm is formulated without its own tolerances.  It relies on the pointers to the intersection points;
      // "nearby points" should be represented by the same pointer.  If two points are represented by different memory
      // locations, they are treated as positively separated in geometry as well.

      SPLICEDEBUG( std::cerr << "nextLine at " << Lang::Coords2D( *p ) << " -> " << "( " << n.x_ << ", " << n.y_ << " )" << " : " );
      // Note that n will be unit, so it can be used to compute lengths.
      const Concrete::Coords2D & my_p = *p;

      // back is parallel to this line, but not in the same halfspace as n.
      // nOut is parallel to n_ but is turned so that it points out from the enclosed area.
      // They will both be used to select among lines that intersect at the same point.
      Concrete::UnitFloatPair back = d_;
      if( Concrete::inner( back, n ) > 0 )
	{
	  back = back.reverse( );
	}
      Concrete::UnitFloatPair nIn( back.y_, -back.x_ );

      size_t res = 0;
      Concrete::Length bestRes( HUGE_VAL );
      const Concrete::Coords2D * bestPoint = 0;
      size_t idx = 0;
      typedef typeof intersections_ ListType;
      for( ListType::const_iterator i = intersections_.begin( ); i != intersections_.end( ); ++i, ++idx )
	{
	  if( *i == 0 || *i == p )
	    {
	      continue;
	    }
	  Concrete::Length tmp = Concrete::inner( n, **i - my_p );
	  if( *i == bestPoint )
	    {
	      // The lines are compared by direction.
	      Concrete::UnitFloatPair dOld = lines[res].d_;
	      if( Concrete::inner( nIn, dOld ) < 0 )
		{
		  dOld = dOld.reverse( );
		}
	      Concrete::UnitFloatPair dNew = lines[idx].d_;
	      if( Concrete::inner( nIn, dNew ) < 0 )
		{
		  dNew = dNew.reverse( );
		}
	      if( Concrete::inner( back, dNew ) > Concrete::inner( back, dOld ) )
		{
		  bestRes = tmp;
		  // bestPoint is already equal to *i
		  SPLICEDEBUG( std::cerr << "{" << res << "->" << idx << "}" );
		  res = idx;
		}
	    }
	  else if( tmp > Concrete::ZERO_LENGTH && tmp < bestRes )
	    {
	      bestRes = tmp;
	      bestPoint = *i;
	      SPLICEDEBUG( std::cerr << "{" << idx << "}" );
	      res = idx;
	    }
	}
      if( bestRes == Concrete::HUGE_LENGTH )
	{
	  SPLICEDEBUG( std::cerr << "none" << std::endl );
	  throw "no intersection";
	}
      SPLICEDEBUG( std::cerr << *intersections_[res] << std::endl );
      return res;
    }
    Concrete::Length distanceTo( const Concrete::Coords2D p ) const
    {
      return ( Concrete::inner( n_, p ) - r_ ).abs( );
    }
  };

  }
}

void
Computation::ZBufTriangle::pushLines( std::vector< Computation::SplicingLine > * dst ) const
{
  for( std::vector< Concrete::Coords2D >::const_iterator i0 = points_.begin( ); i0 != points_.end( ); ++i0 )
    {
      std::vector< Concrete::Coords2D >::const_iterator i1 = i0;
      ++i1;
      if( i1 == points_.end( ) )
	{
	  i1 = points_.begin( );
	}
      pushIfUnique( dst, *i0, *i1 );
    }
}

void
Computation::ZBufTriangle::pushIntersection( std::vector< Computation::SplicingLine > * dst, const Computation::ZBufTriangle & other ) const
{
  double a0[3];
  Concrete::Length b0;
  zMap_->writeToMatrices( a0, & b0 );

  double a1[3];
  Concrete::Length b1;
  other.zMap_->writeToMatrices( a1, & b1 );

  Concrete::Coords3D ray( a0[1]*a1[2] - a0[2]*a1[1],
			  a0[2]*a1[0] - a0[0]*a1[2],
			  a0[0]*a1[1] - a0[1]*a1[0] );

  // If the ray is zero, the planes are paralell, so there's no splicing line to add.
  if( ray.normScalar( ) < 1e-5 )
    {
      return;
    }

  size_t bestCol;
  {
    double * src = a0;
    double bestAbs = 0;
    for( size_t col_i = 0; col_i < 3; ++col_i, ++src )
      {
	double tmp = fabs( *src );
	if( tmp > bestAbs )
	  {
	    bestAbs = tmp;
	    bestCol = col_i;
	  }
    }
  }
  // Here, one could check that bestCol has really been assigned.
  
  {
    const double f = a1[bestCol] / a0[bestCol];
    double * src = a0;
    double * dst = a1;
    for( ; src != a0 + 3; ++src, ++dst )
      {
	*dst -= f * *src;
      }
    b1 -= f * b0;
  }
  
  // The remaining two columns of the second row can now be used to determine the other two components.
  // Let secondCol point out the biggest remaining element in a1, and zeroCol point out the column containing the smallest
  // (non-zero) element.
  // First, we guess...
  size_t secondCol;
  size_t zeroCol;
  switch( bestCol )
    {
    case 0:
      secondCol = 1;
      zeroCol = 2;
      break;
    case 1:
      secondCol = 0;
      zeroCol = 2;
      break;
    case 2:
      secondCol = 0;
      zeroCol = 1;
      break;
    default:
      throw Exceptions::InternalError( "pushIntersection: Column switch out of range." );
    }
  // ... then correct the guess if it's wrong
  if( fabs( a1[secondCol] ) < fabs( a1[zeroCol] ) )
    {
      size_t tmp = zeroCol;
      zeroCol = secondCol;
      secondCol = tmp;
    }

  Concrete::Length res[3];
  // Now it's time to determine the first two components of the solution.
  res[zeroCol] = Concrete::ZERO_LENGTH;
  res[secondCol] = b1 / a1[secondCol];

  // and then the final component follows by back-substitution:
  b0 -= a0[secondCol] * res[secondCol];
  res[bestCol] = b0 / a0[bestCol];
  
  Concrete::Coords3D p0( res[0], res[1], res[2] );
  Concrete::Coords3D p1( p0 + Concrete::SOME_LENGTH * ray.direction( ) );
  
  pushIfUnique( dst, p0, p1, false );
}

void
Computation::ZBufTriangle::pushIfUnique( std::vector< Computation::SplicingLine > * dst, const Concrete::Coords3D p03D, const Concrete::Coords3D p13D, bool isTriangleSide ) const
{
  const Concrete::Coords2D p0 = p03D.make2DAutomatic( zMap_->eyez( ) );
  const Concrete::Coords2D p1 = p13D.make2DAutomatic( zMap_->eyez( ) );
  typedef typeof *dst ListType;
  for( ListType::const_iterator i = dst->begin( ); i != dst->end( ); ++i )
    {
      if( i->distanceTo( p0 ) < Computation::theTrixelizeSplicingTol && i->distanceTo( p1 ) < Computation::theTrixelizeSplicingTol )
	{
	  return;
	}
   }
  dst->push_back( Computation::SplicingLine( p0, p1 - p0, isTriangleSide ) );
}

namespace MetaPDF
{
  typedef std::pair< const Concrete::Coords2D *, const Concrete::Coords2D * > VisitType;
}
namespace std
{
  template< >
  class less< MetaPDF::VisitType >
  {
  public:
    bool operator () ( const MetaPDF::VisitType & p1,
		       const MetaPDF::VisitType & p2 )
    {
      if( p1.first < p2.first )
	{
	  return true;
	}
      if( p2.first < p1.first )
	{
	  return false;
	}
      return p1.second < p2.second;
    }
  };
}

void
Computation::ZBufTriangle::splice( const ZBufTriangle & tOld, const ZBufTriangle & tNew, std::list< Computation::ZBufTriangle > * oldDisjointTriangles, std::list< Computation::ZBufTriangle > * oldOccludedTriangles, std::list< Computation::ZBufTriangle > * newDisjointTriangles, std::list< Computation::ZBufTriangle > * newOccludedTriangles, std::list< Computation::ZBufTriangle > * triangleQueue )
{
  SPLICEDEBUG( std::cerr << "==== Overlapping triangles ====" << std::endl
	       << "  old: " << tOld << std::endl
	       << "  new: " << tNew << std::endl );

  // This function is only called if the triangles overlap

  std::vector< Computation::SplicingLine > lines;

  tOld.pushLines( & lines );
  tNew.pushLines( & lines );

  tOld.pushIntersection( & lines, tNew );

  const size_t numberOfLines = lines.size( );

  SPLICEDEBUG( std::cerr << "#needs circle" << std::endl
	       << "#needs centering" << std::endl
	       << std::endl
	       << "@width:0bp" << std::endl
	       << "|" << std::endl
	       << "{" << std::endl
	       << "res: Hot2D <<" << std::endl );

  {
    typedef typeof lines ListType;
    for( ListType::iterator i = lines.begin( ); i != lines.end( ); ++i )
      {
	SPLICEDEBUG( std::cerr << "|** Line " << ( i - lines.begin( ) ) << ": " );
	//	SPLICEDEBUG( std::cerr << i->p0_ << " -- " << i->p0_ + i->d_ * i->length_ << std::endl );
	SPLICEDEBUG( std::cerr << std::endl
		     << "res << stroke [] "
		     << " (" << Helpers::droolFormat( i->p0_.x_ ) << "," << Helpers::droolFormat( i->p0_.y_ )
		     << ")--(" << Helpers::droolFormat( (i->p0_ + i->d_ * i->length_).x_ ) << "," << Helpers::droolFormat( (i->p0_ + i->d_ * i->length_).y_ ) << ")"
		     << std::endl );
	i->intersections_.resize( numberOfLines, 0 );
      }
  }


  std::vector< Concrete::Coords2D > intersectionsMem;
  intersectionsMem.reserve( numberOfLines * ( numberOfLines - 1 ) / 2 );

  {
    typedef typeof lines ListType;
    size_t idx_i = 0;
    for( ListType::iterator i = lines.begin( ); i != lines.end( ); ++i, ++idx_i )
      {
	SPLICEDEBUG( std::cerr << "|** Line " << idx_i << " intersections: " );
	SPLICEDEBUG( std::cerr << std::endl );
	ListType::const_iterator j = i;
	size_t idx_j = idx_i;
	++j;
	++idx_j;
	for( ; j != lines.end( ); ++j, ++idx_j )
	  {
	    try
	      {
		Concrete::Coords2D tmp = i->intersection( *j );
		typedef typeof intersectionsMem VectorType;
		bool reuse = false;
		// We try to reuse an old point.  Not to save space, but to make the algorithm more robust by
		// perhaps avoiding some corner cases this way...
		for( VectorType::const_iterator k = intersectionsMem.begin( ); k != intersectionsMem.end( ); ++k )
		  {
		    if( ( tmp - *k ).norm( ) < Computation::theTrixelizeSplicingTol )
		      {
			//			SPLICEDEBUG( std::cerr << "<" << tmp << ">  " );
			SPLICEDEBUG( std::cerr << "|** reusing <" << tmp << ">  " << std::endl );
			reuse = true;
			lines[ idx_i ].intersections_[ idx_j ] = & *k;
			lines[ idx_j ].intersections_[ idx_i ] = & *k;
			break;
		      }
		  }
		if( ! reuse )
		  {
		    //		    SPLICEDEBUG( std::cerr << tmp << "  " );
		    SPLICEDEBUG( std::cerr << "res << [shift (" << Helpers::droolFormat( tmp.x_ ) << "," << Helpers::droolFormat( tmp.y_ ) << ")] [] stroke [] [circle 0.1mm]" << std::endl );
		    intersectionsMem.push_back( tmp );
		    lines[ idx_i ].intersections_[ idx_j ] = & intersectionsMem.back( );
		    lines[ idx_j ].intersections_[ idx_i ] = & intersectionsMem.back( );
		  }
	      }
	    catch( const char * )
	      {
		//		SPLICEDEBUG( std::cerr << "(no-int)" << "  " );
		SPLICEDEBUG( std::cerr << "|** (no-int)" << std::endl );
	      }
	  }
	SPLICEDEBUG( std::cerr << std::endl );
      }
  }

  SPLICEDEBUG( std::cerr << "res;" << std::endl
	       << "bb: [bbox res]" << std::endl
	       << "@<< [scale 19cm/[max [xmax bb]-[xmin bb] [ymax bb]-[ymin bb]]] [] res" << std::endl
	       << "}" << std::endl );
  
  std::set< VisitType > visited;
  
  for( size_t start_a = 0; start_a < numberOfLines; ++start_a )
    {
      if( ! lines[ start_a ].isTriangleSide_ )
	{
	  continue;
	}
      for( size_t start_b = 0; start_b < numberOfLines; ++start_b )
	{
	  if( start_b == start_a )
	    {
	      continue;
	    }
	  SPLICEDEBUG( std::cerr << "From line " << start_a << " to line " << start_b << std::endl );
	  const Concrete::Coords2D * p0 = lines[ start_a ].intersections_[ start_b ];
	  if( p0 == 0 )
	    {
	      SPLICEDEBUG( std::cerr << "  No intersection" << std::endl );
	      continue;
	    }
	  Concrete::UnitFloatPair start_n = lines[ start_a ].n_;
	  for( char start_n_i = 0; start_n_i < 2; ++start_n_i, start_n = start_n.reverse( ) )
	    {
	      SPLICEDEBUG( std::cerr << "  Starting direction " << static_cast< int >( start_n_i ) << std::endl );
	      Concrete::UnitFloatPair n = start_n;
	      size_t currentLine = start_b;
	      size_t nextLine;
	      try
		{
		  nextLine = lines[ currentLine ].nextLine( p0, n, lines );
		  SPLICEDEBUG( std::cerr << "Line " << currentLine << " --> " << nextLine << std::endl );
		}
	      catch( const char * ball )
		{
		  continue;
		}
	      const Concrete::Coords2D * p1 = lines[ currentLine ].intersections_[ nextLine ];
	      if( visited.find( VisitType( p0, p1 ) ) != visited.end( ) )
		{
		  SPLICEDEBUG( std::cerr << *p0 << " --> " << *p1 << " already visited" << std::endl );
		  continue;
		}
	      visited.insert( VisitType( p0, p1 ) );

	      // visitedLines is used to tell when we have completed a polygon; when a line is visited a second time,
	      // the polygon is complete.  The first point is on start_b.  start_a is only used to give a direction along
	      // start_b.
	      std::vector< bool > visitedLines;
	      visitedLines.resize( numberOfLines, false );
	      visitedLines[ currentLine ] = true;

	      {
		Concrete::UnitFloatPair oldn = n;
		n = lines[ currentLine ].n_;
		if(  - oldn.y_ * n.x_ + oldn.x_ * n.y_ < 0 )
		  {
		    n = n.reverse( );
		  }
	      }

	      const Concrete::Coords2D * p2 = 0;

	      currentLine = nextLine;
	      try
		{
		  nextLine = lines[ currentLine ].nextLine( p1, n, lines );
		  SPLICEDEBUG( std::cerr << "Line " << currentLine << " --> " << nextLine << std::endl );
		}
	      catch( const char * ball )
		{
		  SPLICEDEBUG( std::cerr << "Line " << currentLine << " --> " << "infinity! (no enclosed region)" << std::endl );
		  continue;
		}
	      p2 = lines[ currentLine ].intersections_[ nextLine ];
	      visited.insert( VisitType( p1, p2 ) );
	      visitedLines[ currentLine ] = true;
		
	      // Here, we choose a way to compute a point well inside the *p0--*p1--*p2 triangle.
	      // Using the mean is quick, but the incenter should be more robust.
	      Concrete::Coords2D mean = (1./3) * ( *p0 + *p1 + *p2 );
	      //	      Concrete::Coords2D mean = Computation::triangleIncenter( *p0, *p1, *p2 );

	      // The following test never triggers, which is good.
// 	      if( tOld.contains( mean ) &&
// 		  ( ( ! tOld.contains( *p0, Computation::theTrixelizeSplicingTol ) ) ||
// 		    ( ! tOld.contains( *p1, Computation::theTrixelizeSplicingTol ) ) ||
// 		    ( ! tOld.contains( *p2, Computation::theTrixelizeSplicingTol ) ) ) )
// 		{
// 		  std::cerr << "tOld contains mean but not all corners!" << std::endl ;
// 		}
// 	      if( tNew.contains( mean ) &&
// 		  ( ( ! tNew.contains( *p0, Computation::theTrixelizeSplicingTol ) ) ||
// 		    ( ! tNew.contains( *p1, Computation::theTrixelizeSplicingTol ) ) ||
// 		    ( ! tNew.contains( *p2, Computation::theTrixelizeSplicingTol ) ) ) )
// 		{
// 		  std::cerr << "tNew contains mean but not all corners!" << std::endl ;
// 		}
		    


	      const Computation::ZBufTriangle * topTriangle = 0;
	      bool reQueue = true;
	      std::list< Computation::ZBufTriangle > * disjointDestination = 0; // this must be assigned when ( ! reQueue )
	      if( tOld.contains( mean ) )
		{
		  reQueue = false;
		  if( tNew.contains( mean ) )
		    {
		      if( tOld.isOnTopOfAt( tNew, mean ) )
			{
			  topTriangle = & tOld;
			  disjointDestination = oldDisjointTriangles;
			  SPLICEDEBUG( std::cerr << "Part of tOld" << std::endl );
			}
		      else
			{
			  topTriangle = & tNew;
			  disjointDestination = newDisjointTriangles;
			  SPLICEDEBUG( std::cerr << "Part of tNew" << std::endl );
			}
		    }
		  else
		    {
		      topTriangle = & tOld;
		      disjointDestination = oldDisjointTriangles;
		      SPLICEDEBUG( std::cerr << "Part of tOld" << std::endl );
		    }
		}
	      else
		{
		  if( tNew.contains( mean ) )
		    {
		      topTriangle = & tNew;
		      SPLICEDEBUG( std::cerr << "Part of tNew" << std::endl );
		    }
		  else
		    {
		      SPLICEDEBUG( std::cerr << "Outside painted area" << std::endl );
		      continue;
		    }
		}

	      size_t undoSize;
	      if( reQueue )
		{
		  undoSize = triangleQueue->size( );
		}
	      else
		{
		  undoSize = disjointDestination->size( );
		}

	      // This tolerance test assures that we don't produce tiny-tiny triangles.
	      if( Computation::triangleArea( *p0, *p1, *p2 ) > Computation::theTrixelizeOverlapTol * Computation::triangleSemiPerimeter( *p0, *p1, *p2 ) )
		{
		  if( reQueue )
		    {
		      triangleQueue->push_back( Computation::ZBufTriangle( topTriangle->painter_,
									   topTriangle->zMap_,
									   *p0, *p1, *p2) );
		    }
		  else
		    {
		      disjointDestination->push_back( Computation::ZBufTriangle( topTriangle->painter_,
										 topTriangle->zMap_,
										 *p0, *p1, *p2) );
		    }
		}
	      p1 = p2;
	      
	      {
		Concrete::UnitFloatPair oldn = n;
		n = lines[ currentLine ].n_;
		if(  - oldn.y_ * n.x_ + oldn.x_ * n.y_ < 0 )
		  {
		    n = n.reverse( );
		  }
	      }
	      
	      bool insertTriangles = true;

	      while( ! visitedLines[ nextLine ] )
		{
		  currentLine = nextLine;
		  try
		    {
		      nextLine = lines[ currentLine ].nextLine( p1, n, lines );
		      SPLICEDEBUG( std::cerr << "Line " << currentLine << " --> " << nextLine << std::endl );
		    }
		  catch( const char * ball )
		    {
		      SPLICEDEBUG( std::cerr << "Line " << currentLine << " --> " << "infinity! (no enclosed region)" << std::endl );
		      insertTriangles = false;
		      break;
		    }
		  p2 = lines[ currentLine ].intersections_[ nextLine ];
		  visited.insert( VisitType( p1, p2 ) );
		  visitedLines[ currentLine ] = true;
		  
		  if( Computation::triangleArea( *p0, *p1, *p2 ) > Computation::theTrixelizeOverlapTol * Computation::triangleSemiPerimeter( *p0, *p1, *p2 ) )
		    {
		      if( reQueue )
			{
			  triangleQueue->push_back( Computation::ZBufTriangle( topTriangle->painter_,
									       topTriangle->zMap_,
									       *p0, *p1, *p2) );
			}
		      else
			{
			  disjointDestination->push_back( Computation::ZBufTriangle( topTriangle->painter_,
										     topTriangle->zMap_,
										     *p0, *p1, *p2) );
			}
		    }
		  p1 = p2;
		  
		  {
		    Concrete::UnitFloatPair oldn = n;
		    n = lines[ currentLine ].n_;
		    if(  - oldn.y_ * n.x_ + oldn.x_ * n.y_ < 0 )
		      {
			n = n.reverse( );
		      }
		  }
		}
	      if( insertTriangles )
		{
		  visited.insert( VisitType( p2, p0 ) );
		  SPLICEDEBUG( std::cerr << "Found new region." << std::endl );
		}
	      else
		{
		  if( reQueue )
		    {
		      while( triangleQueue->size( ) > undoSize )
			{
			  triangleQueue->pop_back( );
			}
		    }
		  else
		    {
		      while( disjointDestination->size( ) > undoSize )
			{
			  disjointDestination->pop_back( );
			}
		    }
		}
	    }
	}
    }
}

RefCountPtr< const Lang::ElementaryPath2D >
Computation::ZBufTriangle::toPath( ) const
{
  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;

  for( std::vector< Concrete::Coords2D >::const_iterator i = points_.begin( ); i != points_.end( ); ++i )
    {
      res->push_back( new Concrete::PathPoint2D( i->x_, i->y_ ) );
    }
  res->close( );

  return RefCountPtr< const Lang::ElementaryPath2D >( res );
}

RefCountPtr< const Lang::Drawable2D >
Computation::ZBufTriangle::debugFrame( ) const
{
  const Concrete::Coords2D & p1( points_[ 0 ] );
  const Concrete::Coords2D & p2( points_[ 1 ] );
  const Concrete::Coords2D & p3( points_[ 2 ] );

  SPLICEDEBUG( std::cerr << "Debug frame: " << p1 << " -- " << p2 << " -- " << p3 << std::endl ; )

  Kernel::GraphicsState * discStatePtr = new Kernel::GraphicsState( true );
  discStatePtr->cap_ = Lang::CapStyle::CAP_ROUND;
  discStatePtr->width_ = 2 * Computation::triangleArea( p1, p2, p3 ) / Computation::triangleSemiPerimeter( p1, p2, p3 );
  discStatePtr->strokingColor_ = painter_->getColor( );
  RefCountPtr< const Kernel::GraphicsState > discState( discStatePtr );

  Kernel::GraphicsState * frameStatePtr = new Kernel::GraphicsState( true );
  frameStatePtr->width_ = Concrete::Length( 0.2 );
  RefCountPtr< const Kernel::GraphicsState > frameState( frameStatePtr );

  Lang::ElementaryPath2D * pointPath = new Lang::ElementaryPath2D;
  const Concrete::Coords2D incenter = Computation::triangleIncenter( p1, p2, p3 );
  pointPath->push_back( new Concrete::PathPoint2D( incenter.x_, incenter.y_ ) );
  pointPath->push_back( new Concrete::PathPoint2D( incenter.x_, incenter.y_ ) );

  SPLICEDEBUG( std::cerr << "  incenter: " << incenter << std::endl );
  SPLICEDEBUG( std::cerr << "  radius: " <<  Lang::Length( discState->width_ ) << std::endl );

  return Helpers::newSolidTransparencyGroup( RefCountPtr< const Lang::Drawable2D >
					     ( new Lang::PaintedPath2D( frameState, toPath( ), "S" ) ),
					     RefCountPtr< const Lang::Drawable2D >
					     ( new Lang::PaintedPath2D( discState, RefCountPtr< const Lang::ElementaryPath2D >( pointPath ), "S" ) ) );
}
