#include <cmath>

#include "trianglefunctions.h"


using namespace Shapes;


Concrete::Coords2D
Computation::triangleIncenter( const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 )
{
	const Concrete::Coords2D d12 = p2 - p1;
	const Concrete::Coords2D d13 = p3 - p1;
	const Concrete::Coords2D d23 = p3 - p2;

	Concrete::Length n12 = d12.norm( );
	Concrete::Length n13 = d13.norm( );
	Concrete::Length n23 = d23.norm( );

	// All initializations must come before the goto jumps, or we would have to use one scope delimiter per norm.
	// However, I don't just because that would make Emacs indent in an ugly way.

	const Concrete::Length TINY_LENGTH( 1e-8 );

	if( n12 < TINY_LENGTH )
		{
			goto returnmean;
		}
	if( n13 < TINY_LENGTH )
		{
			goto returnmean;
		}
	if( n23 < TINY_LENGTH )
		{
			goto returnmean;
		}

	{

		const Concrete::UnitFloatPair r12 = d12.direction( n12 );
		const Concrete::UnitFloatPair r13 = d13.direction( n13 );
		const Concrete::UnitFloatPair r23 = d23.direction( n23 );

		// Now we cheat, because we will not make the normals unit!
		const Concrete::UnitFloatPair a1( r12.x_ - r13.x_, r12.y_ - r13.y_, bool( ) );	// normal to the angle bisector line through p1
		const Concrete::UnitFloatPair a2( r12.x_ + r23.x_, r12.y_ + r23.y_, bool( ) );	// normal to the angle bisector line through p2

		Concrete::Length b1 = Concrete::inner( a1, p1 );	// constant of the angle bisector line through p1
		Concrete::Length b2 = Concrete::inner( a2, p2 );	// constant of the angle bisector line through p2

		// Then we compute the intersection of those lines.
		double det = ( a1.x_ * a2.y_ - a1.y_ * a2.x_ );
		if( fabs( det ) < 1e-8 )
			{
				goto returnmean;
			}
		double invDet = 1. / det;
		Concrete::Length x = invDet * (	 a2.y_ * b1 - a1.y_ * b2 );
		Concrete::Length y = invDet * ( - a2.x_ * b1 + a1.x_ * b2 );

		return Concrete::Coords2D( x, y );
	}
 returnmean:
	return (1/3) * ( p1 + p2 + p3 );
}

Concrete::Coords3D
Computation::triangleIncenter( const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3 )
{
	const Concrete::Coords3D d12 = p2 - p1;
	const Concrete::Coords3D d13 = p3 - p1;
	const Concrete::Coords3D d23 = p3 - p2;

	Concrete::Length n12 = d12.norm( );
	Concrete::Length n13 = d13.norm( );
	Concrete::Length n23 = d23.norm( );

	// All initializations must come before the goto jumps, or we would have to use one scope delimiter per norm.
	// However, I don't just because that would make Emacs indent in an ugly way.

	const Concrete::Length TINY_LENGTH( 1e-8 );

	if( n12 < TINY_LENGTH )
		{
			goto returnmean;
		}
	if( n13 < TINY_LENGTH )
		{
			goto returnmean;
		}
	if( n23 < TINY_LENGTH )
		{
			goto returnmean;
		}

	{

		const Concrete::UnitFloatTriple r12 = d12.direction( n12 );
		const Concrete::UnitFloatTriple r13 = d13.direction( n13 );
		const Concrete::UnitFloatTriple r23 = d23.direction( n23 );

		// Now we cheat, because we will not make the directions unit!
		// Note that we don't use the same kind of the equations in 3D as we did in 2D!
		const Concrete::UnitFloatTriple a1( r12.x_ + r13.x_, r12.y_ + r13.y_, r12.z_ + r13.z_, bool( ) );	// direction of the angle bisector line through p1
		const Concrete::UnitFloatTriple a2( r23.x_ - r12.x_, r23.y_ - r12.y_, r23.z_ - r12.z_, bool( ) );	// direction of the angle bisector line through p2

		// We will now solve for the scalars k1 and k2 such that
		//	 p1 + k1 a1 == p2 + k2 a2
		// that is, we solve the following system in least-squares sense (this gives robustness)
		//	 ( a1 -a2 ) ( k1 k2 ) == p2 - p1
		// To do this, we form the normal equations:
		//	 A ( k1 k2 ) == B
		// Where
		//	 A = Transpose( a1 -a2 ) ( a1 -a2 )
		//	 B = Transpose( a1 -a2 ) ( p2 - p1 ) = Transpose( a1 -a2 ) d12

		double a11 = Concrete::inner( a1, a1 );
		double a12 = - Concrete::inner( a1, a2 ); // this is also a21
		double a22 = Concrete::inner( a2, a2 );

		Concrete::Length b1 = Concrete::inner( a1, d12 );
		Concrete::Length b2 = - Concrete::inner( a2, d12 );

		// Then we compute the intersection of those lines.
		double det = ( a11 * a22 - a12 * a12 );
		if( fabs( det ) < 1e-8 )
			{
				goto returnmean;
			}
		double invDet = 1. / det;
		Concrete::Length k1 = invDet * (	 a22 * b1 - a12 * b2 );

		return p1 + k1 * a1;
	}
 returnmean:
	return (1/3) * ( p1 + p2 + p3 );
}

Concrete::Area
Computation::triangleArea( const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 )
{
	const Concrete::Coords2D d1 = p2 - p1;
	const Concrete::Coords2D d2 = p3 - p1;
	return 0.5 * ( d1.x_ * d2.y_ - d1.y_ * d2.x_ ).abs( );
}

Concrete::Length
Computation::triangleSemiPerimeter( const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 )
{
	return 0.5 * ( ( p2 - p1 ).norm( ) + ( p3 - p2 ).norm( ) + ( p1 - p3 ).norm( ) );
}

Concrete::Area
Computation::triangleArea( const Concrete::Coords3D & p1, const Concrete::Coords3D & p2, const Concrete::Coords3D & p3 )
{
	return 0.5 * Concrete::crossMagnitude( p2 - p1, p3 - p1 );
}
