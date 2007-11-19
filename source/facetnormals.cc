#include "drawabletypes.h"
#include "lighttypes.h"
#include "astclass.h"
#include "constructorrepresentation.h"
#include "globals.h"

using namespace Shapes;


Lang::FacetNormalGray::FacetNormalGray( const Concrete::Coords3D & position,
																				const RefCountPtr< const Lang::SpecularReflection > & reflections,
																				const Concrete::UnitFloatTriple & reflectionUnitNormal,
																				const Concrete::Gray lightMultiply,
																				const RefCountPtr< const Lang::SpecularReflection > & autoScattering,
																				const Concrete::Gray autoIntensity )
	: position_( position ),
		reflections_( reflections ),
		reflectionUnitNormal_( reflectionUnitNormal ),
		lightMultiply_( lightMultiply ),
		autoScattering_( autoScattering ),
		autoIntensity_( autoIntensity )
{ }

Lang::FacetNormalGray::~FacetNormalGray( )
{ }

RefCountPtr< const Lang::Class > Lang::FacetNormalGray::TypeID( new Lang::SystemFinalClass( strrefdup( "FacetNormalGray" ) ) );
TYPEINFOIMPL( FacetNormalGray );

Concrete::Gray
Lang::FacetNormalGray::compute( const Concrete::Coords3D & point, const Concrete::UnitFloatTriple normal, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	// Here, we must remember that the lights, but only the lights, are already transformed!
	
	Concrete::UnitFloatTriple tf_normal = tf.transformPlaneUnitNormal( reflectionUnitNormal_ );
	//	Concrete::Coords3D tf_position( position_.transformed( tf ) );
	double grayVal = autoScattering_->illuminate( point, eyez, tf_normal, autoIntensity_ ).gr_;

	typedef typeof lights ListType;
	for( ListType::const_iterator i = lights.begin( ); i != lights.end( ); ++i )
		{
			double tmp = 0;
			{
				typedef const Lang::GrayLight LightType;
				LightType * light = dynamic_cast< LightType * >( i->getPtr( ) );
				if( light != 0 )
					{
						tmp = reflections_->illuminate( point, eyez, tf_normal, *light ).gr_;
						goto foundLightType;
					}
			}
			{
				typedef const Lang::RGBLight LightType;
				LightType * light = dynamic_cast< LightType * >( i->getPtr( ) );
				if( light != 0 )
					{
						tmp = reflections_->illuminate( point, eyez, tf_normal, *light ).mean( );
						goto foundLightType;
					}
			}
			throw Exceptions::InternalError( "A strange type of light." );
		foundLightType:
			tmp *= lightMultiply_.gr_;
			grayVal += tmp;
			if( grayVal > 1 )
				{
					grayVal = 1;
					break;
				}
		}
	
	return Concrete::Gray( grayVal );
}

Concrete::Gray
Lang::FacetNormalGray::compute( const Concrete::Coords3D & point, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	return compute( point, reflectionUnitNormal_, tf, eyez, lights );
}

Concrete::Gray
Lang::FacetNormalGray::getDebugColor( ) const
{
	return lightMultiply_;
}

RefCountPtr< const Lang::FacetNormalGray >
Lang::FacetNormalGray::transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::FacetNormalGray >
		( new Lang::FacetNormalGray( position_.transformed( tf ),
																 reflections_,
																 tf.transformPlaneUnitNormal( reflectionUnitNormal_ ),
																 lightMultiply_,
																 autoScattering_,
																 autoIntensity_ ) );
}


void
Lang::FacetNormalGray::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::SpecularReflection * >( reflections_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::SpecularReflection * >( autoScattering_.getPtr( ) )->gcMark( marked );
}


Lang::FacetNormalRGB::FacetNormalRGB( const Concrete::Coords3D & position,
																			const RefCountPtr< const Lang::SpecularReflection > & reflections,
																			const Concrete::UnitFloatTriple & reflectionUnitNormal,
																			const RefCountPtr< const Lang::RGB > & lightMultiply,
																			const RefCountPtr< const Lang::SpecularReflection > & autoScattering,
																			const RefCountPtr< const Lang::RGB > & autoIntensity )
	: position_( position ),
		reflections_( reflections ),
		reflectionUnitNormal_( reflectionUnitNormal ),
		lightMultiply_( lightMultiply ),
		autoScattering_( autoScattering ),
		autoIntensity_( autoIntensity )
{ }

Lang::FacetNormalRGB::~FacetNormalRGB( )
{ }

RefCountPtr< const Lang::Class > Lang::FacetNormalRGB::TypeID( new Lang::SystemFinalClass( strrefdup( "FacetNormalRGB" ) ) );
TYPEINFOIMPL( FacetNormalRGB );

Concrete::RGB
Lang::FacetNormalRGB::compute( const Concrete::Coords3D & point, const Concrete::UnitFloatTriple normal, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	throw Exceptions::NotImplemented( "FacetNormalRGB::compute" );
}

Concrete::RGB
Lang::FacetNormalRGB::compute( const Concrete::Coords3D & point, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const
{
	return compute( point, reflectionUnitNormal_, tf, eyez, lights );
}

Concrete::RGB
Lang::FacetNormalRGB::getDebugColor( ) const
{
	return lightMultiply_->components( );
}

RefCountPtr< const Lang::FacetNormalRGB >
Lang::FacetNormalRGB::transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::FacetNormalRGB >
		( new Lang::FacetNormalRGB( position_.transformed( tf ),
																reflections_,
																tf.transformPlaneUnitNormal( reflectionUnitNormal_ ),
																lightMultiply_,
																autoScattering_,
																autoIntensity_ ) );
}

void
Lang::FacetNormalRGB::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::SpecularReflection * >( reflections_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::SpecularReflection * >( autoScattering_.getPtr( ) )->gcMark( marked );
}


Computation::FacetInterpolatorGray::~FacetInterpolatorGray( )
{ }


Computation::FacetInterpolatorGray1::~FacetInterpolatorGray1( )
{ }

RefCountPtr< const Lang::Gray >
Computation::FacetInterpolatorGray1::compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const
{
	return RefCountPtr< const Lang::Gray >( new Lang::Gray( n1_->compute( point, tf, eyez, lights ) ) );
}

RefCountPtr< const Lang::Gray >
Computation::FacetInterpolatorGray1::getDebugColor( ) const
{
	return RefCountPtr< const Lang::Gray >( new Lang::Gray( n1_->getDebugColor( ) ) );
}

RefCountPtr< const Computation::FacetInterpolatorGray >
Computation::FacetInterpolatorGray1::transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Computation::FacetInterpolatorGray >
		( new FacetInterpolatorGray1( n1_->transformed( tf ) ) );
}


void
Computation::FacetInterpolatorGray1::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::FacetNormalGray * >( n1_.getPtr( ) )->gcMark( marked );
}


Computation::FacetInterpolatorGray2::FacetInterpolatorGray2( const RefCountPtr< const Lang::FacetNormalGray > & n1,
																														 const RefCountPtr< const Lang::FacetNormalGray > & n2 )
	: n1_( n1 ), n2_( n2 ),
		d_( 0 ), t_( 0, 0, 0, bool( ) ), m_( 0 ),
		rotationDirection_( 0, 0, 0, bool( ) )
{
	Concrete::Coords3D r = n2_->position( ) - n1_->position( );
	d_ = r.norm( );
	t_ = r.direction( d_ );
	m_ = Concrete::inner( t_, n1_->position( ) );
	
	angle_ = acos( Concrete::inner( n1_->normal( ), n2_->normal( ) ) );
	try
		{
			rotationDirection_ = Concrete::crossDirection( n1_->normal( ), n2_->normal( ) );
		}
	catch( const NonLocalExit::CrossDirectionOfParallel & ball )
		{
			if( angle_ > 3 )
				{
					throw Exceptions::MiscellaneousRequirement( "The facet normals on a facet must not point in opposite directions." );
				}
			rotationDirection_ = Concrete::UnitFloatTriple( 1, 0, 0, bool( ) );
		}
}

Computation::FacetInterpolatorGray2::~FacetInterpolatorGray2( )
{ }

RefCountPtr< const Lang::Gray >
Computation::FacetInterpolatorGray2::compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez )
 const
{
	double w2 = ( Concrete::inner( t_, point ) - m_ ) / d_;
	double w1 = 1 - w2;

	// The following may not be efficient, but it was easy to code by copying code from Core_rotate3D.
	// I've got the feeling that this is much better done using geometric algebra based on Clifford algebra.

	Concrete::UnitFloatTriple normal = n1_->normal( ).rotate( rotationDirection_, w2 * angle_ );

	Concrete::Gray c1 = n1_->compute( point, normal, tf, eyez, lights ).mulNoCheck( w1 );
	Concrete::Gray c2 = n2_->compute( point, normal, tf, eyez, lights ).mulNoCheck( w2 );
	Concrete::Gray res = c1.addNoCheck( c2 );
	if( res.gr_ < 0 )
		{
			res.gr_ = 0;
		}
	else if( res.gr_ > 1 )
		{
			res.gr_ = 1;
		}
	return RefCountPtr< const Lang::Gray >( new Lang::Gray( res ) );
}

RefCountPtr< const Lang::Gray >
Computation::FacetInterpolatorGray2::getDebugColor( ) const
{
	return RefCountPtr< const Lang::Gray >( new Lang::Gray( n1_->getDebugColor( ) ) );
}

RefCountPtr< const Computation::FacetInterpolatorGray >
Computation::FacetInterpolatorGray2::transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Computation::FacetInterpolatorGray >
		( new FacetInterpolatorGray2( n1_->transformed( tf ),
																	n2_->transformed( tf ) ) );
}

void
Computation::FacetInterpolatorGray2::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::FacetNormalGray * >( n1_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::FacetNormalGray * >( n2_.getPtr( ) )->gcMark( marked );
}


Computation::FacetInterpolatorGray3::FacetInterpolatorGray3( const RefCountPtr< const Lang::FacetNormalGray > & n1,
																														 const RefCountPtr< const Lang::FacetNormalGray > & n2,
																														 const RefCountPtr< const Lang::FacetNormalGray > & n3 )
	: n1_( n1 ), n2_( n2 ), n3_( n3 ),
		p1_( n1_->position( ) ), p2_( n2_->position( ) ), p3_( n3_->position( ) ),
		d1_( 0, 0, 0, bool( ) ), d2_( 0, 0, 0, bool( ) ), d3_( 0, 0, 0, bool( ) )
{

	Concrete::Coords3D r12 = p2_ - p1_;
	Concrete::Coords3D r23 = p3_ - p2_;
	Concrete::Coords3D r31 = p1_ - p3_;

	d1_ = ( p2_ + r23 * ( Concrete::innerScalar( r23, p1_ - p2_ ) / Concrete::innerScalar( r23, r23 ) ) - p1_ ).direction( );
	d2_ = ( p3_ + r31 * ( Concrete::innerScalar( r31, p2_ - p3_ ) / Concrete::innerScalar( r31, r31 ) ) - p2_ ).direction( );
	d3_ = ( p1_ + r12 * ( Concrete::innerScalar( r12, p3_ - p1_ ) / Concrete::innerScalar( r12, r12 ) ) - p3_ ).direction( );

	// The lengs are obtained by projecting any of the other two points on the direction:
	l1_ = Concrete::inner( d1_, p2_ - p1_ );
	l2_ = Concrete::inner( d2_, p3_ - p2_ );
	l3_ = Concrete::inner( d3_, p1_ - p3_ );
}

Computation::FacetInterpolatorGray3::~FacetInterpolatorGray3( )
{ }

RefCountPtr< const Lang::Gray >
Computation::FacetInterpolatorGray3::compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez )
 const
{
	// #define USE_CONVEX_COMBINATION
#ifdef USE_CONVEX_COMBINATION
	// This is another way to assign initial weights.	The symmetry is not obvious, but
	// follows if we note that the convex combination of the corners
	//	 (1-(w2+w3)) * p1_ + w2 * p2_ + w3 * p3_
	// that result in <point> is unique inside the triangle, and then this is extended to all the plane.
	//
	Concrete::Coords3D b = point - p1_;
	Concrete::Coords3D a1 = p2_ - p1_;
	Concrete::Coords3D a2 = p3_ - p1_;
	const Physical< 2, 0 > a11 = Concrete::inner( a1, a1 );
	const Physical< 2, 0 > a12 = Concrete::inner( a2, a1 );
	const Physical< 2, 0 > a22 = Concrete::inner( a2, a2 );
	const Physical< 2, 0 > b1 = Concrete::inner( a1, b );
	const Physical< 2, 0 > b2 = Concrete::inner( a2, b );
	const Physical< 4, 0 > det = ( a11 * a22 - a12 * a12 );
	if( det.abs( ) < 1e-8 )
		{
			// The surface has no area, so the color shouldn't matter.
			return Lang::THE_BLACK;
		}
	Physical< -4, 0 > invDet = 1. / det;
	double w2 = invDet * (	 a22 * b1 - a12 * b2 );
	double w3 = invDet * ( - a12 * b1 + a11 * b2 );
	double w1 = 1 - ( w2 + w3 );
	// Inside the triangle, this will do nothing by construction.	However, outside the triangle we want to avoid negative weights, maybe...
	// Or maybe negative weights are OK?
	w1 = std::max( 0., w1 );
	w2 = std::max( 0., w2 );
	w3 = std::max( 0., w3 );
#else
	// This is one way to assign initial weights.	It is symmetric in construction.
	//
	double w1 = std::max( 0., 1 - static_cast< double >( Concrete::inner( d1_, point - p1_ ) / l1_ ) );
	double w2 = std::max( 0., 1 - static_cast< double >( Concrete::inner( d2_, point - p2_ ) / l2_ ) );
	double w3 = std::max( 0., 1 - static_cast< double >( Concrete::inner( d3_, point - p3_ ) / l3_ ) );
#endif
	
	
	double sumInv = 1 / ( w1 + w2 + w3 );
	w1 *= sumInv;
	w2 *= sumInv;
	w3 *= sumInv;

	const Concrete::UnitFloatTriple & n1 = n1_->normal( );
	const Concrete::UnitFloatTriple & n2 = n2_->normal( );
	const Concrete::UnitFloatTriple & n3 = n3_->normal( );

	// Here we do the ugly linear interpolation, and let the UnitFloatTriple constructor normalize the result.
	Concrete::UnitFloatTriple normal( w1 * n1.x_ + w2 * n2.x_ + w3 * n3.x_,
																		w1 * n1.y_ + w2 * n2.y_ + w3 * n3.y_,
																		w1 * n1.z_ + w2 * n2.z_ + w3 * n3.z_ );

	Concrete::Gray c1 = n1_->compute( point, normal, tf, eyez, lights ).mulNoCheck( w1 );
	Concrete::Gray c2 = n2_->compute( point, normal, tf, eyez, lights ).mulNoCheck( w2 );
	Concrete::Gray c3 = n3_->compute( point, normal, tf, eyez, lights ).mulNoCheck( w3 );

	Concrete::Gray res = c1.addNoCheck( c2 ).addNoCheck( c3 );
	if( res.gr_ < 0 )
		{
			res.gr_ = 0;
		}
	else if( res.gr_ > 1 )
		{
			res.gr_ = 1;
		}
	return RefCountPtr< const Lang::Gray >( new Lang::Gray( res ) );
}

RefCountPtr< const Lang::Gray >
Computation::FacetInterpolatorGray3::getDebugColor( ) const
{
	return RefCountPtr< const Lang::Gray >( new Lang::Gray( n1_->getDebugColor( ) ) );
}

RefCountPtr< const Computation::FacetInterpolatorGray >
Computation::FacetInterpolatorGray3::transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Computation::FacetInterpolatorGray >
		( new FacetInterpolatorGray3( n1_->transformed( tf ),
																	n2_->transformed( tf ),
																	n3_->transformed( tf ) ) );
}

void
Computation::FacetInterpolatorGray3::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::FacetNormalGray * >( n1_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::FacetNormalGray * >( n2_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::FacetNormalGray * >( n3_.getPtr( ) )->gcMark( marked );
}
