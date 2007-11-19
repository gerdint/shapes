#include <cmath>

#include "lighttypes.h"
#include "astclass.h"
#include "globals.h"
#include "constructorrepresentation.h"

using namespace Shapes;


Lang::SpecularReflection::SpecularReflection( )
{ }

Lang::SpecularReflection::~SpecularReflection( )
{ }

DISPATCHIMPL( SpecularReflection );

RefCountPtr< const Lang::Class > Lang::SpecularReflection::TypeID( new Lang::SystemFinalClass( strrefdup( "SpecularReflection" ) ) );
TYPEINFOIMPL( SpecularReflection );

void
Lang::SpecularReflection::gcMark( Kernel::GCMarkedSet & marked )
{ }

Lang::SpecularReflectionTerm::SpecularReflectionTerm( double weight, double exponent )
	: weight_( weight ), exponent_( exponent )
{ }

Lang::SpecularReflectionTerm::~SpecularReflectionTerm( )
{ }

double
Lang::SpecularReflectionTerm::weight( ) const
{
	return weight_;
}

RefCountPtr< const Lang::SpecularReflection >
Lang::SpecularReflectionTerm::multiply( double scalar ) const
{
	return RefCountPtr< const Lang::SpecularReflection >( new Lang::SpecularReflectionTerm( scalar * weight_, exponent_ ) );
}


Concrete::Gray
Lang::SpecularReflectionTerm::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::GrayLight & light ) const
{
	return light.illuminateGray( point, eyez, unitNormal, *this );
}

Concrete::RGB
Lang::SpecularReflectionTerm::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::RGBLight & light ) const
{
	return light.illuminateRGB( point, eyez, unitNormal, *this );
}

Concrete::Gray
Lang::SpecularReflectionTerm::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & autoIntensity ) const
{
	// According to my calculations, the autoIntensity seen by the eye does not change with the distance to the object;
	// one the size of the object changes, which will make the received power decrease with distance.
	return Concrete::Gray( weight_ * pow( fabs( Concrete::inner( unitNormal, ( Concrete::Coords3D( 0, 0, eyez ) - point ).direction( ) ) ), exponent_ ) * autoIntensity.gr_ );
}

Concrete::RGB
Lang::SpecularReflectionTerm::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & autoIntensity ) const
{
	// According to my calculations, the autoIntensity seen by the eye does not change with the distance to the object;
	// one the size of the object changes, which will make the received power decrease with distance.
	double a = weight_ * pow( fabs( Concrete::inner( unitNormal, ( Concrete::Coords3D( 0, 0, eyez ) - point ).direction( ) ) ), exponent_ );
	return Concrete::RGB( a * autoIntensity.r_, a * autoIntensity.g_, a * autoIntensity.b_ );
}

double
Lang::SpecularReflectionTerm::exponent( ) const
{
	return exponent_;
}


Lang::SpecularReflectionNull::SpecularReflectionNull( )
{ }

Lang::SpecularReflectionNull::~SpecularReflectionNull( )
{ }

double
Lang::SpecularReflectionNull::weight( ) const
{
	return 0.;
}

RefCountPtr< const Lang::SpecularReflection >
Lang::SpecularReflectionNull::multiply( double scalar ) const
{
	return Lang::THE_SPECULARREFLECTION_NULL;
}

Concrete::Gray
Lang::SpecularReflectionNull::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::GrayLight & light ) const
{
	return Concrete::Gray( 0. );
}

Concrete::RGB
Lang::SpecularReflectionNull::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::RGBLight & light ) const
{
	return Concrete::RGB( 0., 0., 0. );
}

Concrete::Gray
Lang::SpecularReflectionNull::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & autoIntensity ) const
{
	return Concrete::Gray( 0. );
}

Concrete::RGB
Lang::SpecularReflectionNull::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & autoIntensity ) const
{
	return Concrete::RGB( 0., 0., 0. );
}


Lang::SpecularReflectionPair::SpecularReflectionPair( const RefCountPtr< const Lang::SpecularReflection > & car, const RefCountPtr< const Lang::SpecularReflection > & cdr )
	: car_( car ), cdr_( cdr )
{ }

Lang::SpecularReflectionPair::~SpecularReflectionPair( )
{ }

double
Lang::SpecularReflectionPair::weight( ) const
{
	return car_->weight( ) + cdr_->weight( );
}

RefCountPtr< const Lang::SpecularReflection >
Lang::SpecularReflectionPair::multiply( double scalar ) const
{
	return RefCountPtr< const Lang::SpecularReflection >
		( new Lang::SpecularReflectionPair( car_->multiply( scalar ), cdr_->multiply( scalar ) ) );
}


Concrete::Gray
Lang::SpecularReflectionPair::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::GrayLight & light ) const
{
	return car_->illuminate( point, eyez, unitNormal, light ).addNoCheck( cdr_->illuminate( point, eyez, unitNormal, light ) );
}

Concrete::RGB
Lang::SpecularReflectionPair::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::RGBLight & light ) const
{
	return car_->illuminate( point, eyez, unitNormal, light ).addNoCheck( cdr_->illuminate( point, eyez, unitNormal, light ) );
}

Concrete::Gray
Lang::SpecularReflectionPair::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & autoIntensity ) const
{
	return car_->illuminate( point, eyez, unitNormal, autoIntensity ).addNoCheck( cdr_->illuminate( point, eyez, unitNormal, autoIntensity ) );
}

Concrete::RGB
Lang::SpecularReflectionPair::illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & autoIntensity ) const
{
	return car_->illuminate( point, eyez, unitNormal, autoIntensity ).addNoCheck( cdr_->illuminate( point, eyez, unitNormal, autoIntensity ) );
}


Lang::LightSource::LightSource( bool shadows )
	: shadows_( shadows )
{ }

DISPATCHIMPL( LightSource );

RefCountPtr< const Lang::Class > Lang::LightSource::TypeID( new Lang::SystemFinalClass( strrefdup( "LightSource" ) ) );
TYPEINFOIMPL( LightSource );

Lang::LightSource::~LightSource( )
{ }

RefCountPtr< const Lang::Geometric3D >
Lang::LightSource::transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
	return typed_transformed( tf );
}

RefCountPtr< const Lang::Geometric2D >
Lang::LightSource::to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
	throw Exceptions::MiscellaneousRequirement( "A LightSource cannot be viewed in 2D." );
}

void
Lang::LightSource::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::GrayLight::GrayLight( bool shadows )
	: Lang::LightSource( shadows )
{ }

Lang::GrayLight::~GrayLight( )
{ }


Lang::RGBLight::RGBLight( bool shadows )
	: Lang::LightSource( shadows )
{ }

Lang::RGBLight::~RGBLight( )
{ }


Lang::AmbientLightGray::AmbientLightGray( const Concrete::Gray & intensity )
	: Lang::GrayLight( false ), intensity_( intensity )
{ }

Lang::AmbientLightGray::~AmbientLightGray( )
{ }

RefCountPtr< const Lang::LightSource >
Lang::AmbientLightGray::typed_transformed( const Lang::Transform3D & tf ) const
{
	// It's sad that we don't have access to the self reference, so that it could be returned instead of a new object with is just a copy of ourselves.
	return RefCountPtr< const Lang::LightSource >( new Lang::AmbientLightGray( intensity_ ) );
}

Concrete::RGB
Lang::AmbientLightGray::illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	double a = intensity_.gr_ * refl.weight( );
	return Concrete::RGB( a, a, a );
}

Concrete::Gray
Lang::AmbientLightGray::illuminateGray( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	return Concrete::Gray( intensity_.gr_ * refl.weight( ) );
}

Lang::AmbientLightRGB::AmbientLightRGB( const Concrete::RGB & intensity )
	: Lang::RGBLight( false ), intensity_( intensity )
{ }

Lang::AmbientLightRGB::~AmbientLightRGB( )
{ }

RefCountPtr< const Lang::LightSource >
Lang::AmbientLightRGB::typed_transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::LightSource >( new Lang::AmbientLightRGB( intensity_ ) );
}

Concrete::RGB
Lang::AmbientLightRGB::illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	const double w = refl.weight( );
	return Concrete::RGB( intensity_.r_ * w, intensity_.g_ * w, intensity_.b_ * w );
}

Lang::SpecularLightGray::SpecularLightGray( const Concrete::Coords3D & p, const Concrete::Gray & intensity, Concrete::Length intensityRadius, bool shadows )
	: Lang::GrayLight( shadows ), p_( p ), intensity_( intensity ), intensityRadius_( intensityRadius )
{ }

Lang::SpecularLightGray::~SpecularLightGray( )
{ }

RefCountPtr< const Lang::LightSource >
Lang::SpecularLightGray::typed_transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::LightSource >( new Lang::SpecularLightGray( p_.transformed( tf ), intensity_, intensityRadius_, shadows_ ) );
}

Concrete::RGB
Lang::SpecularLightGray::illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	throw Exceptions::NotImplemented( "XXXLightXXX::illuminateXXX" );
}

Concrete::Gray
Lang::SpecularLightGray::illuminateGray( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	Concrete::Coords3D rLight = p_ - point;
	Concrete::Coords3D rEye = Concrete::Coords3D( Concrete::ZERO_LENGTH, Concrete::ZERO_LENGTH, eyez ) - point;
	Concrete::Length dLight = rLight.norm( );
	Concrete::Length dEye = rEye.norm( );
	Concrete::UnitFloatTriple rHatLight = rLight.direction( dLight );
	Concrete::UnitFloatTriple rHatEye = rEye.direction( dEye );
	Concrete::Length dTot = dLight + dEye;

	Concrete::UnitFloatTriple rHatReflection = unitNormal.reflect( rHatLight );

//	 std::cerr << "Point: " << Helpers::shapesFormat( point ) << std::endl ;
//	 std::cerr << "rLight: " << Helpers::shapesFormat( rLight ) << std::endl ;
//	 std::cerr << "Surface normal: " << Helpers::shapesFormat( unitNormal ) << std::endl ;
//	 std::cerr << "rHatReflection: " << Helpers::shapesFormat( rHatReflection ) << std::endl ;
//	 std::cerr << "rEye: " << Helpers::shapesFormat( rEye ) << std::endl ;
//	 std::cerr << "rHatEye: " << Helpers::shapesFormat( rHatEye ) << std::endl ;
//	 std::cerr << "	Phong's angle: " << acos( Concrete::inner( rHatReflection, rHatEye ) ) << std::endl ;

	// This breaks the Phong model, but I want reflection in all directions!
	const double cPhong2 = cos( 0.5 * acos( Concrete::inner( rHatReflection, rHatEye ) ) );

	// Just in case numeric errors make c negative anyway...
	if( cPhong2 <= 0 )
		{
			return Concrete::Gray( 0 );
		}

	// Further, I want something that becomes ambient when refl.exponent == 0.

	double cAmb = 1;
	if( refl.exponent( ) < 1 )
		{
			cAmb = pow( fabs( Concrete::inner( unitNormal, rHatLight ) ), 1 - refl.exponent( ) );
		}

	double rRel = 1;
	if( intensityRadius_ < Concrete::HUGE_LENGTH )
		{
			rRel = intensityRadius_ / dTot;
		}

//	 std::cerr << "	Intensity: " << intensity_.gr_ << std::endl ;
//	 std::cerr << "	Phong2: " << pow( cPhong2, refl.exponent( ) ) << std::endl ;
//	 std::cerr << "	Amb2: " << pow( cIn, 1 / ( 1 + 10 * refl.exponent( ) ) ) << std::endl ;
//	 std::cerr << "	Distance factor: " << rRel * rRel << std::endl ;
//	 std::cerr << "	Total: " << refl.weight( ) * pow( c, refl.exponent( ) ) * rRel * rRel * intensity_.gr_ << std::endl ;

	return Concrete::Gray( refl.weight( ) * pow( cPhong2, refl.exponent( ) ) * cAmb * rRel * rRel * intensity_.gr_ );
}

Lang::SpecularLightRGB::SpecularLightRGB( const Concrete::Coords3D & p, const Concrete::RGB & intensity, Concrete::Length intensityRadius, bool shadows )
	: Lang::RGBLight( shadows ), p_( p ), intensity_( intensity ), intensityRadius_( intensityRadius )
{ }

Lang::SpecularLightRGB::~SpecularLightRGB( )
{ }

RefCountPtr< const Lang::LightSource >
Lang::SpecularLightRGB::typed_transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::LightSource >( new Lang::SpecularLightRGB( p_.transformed( tf ), intensity_, intensityRadius_, shadows_ ) );
}

Concrete::RGB
Lang::SpecularLightRGB::illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	throw Exceptions::NotImplemented( "XXXLightXXX::illuminateXXX" );
}

Lang::DistantLightGray::DistantLightGray( const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & intensity, bool shadows )
	: Lang::GrayLight( shadows ), unitNormal_( unitNormal ), intensity_( intensity )
{ }

Lang::DistantLightGray::~DistantLightGray( )
{ }

RefCountPtr< const Lang::LightSource >
Lang::DistantLightGray::typed_transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::LightSource >( new Lang::DistantLightGray( tf.transformPlaneUnitNormal( unitNormal_ ), intensity_, shadows_ ) );
}

Concrete::RGB
Lang::DistantLightGray::illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	throw Exceptions::NotImplemented( "XXXLightXXX::illuminateXXX" );
}

Concrete::Gray
Lang::DistantLightGray::illuminateGray( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	throw Exceptions::NotImplemented( "XXXLightXXX::illuminateXXX" );
}


Lang::DistantLightRGB::DistantLightRGB( const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & intensity, bool shadows )
	: Lang::RGBLight( shadows ), unitNormal_( unitNormal ), intensity_( intensity )
{ }

Lang::DistantLightRGB::~DistantLightRGB( )
{ }

RefCountPtr< const Lang::LightSource >
Lang::DistantLightRGB::typed_transformed( const Lang::Transform3D & tf ) const
{
	return RefCountPtr< const Lang::LightSource >( new Lang::DistantLightRGB( tf.transformPlaneUnitNormal( unitNormal_ ), intensity_, shadows_ ) );
}

Concrete::RGB
Lang::DistantLightRGB::illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const
{
	throw Exceptions::NotImplemented( "XXXLightXXX::illuminateXXX" );
}

Lang::LightGroup::LightGroup( )
{ }

DISPATCHIMPL( LightGroup );

RefCountPtr< const Lang::Class > Lang::LightGroup::TypeID( new Lang::SystemFinalClass( strrefdup( "LightGroup" ) ) );
TYPEINFOIMPL( LightGroup );

Lang::LightGroup::~LightGroup( )
{ }

RefCountPtr< const Lang::Geometric3D >
Lang::LightGroup::transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
	throw Exceptions::NotImplemented( "XXXLightXXX::illuminateXXX" );
}

RefCountPtr< const Lang::Geometric2D >
Lang::LightGroup::to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const
{
	throw Exceptions::NotImplemented( "XXXLightXXX::illuminateXXX" );
}


Lang::LightNull::LightNull( )
{ }

Lang::LightNull::~LightNull( )
{ }

RefCountPtr< const Lang::LightGroup >
Lang::LightNull::typed_transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::LightGroup > & self ) const
{
	return self;
}
 
bool
Lang::LightNull::isNull( ) const
{
	return true;
}

bool
Lang::LightNull::containsRGB( ) const
{
	return false;
}

void
Lang::LightNull::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::LightPair::LightPair( const RefCountPtr< const Lang::LightSource > & car, const RefCountPtr< const Lang::LightGroup > & cdr )
	: car_( car ), cdr_( cdr )
{ }

Lang::LightPair::~LightPair( )
{ }

RefCountPtr< const Lang::LightSource >
Lang::LightPair::car( ) const
{
	return car_;
}

RefCountPtr< const Lang::LightGroup >
Lang::LightPair::cdr( ) const
{
	return cdr_;
}

RefCountPtr< const Lang::LightGroup >
Lang::LightPair::typed_transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::LightGroup > & self ) const
{
	return RefCountPtr< const Lang::LightGroup >( new Lang::LightPair( car_->typed_transformed( tf ), cdr_->typed_transformed( tf, cdr_ ) ) );
}

bool
Lang::LightPair::isNull( ) const
{
	return false;
}

bool
Lang::LightPair::containsRGB( ) const
{
	return ( ! car_->isGray( ) ) || cdr_->containsRGB( );
}

void
Lang::LightPair::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::LightSource * >( car_.getPtr( ) )->gcMark( marked );
	const_cast< Lang::LightGroup * >( cdr_.getPtr( ) )->gcMark( marked );
}
