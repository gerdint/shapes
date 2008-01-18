#ifndef lighttypes_h
#define lighttypes_h

#include "Shapes_Lang_decls.h"

#include "refcount.h"
#include "shapesvalue.h"
#include "statetypes.h"
#include "functiontypes.h"
#include "elementarycoords.h"

namespace Shapes
{
	namespace Lang
	{

	class SpecularReflection : public Lang::Value
	{
	public:
		SpecularReflection( );
		virtual ~SpecularReflection( );
		virtual double weight( ) const = 0;
		virtual RefCountPtr< const Lang::SpecularReflection > multiply( double scalar ) const = 0;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::GrayLight & light ) const = 0;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::RGBLight & light ) const = 0;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & autoIntensity ) const = 0;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & autoIntensity ) const = 0;
		TYPEINFODECL;
		DISPATCHDECL;
		virtual void gcMark( Kernel::GCMarkedSet & marked );	// This method resides here since we know (that today) no sub classes need to do gcMark.
	};

	class SpecularReflectionTerm : public Lang::SpecularReflection
	{
		double weight_;
		double exponent_;
	public:
		SpecularReflectionTerm( double weight, double exponent );
		virtual ~SpecularReflectionTerm( );
		virtual double weight( ) const;
		virtual RefCountPtr< const Lang::SpecularReflection > multiply( double scalar ) const;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::GrayLight & light ) const;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::RGBLight & light ) const;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & autoIntensity ) const;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & autoIntensity ) const;
		double exponent( ) const;
	};

	class SpecularReflectionPair : public Lang::SpecularReflection
	{
		RefCountPtr< const Lang::SpecularReflection > car_;
		RefCountPtr< const Lang::SpecularReflection > cdr_;
	public:
		SpecularReflectionPair( const RefCountPtr< const Lang::SpecularReflection > & car, const RefCountPtr< const Lang::SpecularReflection > & cdr );
		~SpecularReflectionPair( );
		virtual double weight( ) const;
		virtual RefCountPtr< const Lang::SpecularReflection > multiply( double scalar ) const;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::GrayLight & light ) const;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::RGBLight & light ) const;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & autoIntensity ) const;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & autoIntensity ) const;
	};

	class SpecularReflectionNull : public Lang::SpecularReflection
	{
	public:
		SpecularReflectionNull( );
		~SpecularReflectionNull( );
		virtual double weight( ) const;
		virtual RefCountPtr< const Lang::SpecularReflection > multiply( double scalar ) const;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::GrayLight & light ) const;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::RGBLight & light ) const;
		virtual Concrete::Gray illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::Gray & autoIntensity ) const;
		virtual Concrete::RGB illuminate( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Concrete::RGB & autoIntensity ) const;
	};

	class LightSource : public Lang::Geometric3D
	{
	protected:
		bool shadows_;
	public:
		LightSource( bool shadows );
		virtual ~LightSource( );
		virtual bool shadows( ) const { return shadows_; }
		virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const;
		virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const;
		virtual RefCountPtr< const Lang::LightSource > typed_transformed( const Lang::Transform3D & tf ) const = 0;
		virtual bool isGray( ) const = 0;
		TYPEINFODECL;
		virtual void gcMark( Kernel::GCMarkedSet & marked );	// This method resides here since we know (that today) no sub classes need to do gcMark.
		DISPATCHDECL;
	};

	class GrayLight : public Lang::LightSource
	{
	public:
		GrayLight( bool shadows );
		virtual ~GrayLight( );
		virtual bool isGray( ) const { return true; };
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const = 0;
		virtual Concrete::Gray illuminateGray( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const = 0;
	};

	class RGBLight : public Lang::LightSource
	{
	public:
		RGBLight( bool shadows );
		virtual ~RGBLight( );
		virtual bool isGray( ) const { return false; };
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const = 0;
	};

	class AmbientLightGray : public GrayLight
	{
		Concrete::Gray intensity_;
	public:
		AmbientLightGray( const Concrete::Gray & intensity );
		virtual ~AmbientLightGray( );
		virtual RefCountPtr< const Lang::LightSource > typed_transformed( const Lang::Transform3D & tf ) const;
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
		virtual Concrete::Gray illuminateGray( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
	};

	class AmbientLightRGB : public RGBLight
	{
		Concrete::RGB intensity_;
	public:
		AmbientLightRGB( const Concrete::RGB & intensity );
		virtual ~AmbientLightRGB( );
		virtual RefCountPtr< const Lang::LightSource > typed_transformed( const Lang::Transform3D & tf ) const;
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
	};

	class SpecularLightGray : public GrayLight
	{
		Concrete::Coords3D p_;
		Concrete::Gray intensity_;
		Concrete::Length intensityRadius_;
	public:
		SpecularLightGray( const Concrete::Coords3D & p, const Concrete::Gray & intensity, Concrete::Length intensityRadius, bool shadows );
		virtual ~SpecularLightGray( );
		virtual RefCountPtr< const Lang::LightSource > typed_transformed( const Lang::Transform3D & tf ) const;
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
		virtual Concrete::Gray illuminateGray( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
	};

	class SpecularLightRGB : public RGBLight
	{
		Concrete::Coords3D p_;
		Concrete::RGB intensity_;
		Concrete::Length intensityRadius_;
	public:
		SpecularLightRGB( const Concrete::Coords3D & p, const Concrete::RGB & intensity, Concrete::Length intensityRadius, bool shadows );
		virtual ~SpecularLightRGB( );
		virtual RefCountPtr< const Lang::LightSource > typed_transformed( const Lang::Transform3D & tf ) const;
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
	};

	class DistantLightGray : public GrayLight
	{
		Concrete::UnitFloatTriple rHatLight_;
		Concrete::Gray intensity_;
	public:
		DistantLightGray( const Concrete::UnitFloatTriple & rHatLight, const Concrete::Gray & intensity, bool shadows );
		virtual ~DistantLightGray( );
		virtual RefCountPtr< const Lang::LightSource > typed_transformed( const Lang::Transform3D & tf ) const;
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
		virtual Concrete::Gray illuminateGray( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
	};

	class DistantLightRGB : public RGBLight
	{
		Concrete::UnitFloatTriple rHatLight_;
		Concrete::RGB intensity_;
	public:
		DistantLightRGB( const Concrete::UnitFloatTriple & rHatLight, const Concrete::RGB & intensity, bool shadows );
		virtual ~DistantLightRGB( );
		virtual RefCountPtr< const Lang::LightSource > typed_transformed( const Lang::Transform3D & tf ) const;
		virtual Concrete::RGB illuminateRGB( const Concrete::Coords3D & point, const Concrete::Length eyez, const Concrete::UnitFloatTriple & unitNormal, const Lang::SpecularReflectionTerm & refl ) const;
	};

	class LightGroup : public Geometric3D
	{
	public:
		LightGroup( );
		virtual ~LightGroup( );
		virtual RefCountPtr< const Lang::LightGroup > typed_transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::LightGroup > & self ) const = 0;
		virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const;
		virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const;
		virtual bool isNull( ) const = 0;
		virtual bool containsRGB( ) const = 0;
		TYPEINFODECL;
		DISPATCHDECL;
	};

	class LightNull : public LightGroup
	{
	public:
		LightNull( );
		virtual ~LightNull( );
		virtual RefCountPtr< const Lang::LightGroup > typed_transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::LightGroup > & self ) const;
		virtual bool isNull( ) const;
		virtual bool containsRGB( ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class LightPair : public LightGroup
	{
		RefCountPtr< const Lang::LightSource > car_;
		RefCountPtr< const Lang::LightGroup > cdr_;
	public:
		LightPair( const RefCountPtr< const Lang::LightSource > & car, const RefCountPtr< const Lang::LightGroup > & cdr );
		virtual ~LightPair( );
		RefCountPtr< const Lang::LightSource > car( ) const;
		RefCountPtr< const Lang::LightGroup > cdr( ) const;
		virtual RefCountPtr< const Lang::LightGroup > typed_transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::LightGroup > & self ) const;
		virtual bool isNull( ) const;
		virtual bool containsRGB( ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	}
}

#endif
