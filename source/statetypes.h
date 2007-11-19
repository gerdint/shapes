#ifndef statetypes_h
#define statetypes_h

#include "Shapes_Ast_decls.h"
#include "Shapes_Kernel_decls.h"
#include "Shapes_Lang_decls.h"
#include "Shapes_Lang_decls.h"

#include "ptrowner.h"
#include "refcount.h"
#include "environment.h"
#include "pdfstructure.h"
#include "elementarylength.h"
#include "concretecolors.h"

#include <list>
#include <iostream>
#include <stack>
#include <set>


namespace Shapes
{
	namespace Lang
	{

		class DynamicBindings : public Lang::Value
		{
		public:
			typedef std::map< Kernel::DynamicEnvironmentKeyType, std::pair< Kernel::VariableHandle, Ast::SourceLocation > > MapType;
			DynamicBindings( );
			virtual ~DynamicBindings( );
			TYPEINFODECL;
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const = 0;
			DISPATCHDECL;
		};
		
		class DynamicBindingsPair : public DynamicBindings
		{
			RefCountPtr< const Lang::DynamicBindings > car_;
			RefCountPtr< const Lang::DynamicBindings > cdr_;
		public:
			DynamicBindingsPair( const RefCountPtr< const Lang::DynamicBindings > & car, const RefCountPtr< const Lang::DynamicBindings > & cdr );
			virtual ~DynamicBindingsPair( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};
		
		class UserDynamicBinding : public DynamicBindings
		{
			Kernel::DynamicEnvironmentKeyType key_;
			const char * id_;
			Ast::SourceLocation loc_;
			Kernel::VariableHandle var_;
		public:
			UserDynamicBinding( const Kernel::DynamicEnvironmentKeyType & key, const char * id, const Ast::SourceLocation & loc, const Kernel::VariableHandle & var );
			virtual ~UserDynamicBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};
		
		class WidthBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			Concrete::Length val_;
		public:
			WidthBinding( const Ast::SourceLocation & loc, Concrete::Length val );
			virtual ~WidthBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};
		
		class MiterLimitBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			Concrete::Length val_;
		public:
			MiterLimitBinding( const Ast::SourceLocation & loc, Concrete::Length val );
			virtual ~MiterLimitBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class Color : public Lang::Value
		{
		public:
			Color( );
			virtual ~Color( );
			static RefCountPtr< const Lang::Class > TypeID;
			static RefCountPtr< const char > staticTypeName( );
			virtual void setStroking( std::ostream & os ) const = 0;
			virtual void setNonStroking( std::ostream & os ) const = 0;
			virtual RefCountPtr< SimplePDF::PDF_Vector > componentVector( ) const = 0;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		class Gray : public Color
		{
			Concrete::Gray components_;
		public:
			Gray( const Concrete::Gray & components );
			virtual ~Gray( );
			TYPEINFODECL;
			virtual void setStroking( std::ostream & os ) const;
			virtual void setNonStroking( std::ostream & os ) const;
			virtual RefCountPtr< SimplePDF::PDF_Vector > componentVector( ) const;
			const Concrete::Gray & components( ) const { return components_; };
			DISPATCHDECL;
		};
		
		class RGB : public Color
		{
			Concrete::RGB components_;
		public:
			RGB( const Concrete::RGB & components );
			virtual ~RGB( );
			TYPEINFODECL;
			virtual void setStroking( std::ostream & os ) const;
			virtual void setNonStroking( std::ostream & os ) const;
			virtual RefCountPtr< SimplePDF::PDF_Vector > componentVector( ) const;
			const Concrete::RGB & components( ) const { return components_; };
			DISPATCHDECL;
		};
		
		class CMYK : public Color
		{
			Concrete::CMYK components_;
		public:
			CMYK( const Concrete::CMYK & components );
			virtual ~CMYK( );
			TYPEINFODECL;
			virtual void setStroking( std::ostream & os ) const;
			virtual void setNonStroking( std::ostream & os ) const;
			virtual RefCountPtr< SimplePDF::PDF_Vector > componentVector( ) const;
			const Concrete::CMYK & components( ) const { return components_; };
			DISPATCHDECL;
		};
		
		class StrokingBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			RefCountPtr< const Lang::Color > color_;
		public:
			StrokingBinding( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Color > color );
			virtual ~StrokingBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class NonStrokingBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			RefCountPtr< const Lang::Color > color_;
		public:
			NonStrokingBinding( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Color > color );
			virtual ~NonStrokingBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};
		
		class Alpha : public Lang::NoOperatorOverloadValue
		{
		public:
			bool isShape_;
			double a_;
			Alpha( bool isShape, double a );
			virtual ~Alpha( );
			static void applyGraphicsState( std::ostream & os, SimplePDF::PDF_Resources * resources, const Lang::Alpha & self, bool isStroking );
			TYPEINFODECL;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			
		private:
			static std::map< double, RefCountPtr< SimplePDF::PDF_Object > > strokingShapeResourcemap;
			static std::map< double, RefCountPtr< SimplePDF::PDF_Object > > strokingOpacityResourcemap;
			static std::map< double, RefCountPtr< SimplePDF::PDF_Object > > nonStrokingShapeResourcemap;
			static std::map< double, RefCountPtr< SimplePDF::PDF_Object > > nonStrokingOpacityResourcemap;
		};
		
		class AlphaBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			RefCountPtr< const Lang::Alpha > alpha_;
			bool isStroking_;
		public:
			AlphaBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::Alpha > & alpha, bool isStroking );
			virtual ~AlphaBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class SoftMask : public Lang::NoOperatorOverloadValue
		{
		public:
			typedef enum { ALPHA, LUMINOSITY } SubType;
			RefCountPtr< SimplePDF::PDF_Object > graphicsStateResource_;
			
			SoftMask( );	// this yields the None soft mask
			SoftMask( SubType subType, const RefCountPtr< const Lang::TransparencyGroup > & tpGroup, const RefCountPtr< const Lang::Color > & background, const RefCountPtr< const Lang::PDF_Function > & transfer );
			virtual ~SoftMask( );
			TYPEINFODECL;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
		};
		
		class Dash : public Lang::Value
		{
		public:
			class Iterator
			{
				RefCountPtr< std::list< Concrete::Length > > mem_;
				std::list< Concrete::Length >::const_iterator begin_;
				std::list< Concrete::Length >::const_iterator end_;
				double scale_;
				std::list< Concrete::Length >::const_iterator i_;
				bool on_;
				Concrete::Length length_;
			public:
				Iterator( RefCountPtr< std::list< Concrete::Length > > pattern, double scale, std::list< Concrete::Length >::const_iterator i, bool on, Concrete::Length length );
				Iterator & operator ++ ( );
				bool isOn( ) const;
				Concrete::Length getLength( ) const;
			};
		private:
			RefCountPtr< std::list< Concrete::Length > > pattern_;
			Concrete::Length phase_;
			double scale_;
			Concrete::Length myLength_;
		public:
			Dash( );
			Dash( RefCountPtr< std::list< Concrete::Length > > pattern, Concrete::Length phase, double scale );
			Dash( RefCountPtr< std::list< Concrete::Length > > pattern, Concrete::Length phase, double scale, Concrete::Length length ); // length must be the sum of pattern
			~Dash( );
			TYPEINFODECL;
			void setDash( std::ostream & os ) const;
			RefCountPtr< SimplePDF::PDF_Vector > getDashArray( ) const;
			RefCountPtr< const Lang::Dash > scaled( double factor ) const;
			RefCountPtr< const Lang::Dash > shifted( Concrete::Length dist ) const;
			Concrete::Length length( ) const;
			bool isSolid( ) const;
			Iterator begin( ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};

		class DashBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			RefCountPtr< const Lang::Dash > dash_;
		public:
			DashBinding( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Dash > dash );
			virtual ~DashBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class CapStyle : public Lang::NoOperatorOverloadValue
		{
		public:
			typedef enum { CAP_BUTT = 0, CAP_ROUND, CAP_SQUARE, CAP_SAME, CAP_UNDEFINED } ValueType;
			ValueType cap_;
			CapStyle( const ValueType & cap );
			TYPEINFODECL;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
		};
		
		class CapStyleBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			Lang::CapStyle::ValueType cap_;
		public:
			CapStyleBinding( const Ast::SourceLocation & loc, const Lang::CapStyle::ValueType & cap );
			virtual ~CapStyleBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};
		
		class JoinStyle : public Lang::NoOperatorOverloadValue
		{
		public:
			typedef enum { JOIN_MITER = 0, JOIN_ROUND, JOIN_BEVEL, JOIN_SAME, JOIN_UNDEFINED } ValueType;
			ValueType join_;
			JoinStyle( const ValueType & join );
			TYPEINFODECL;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
		};
		
		class JoinStyleBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			Lang::JoinStyle::ValueType join_;
		public:
			JoinStyleBinding( const Ast::SourceLocation & loc, const Lang::JoinStyle::ValueType & join );
			virtual ~JoinStyleBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class BlendMode : public Lang::NoOperatorOverloadValue
		{
		public:
			typedef enum {
				NORMAL = 0, MULTIPLY, SCREEN, OVERLAY, DARKEN, LIGHTEN, COLOR_DODGE, COLOR_BURN, HARD_LIGHT, SOFT_LIGHT, DIFFERENCE, EXCLUSION,
				HUE, SATURATION, COLOR, LUMINOSITY, BLEND_SAME, BLEND_UNDEFINED } ValueType;
			ValueType mode_;
			BlendMode( const ValueType & mode );
			TYPEINFODECL;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			static void applyGraphicsState( std::ostream & os, SimplePDF::PDF_Resources * resources, const ValueType & mode );
		private:
			static std::map< ValueType, RefCountPtr< SimplePDF::PDF_Object > > resourceMap;
		};
		
		class BlendModeBinding : public DynamicBindings
		{
			Ast::SourceLocation loc_;
			Lang::BlendMode::ValueType blend_;
		public:
			BlendModeBinding( const Ast::SourceLocation & loc, const Lang::BlendMode::ValueType & blend );
			virtual ~BlendModeBinding( );
			virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		// Currently, color space is not part of the graphics state; it's only used for blending in transparency groups.
		// However, since it can be part of the graphcs state, the definition is placed here anyway.
		// The related dynamic binding classes are found in dynamicenvironment.h
		class ColorSpace : public Lang::NoOperatorOverloadValue
		{
		public:
			ColorSpace( );
			virtual ~ColorSpace( );
			TYPEINFODECL;
			virtual RefCountPtr< SimplePDF::PDF_Name > name( ) const = 0;
			virtual bool isInherent( ) const { return false; }
			virtual bool isBlendable( ) const { return false; }
			virtual bool containsColor( const Lang::Color * col ) const = 0;
			virtual size_t numberOfComponents( ) const = 0;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
		};
		
		class InheritedColorSpace : public Lang::ColorSpace
		{
		public:
			InheritedColorSpace( );
			~InheritedColorSpace( );
			virtual bool isInherent( ) const { return true; }
			virtual bool isBlendable( ) const { return true; }
			virtual bool containsColor( const Lang::Color * col ) const;
			virtual RefCountPtr< SimplePDF::PDF_Name > name( ) const;
			virtual size_t numberOfComponents( ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
		};
		
		template< class C >
		class DeviceColorSpace : public Lang::ColorSpace
		{
			RefCountPtr< const SimplePDF::PDF_Name > space_;
			size_t numberOfComponents_;
		public:
			DeviceColorSpace( const char * spaceName, size_t numOfComponents );
			~DeviceColorSpace( );
			virtual bool isBlendable( ) const { return true; }
			virtual bool containsColor( const Lang::Color * col ) const;
			virtual RefCountPtr< SimplePDF::PDF_Name > name( ) const;
			virtual size_t numberOfComponents( ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
		};
		
		template< class C >
		DeviceColorSpace< C >::DeviceColorSpace( const char * spaceName, size_t numOfComponents )
			: space_( RefCountPtr< SimplePDF::PDF_Name >( new SimplePDF::PDF_Name( spaceName ) ) ),
				numberOfComponents_( numOfComponents )
		{ }
		
		template< class C >
			DeviceColorSpace< C >::~DeviceColorSpace< C >( )
			{ }
		
		template< class C >
			RefCountPtr< SimplePDF::PDF_Name >
			DeviceColorSpace< C >::name( ) const
			{
				return space_.unconst_cast< SimplePDF::PDF_Name >( );
			}
		
		template< class C >
		size_t
		DeviceColorSpace< C >::numberOfComponents( ) const
		{
			return numberOfComponents_;
		}
		
		template< class C >
		bool
		DeviceColorSpace< C >::containsColor( const Lang::Color * col ) const
		{
			return dynamic_cast< const C * >( col ) != 0;
		}
		
	}

	namespace Kernel
	{

		class UserDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
			Kernel::DynamicEnvironmentKeyType key_;
			RefCountPtr< const Lang::Function > filter_;
			Kernel::VariableHandle defaultVal_;
		public:
			UserDynamicVariableProperties( const char * name, const Kernel::DynamicEnvironmentKeyType & key, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal );
			virtual ~UserDynamicVariableProperties( );
			
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		
		class UserDynamicStateProperties : public Kernel::DynamicStateProperties
		{
			Kernel::DynamicEnvironmentKeyType key_;
			Kernel::PassedEnv defaultStateEnv_;
			Kernel::PassedDyn defaultStateDyn_;
			Ast::StateReference * defaultState_;
		public:
			UserDynamicStateProperties( const char * name, const Kernel::DynamicEnvironmentKeyType & key, const Kernel::PassedEnv & defaultStateEnv, Kernel::PassedDyn defaultStateDyn, Ast::StateReference * defaultState );
			virtual ~UserDynamicStateProperties( );
			
			virtual Kernel::StateHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::StateHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		
		class WidthDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			WidthDynamicVariableProperties( const char * name );
			virtual ~WidthDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		class MiterLimitDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			MiterLimitDynamicVariableProperties( const char * name );
			virtual ~MiterLimitDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		
		class StrokingDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			StrokingDynamicVariableProperties( const char * name );
			virtual ~StrokingDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		class NonStrokingDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			NonStrokingDynamicVariableProperties( const char * name );
			virtual ~NonStrokingDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		class AlphaDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
			bool isStroking_;
		public:
			AlphaDynamicVariableProperties( const char * name, bool isStroking );
			virtual ~AlphaDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		
		class DashDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			DashDynamicVariableProperties( const char * name );
			virtual ~DashDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		class CapStyleDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			CapStyleDynamicVariableProperties( const char * name );
			virtual ~CapStyleDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		class JoinStyleDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			JoinStyleDynamicVariableProperties( const char * name );
			virtual ~JoinStyleDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		class BlendModeDynamicVariableProperties : public Kernel::DynamicVariableProperties
		{
		public:
			BlendModeDynamicVariableProperties( const char * name );
			virtual ~BlendModeDynamicVariableProperties( );
			virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
			virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
		};
		
		class GraphicsState
		{
		public:
			RefCountPtr< const Lang::Color > strokingColor_;			// Use Gray( -1 ) for no-op, and NullPtr for undefined
			RefCountPtr< const Lang::Color > nonStrokingColor_;	 // Use Gray( -1 ) for no-op, and NullPtr for undefined
			Concrete::Length width_;												// Use negative value for no-op, and NAN for undefined
			Lang::CapStyle::ValueType cap_;
			Lang::JoinStyle::ValueType join_;
			Concrete::Length miterLimit_;																			// Use negative value for no-op, and NAN for undefined
			RefCountPtr< const Lang::Dash > dash_;								// (Dash with negative scale is no-op.)	Use NullPtr for undefined
			Lang::BlendMode::ValueType blend_;
			bool alphaIsShape_;																			// See below.
			RefCountPtr< const Lang::Alpha > strokingAlpha_;			// Use negative value for no-op, NAN for undefined
			RefCountPtr< const Lang::Alpha > nonStrokingAlpha_;	 // Use negative value for no-op, NAN for undefined
		public:
			GraphicsState( );
			explicit GraphicsState( const Kernel::GraphicsState & orig );	 // explicit, since reference counting shall be used in most cases
			GraphicsState( const Kernel::GraphicsState & newValues, const Kernel::GraphicsState & oldValues );
			GraphicsState( bool setDefaults );		
			~GraphicsState( );
			
			bool synchStrokingColor( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchNonStrokingColor( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchStrokingAlpha( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchNonStrokingAlpha( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchWidth( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchCap( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchJoin( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchMiterLimit( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchDash( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchBlend( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			
			bool synchForStroke( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );
			bool synchForNonStroke( std::ostream & os, const GraphicsState * ref, SimplePDF::PDF_Resources * resources, bool force = false );

			bool synchStrokingColorWithNonStrokingColor( std::ostream & os, SimplePDF::PDF_Resources * resources, Concrete::Length width = Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) ) );
		};
		
		class Auto_qQ
		{
			Kernel::GraphicsState * state_;
			Kernel::GraphicsState * enterState_;
			std::ostream & os_;
			bool activated_;
		public:
			Auto_qQ( Kernel::GraphicsState * state, std::ostream & os, bool active = true );
			void activate( );
			~Auto_qQ( );
		};

	}
}

#endif
