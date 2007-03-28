#ifndef dynamicenvironment_h
#define dynamicenvironment_h

#include <cmath>

#include "MetaPDF_Lang_decls.h"
#include "MetaPDF_Kernel_decls.h"
#include "MetaPDF_Concrete_decls.h"
#include "MetaPDF_Ast_decls.h"

#include "statetypes.h"
#include "facettypes.h"
#include "texttypes.h"
#include "charptrless.h"


namespace MetaPDF
{
  namespace Kernel
  {
    
    class SystemDynamicVariables
    {
    public:
      SystemDynamicVariables( );
      SystemDynamicVariables( const RefCountPtr< const Kernel::GraphicsState > & graphicsState );
      
      void merge( const SystemDynamicVariables & other );
      
      RefCountPtr< const Kernel::GraphicsState > graphicsState_;
      RefCountPtr< const Kernel::FacetState > facetState_;
      RefCountPtr< const Kernel::TextState > textState_;
      Concrete::Length eyez_;
      RefCountPtr< const Kernel::PolarHandlePromise > defaultUnit_;
      RefCountPtr< const Lang::ColorSpace > blendSpace_;
    };
    
    class SpecialUnitVariables
    {
    public:
      bool reverseDirection_;
      const Concrete::PathPoint2D * p0_;
      const Concrete::PathPoint2D * p1_;
      
      void specialUnitService( Concrete::Length * d, double * a0, double * a1 );    
    };
    
    class DynamicEnvironment
    {
      RefCountPtr< Kernel::DynamicEnvironment > parent_;
    public:
      typedef DynamicEnvironmentKeyType KeyType;
      typedef Lang::DynamicBindings::MapType MapType;
    private:
      MapType bindings_;
      Kernel::SystemDynamicVariables * sysBindings_;
      Kernel::SpecialUnitVariables * specialBindings_;
      const char * contId_;
      Kernel::ContRef contVal_;
    public:
      DynamicEnvironment( const RefCountPtr< const Kernel::GraphicsState > & graphicsState );
      DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const Lang::DynamicBindings & bindings );
      DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const RefCountPtr< const Kernel::GraphicsState > & graphicsState );
      DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, Kernel::SystemDynamicVariables * sysBindings );
      DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, Kernel::SpecialUnitVariables * specialBindings );
      DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const char * contId, const Kernel::ContRef & contVal );
      ~DynamicEnvironment( );
      
      void tackOn( const KeyType & key, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc );
      void lookup( const KeyType & key, Kernel::EvalState * evalState ) const;
      Kernel::VariableHandle getVarHandle( const KeyType & key ) const;
      
      RefCountPtr< Kernel::DynamicEnvironment > selectParent( RefCountPtr< Kernel::DynamicEnvironment > & self, const MapType & newBindings );
      
      void gcMark( Kernel::GCMarkedSet & marked );
      
      RefCountPtr< const Kernel::GraphicsState > getGraphicsState( ) const;
      RefCountPtr< const Kernel::FacetState > getFacetState( ) const;
      RefCountPtr< const Kernel::TextState > getTextState( ) const;
      Concrete::Length getEyeZ( ) const;
      RefCountPtr< const Kernel::PolarHandlePromise > getDefaultUnit( ) const;
      Kernel::ContRef getEscapeContinuation( const char * id, const Ast::SourceLocation & loc ) const;
      RefCountPtr< const Lang::ColorSpace > getBlendSpace( ) const;
      
      void specialUnitService( Concrete::Length * d, double * a0, double * a1 );
      
      bool isBaseEnvironment( ) const;
      
    private:
      static KeyType nextKey;
    public:
      static KeyType getFreshKey( );
    };

  }

  namespace Lang
  {

    class DynamicExpression : public Lang::NoOperatorOverloadValue
    {
      Kernel::PassedEnv env_;
      Ast::Expression * expr_;
    public:
      DynamicExpression( Kernel::PassedEnv env, Ast::Expression * expr );
      virtual ~DynamicExpression( );
      void eval( Kernel::EvalState * evalState ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
      TYPEINFODECL;
    };
    


//     class DefaultDestinationBinding : public Lang::DynamicBindings
//     {
//       Ast::SourceLocation loc_;
//       Kernel::VariableHandle val_;
//     public:
//       DefaultDestinationBinding( const Ast::SourceLocation & loc, Kernel::VariableHandle & val );
//       virtual ~DefaultDestinationBinding( );
//       virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
//       virtual void gcMark( Kernel::GCMarkedSet & marked );
//     };
    
    class EyeZBinding : public Lang::DynamicBindings
    {
      Ast::SourceLocation loc_;
      Concrete::Length val_;
    public:
      EyeZBinding( const Ast::SourceLocation & loc, Concrete::Length val );
      virtual ~EyeZBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class DefaultUnitBinding : public Lang::DynamicBindings
    {
      Ast::SourceLocation loc_;
      RefCountPtr< const Kernel::PolarHandlePromise > val_;
    public:
      DefaultUnitBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Kernel::PolarHandlePromise > & val );
      virtual ~DefaultUnitBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class BlendSpaceBinding : public Lang::DynamicBindings
    {
      Ast::SourceLocation loc_;
      RefCountPtr< const Lang::ColorSpace > space_;
    public:
      BlendSpaceBinding( const Ast::SourceLocation & loc_, const RefCountPtr< const Lang::ColorSpace > & space );
      virtual ~BlendSpaceBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
  }

  namespace Kernel
  {

//     class DefaultDestinationDynamicVariableProperties : public Kernel::DynamicVariableProperties
//     {
//     public:
//       DefaultDestinationDynamicVariableProperties( const char * name );
//       virtual ~DefaultDestinationDynamicVariableProperties( );
//       virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
//       virtual bool forceValue( ) const { return false; };
//       virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
//     };

    class EyeZDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      EyeZDynamicVariableProperties( const char * name );
      virtual ~EyeZDynamicVariableProperties( );
      virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
      virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };

    class DefaultUnitDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      DefaultUnitDynamicVariableProperties( const char * name );
      virtual ~DefaultUnitDynamicVariableProperties( );
      virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
      virtual bool forceValue( ) const { return false; };
      virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };

    class BlendSpaceDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      BlendSpaceDynamicVariableProperties( const char * name );
      virtual ~BlendSpaceDynamicVariableProperties( );
      virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const;
      virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };

  }
}

#endif
