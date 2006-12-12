#ifndef facettypes_h
#define facettypes_h

#include "MetaPDF_Ast_decls.h"
#include "MetaPDF_Kernel_decls.h"
#include "MetaPDF_Lang_decls.h"
#include "MetaPDF_Computation_decls.h"

#include "ptrowner.h"
#include "refcount.h"
#include "environment.h"
#include "statetypes.h"
#include "elementarylength.h"
#include "concretecolors.h"

#include <list>
#include <iostream>
#include <stack>
#include <set>


namespace MetaPDF
{
  namespace Lang
  {

    class ReflectionsBinding : public DynamicBindings
    {
      Ast::SourceLocation loc_;
      RefCountPtr< const Lang::SpecularReflection > reflections_;
    public:
      ReflectionsBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::SpecularReflection > & reflections );
      virtual ~ReflectionsBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class AutoIntensityBinding : public DynamicBindings
    {
      Ast::SourceLocation loc_;
      RefCountPtr< const Lang::Color > color_;
    public:
      AutoIntensityBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::Color > & color );
      virtual ~AutoIntensityBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

    class AutoScatteringBinding : public DynamicBindings
    {
      Ast::SourceLocation loc_;
      RefCountPtr< const Lang::SpecularReflection > reflections_;
    public:
      AutoScatteringBinding( const Ast::SourceLocation & loc, const RefCountPtr< const Lang::SpecularReflection > & reflections );
      virtual ~AutoScatteringBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class ViewResolutionBinding : public DynamicBindings
    {
      Ast::SourceLocation loc_;
      Concrete::Length resolution_;
    public:
      ViewResolutionBinding( const Ast::SourceLocation & loc, const Concrete::Length resolution );
      virtual ~ViewResolutionBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
    class ShadeOrderBinding : public DynamicBindings
    {
      Ast::SourceLocation loc_;
      Computation::FacetShadeOrder order_;
    public:
      ShadeOrderBinding( const Ast::SourceLocation & loc, const Computation::FacetShadeOrder order );
      virtual ~ShadeOrderBinding( );
      virtual void bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };
    
  }

  namespace Kernel
  {

    class ReflectionsDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      ReflectionsDynamicVariableProperties( const char * name );
      virtual ~ReflectionsDynamicVariableProperties( );
      virtual Kernel::HandleType fetch( const Kernel::PassedDyn & dyn ) const;
      virtual void makeBinding( Kernel::HandleType val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };
    
    class AutoIntensityDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      AutoIntensityDynamicVariableProperties( const char * name );
      virtual ~AutoIntensityDynamicVariableProperties( );
      virtual Kernel::HandleType fetch( const Kernel::PassedDyn & dyn ) const;
      virtual void makeBinding( Kernel::HandleType val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };
    
    class AutoScatteringDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      AutoScatteringDynamicVariableProperties( const char * name );
      virtual ~AutoScatteringDynamicVariableProperties( );
      virtual Kernel::HandleType fetch( const Kernel::PassedDyn & dyn ) const;
      virtual void makeBinding( Kernel::HandleType val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };
    
    class ViewResolutionDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      ViewResolutionDynamicVariableProperties( const char * name );
      virtual ~ViewResolutionDynamicVariableProperties( );
      virtual Kernel::HandleType fetch( const Kernel::PassedDyn & dyn ) const;
      virtual void makeBinding( Kernel::HandleType val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };
    
    class ShadeOrderDynamicVariableProperties : public Kernel::DynamicVariableProperties
    {
    public:
      ShadeOrderDynamicVariableProperties( const char * name );
      virtual ~ShadeOrderDynamicVariableProperties( );
      virtual Kernel::HandleType fetch( const Kernel::PassedDyn & dyn ) const;
      virtual void makeBinding( Kernel::HandleType val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const;
    };
    
    class FacetState
    {
    public:
      RefCountPtr< const Lang::SpecularReflection > reflections_;     // Use SpecularReflectionNumm for no-op, and NullPtr for undefined
      RefCountPtr< const Lang::Color > autoIntensity_;                // Use Gray( -1 ) for no-op, and NullPtr for undefined
      RefCountPtr< const Lang::SpecularReflection > autoScattering_;  // Use SpecularReflectionNumm for no-op, and NullPtr for undefined
      Concrete::Length viewResolution_;                               // Use NaN for undefined
      Computation::FacetShadeOrder shadeOrder_;                       // Use -1 for undefined
    public:
      FacetState( );
      explicit FacetState( const Kernel::FacetState & orig );   // explicit, since reference counting shall be used in most cases
      FacetState( const Kernel::FacetState & newValues, const Kernel::FacetState & oldValues );
      FacetState( bool setDefaults );    
      ~FacetState( );
    };

  }

}

#endif
