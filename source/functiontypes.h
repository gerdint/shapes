#ifndef functiontypes_h
#define functiontypes_h

#include "MetaPDF_Lang_decls.h"
#include "MetaPDF_Kernel_decls.h"
#include "MetaPDF_Ast_decls.h"

#include "metapdfvalue.h"
#include "ptrowner.h"
#include "refcount.h"
#include "pdfstructure.h"
#include "environment.h"
#include "charptrless.h"
#include "consts.h"
#include "elementarytypes.h"
#include "elementarycoords.h"

#include <list>
#include <iostream>
#include <stack>
#include <set>
#include <gsl/gsl_matrix.h>


#define CHECK_ARITY( argsVar, arity, titleExpr )	\
  if( argsVar.size( ) != ARITY )\
    {\
      throw Exceptions::CoreArityMismatch( titleExpr, arity, argsVar.size( ) );\
    }



namespace MetaPDF
{

  namespace Kernel
  {
    
    class Arguments
    {
      const Kernel::EvaluatedFormals * formals_;
      Environment::ValueVector variables_;
      std::vector< const Ast::Node * > locations_;
      size_t dst_;
      bool hasSink_;
      // Putting dstEnd_ after hasSink_ makes initialization more convenient.
      size_t dstEnd_; // This is one less than there are variables if there is a sink.
      bool isSink_; // This is used by functions in the core.

      Ast::ArgListExprs * sinkArgList_; // If null, there is no sink.
      RefCountPtr< const Lang::SingleList > sinkValues_;

      Environment::StateVector states_;   // This type must match that used in Environment
      std::vector< const Ast::Node * > stateLocations_;
      size_t stateDst_;
      
    public:
      Arguments( const Kernel::EvaluatedFormals * formals );
      ~Arguments( );
      
      Kernel::Arguments clone( ) const;
      
      void addOrderedArgument( const Kernel::VariableHandle & arg, Ast::Expression * loc );
      void addNamedArgument( const char * id, const Kernel::VariableHandle & arg, Ast::Expression * loc );

      void addOrderedState( const Kernel::StateHandle & state, Ast::Node * loc );
      void addNamedState( const char * id, const Kernel::StateHandle & state, Ast::Node * loc );
      
      void applyDefaults( );
      
      Kernel::VariableHandle & getHandle( size_t i );
      RefCountPtr< const Lang::Value > & getValue( size_t i );
      const Ast::SourceLocation & getLoc( size_t i ) const;
      const Ast::Node * getNode( size_t i ) const;
      Kernel::Thunk * getThunk( size_t i );                        //  This funciton returns a newly created copy!
      bool isSlot( size_t i ) const;
      
      size_t size( ) const;

      Kernel::StateHandle getState( size_t i );
      const Ast::SourceLocation & getStateLoc( size_t i ) const;
      
      void gcMark( Kernel::GCMarkedSet & marked );
      
      Environment::ValueVector getVariables( ); // This function should only be called when setting up a new environment
      Environment::StateVector getStates( ); // This function should only be called when setting up a new environment
    };

    class EvaluatedFormals
    {
      bool selectiveForcing_;
      bool forceAll_;
    public:
      Kernel::Formals * formals_;                     /* it would have been const if it was not for appendEvaluatedFormal */
      std::vector< Kernel::VariableHandle > defaults_;
      std::vector< const Ast::Node * > locations_;
      bool isSink_;
      
      EvaluatedFormals( Kernel::Formals * _formals );
      EvaluatedFormals( const char * _locationString );
      EvaluatedFormals( const char * _locationString, bool _forceAll );
      ~EvaluatedFormals( );
      
      void appendEvaluatedFormal( const char * id, const Kernel::VariableHandle & defaultVal, const Ast::Node * loc, bool force );
      void appendEvaluatedFormal( const char * id, const Kernel::VariableHandle & defaultVal, const Ast::Node * loc );
      void appendEvaluatedCoreFormal( const char * id, const Kernel::VariableHandle & defaultVal, bool force );
      void appendEvaluatedCoreFormal( const char * id, const Kernel::VariableHandle & defaultVal );
      void appendCoreStateFormal( const char * id );
      
      RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const;
      RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const;
      
      void gcMark( Kernel::GCMarkedSet & marked );    
    };

  }


  namespace Helpers
  {

    template< class T >
    RefCountPtr< T >
    down_cast_CoreArgument( const char * coreTitle, Kernel::Arguments & args, size_t i, const Ast::SourceLocation & callLoc, bool voidIsNull = false )
    {
      RefCountPtr< const Lang::Value > val = args.getValue( i );
      RefCountPtr< T > res = val.down_cast< T >( );
      if( res == NullPtr< T >( ) )
	{
	  if( ! voidIsNull ||
	      dynamic_cast< const Lang::Void * >( val.getPtr( ) ) == 0 )
	    {
	      throw Exceptions::CoreTypeMismatch( callLoc, coreTitle, args.getLoc( i ), val->getTypeName( ), T::staticTypeName( ) );
	    }
	}
      return res;
    }

    template< class T >
    T *
    down_cast_CoreState( const char * coreTitle, Kernel::Arguments & args, size_t i, const Ast::SourceLocation & callLoc, bool voidIsNull = false )
    {
      Kernel::StateHandle st = args.getState( i );
      T * res = dynamic_cast< T * >( st );
      if( res == NullPtr< T >( ) )
	{
	  throw Exceptions::CoreStateTypeMismatch( callLoc, coreTitle, args.getStateLoc( i ), st->getTypeName( ), T::staticTypeName( ) );
	}
      return res;
    }

    template< class T >
    RefCountPtr< T >
      down_cast_CoreDynamic( const char * coreTitle, const char * id, const RefCountPtr< const Lang::Value > & val, const Ast::SourceLocation & callLoc )
      {
	RefCountPtr< T > res = val.down_cast< T >( );
	if( res == NullPtr< T >( ) )
	  {
	    throw Exceptions::CoreDynamicTypeMismatch( callLoc, coreTitle, id, val->getTypeName( ), T::staticTypeName( ) );
	  }
	return res;
      }
    
    template< class T >
    RefCountPtr< T >
    try_cast_CoreArgument( const RefCountPtr< const Lang::Value > & val, bool voidIsNull = false )
    {
      RefCountPtr< T > res = val.down_cast< T >( );
      if( res == NullPtr< T >( ) )
	{
	  if( ! voidIsNull ||
	      dynamic_cast< const Lang::Void * >( val.getPtr( ) ) == 0 )
	    {
	      throw NonLocalExit::NotThisType( );
	    }
	}
      return res;
    }

  }    


  namespace Lang
  {

    class Transform2D : public Lang::Value
    {
    public:
      double xx_;
      double yx_;
      double xy_;
      double yy_;
      Concrete::Length xt_;
      Concrete::Length yt_;
      Transform2D( double xx, double yx, double xy, double yy,
		   Concrete::Length xt, Concrete::Length yt );
      Transform2D( const Lang::Transform2D & tf2, const Lang::Transform2D & tf1 );
      bool isIdentity( ) const;
      bool isTranslation( ) const;
      void shipout( std::ostream & os ) const;
      void replaceBy( const Lang::Transform2D & newtf );     // to be used by text moveto commands
      void prependShift( const Concrete::Coords2D & d );     // to be used by text newline commands
      void prependXShift( const Concrete::Length & dx );     // to be used by text painting commands
      TYPEINFODECL;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual void show( std::ostream & os ) const;
      DISPATCHDECL;
    };
    
    class Transform3D : public Lang::Value
    {
      static const int N = 3;
      mutable gsl_matrix * planeNormalTransformData_;
    public:
      double xx_;
      double yx_;
      double zx_;
      double xy_;
      double yy_;
      double zy_;
      double xz_;
      double yz_;
      double zz_;
      Concrete::Length xt_;
      Concrete::Length yt_;
      Concrete::Length zt_;
      Transform3D( double xx, double yx, double zx, double xy, double yy, double zy, double xz, double yz, double zz,
		   Concrete::Length xt, Concrete::Length yt, Concrete::Length zt );
      Transform3D( const Lang::Transform3D & tf2, const Lang::Transform3D & tf1 );
      virtual ~Transform3D( );
      bool isIdentity( ) const;
      Concrete::UnitFloatTriple transformPlaneUnitNormal( const Concrete::UnitFloatTriple & n ) const;
      TYPEINFODECL;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual void show( std::ostream & os ) const;
      DISPATCHDECL;
    };

    class Function : public Lang::Value
    {
    protected:
      Kernel::EvaluatedFormals * formals_;  // the reason that this is not const is only to allow convenient setup
    public:
      Function( Kernel::EvaluatedFormals * formals );
      virtual ~Function( );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const = 0;
      virtual Kernel::ValueRef transformed( const Lang::Transform2D & tf, Kernel::ValueRef self ) const;
      virtual bool isTransforming( ) const = 0;
      virtual bool isProcedural( ) const;
      virtual RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const;
      virtual RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const;
      virtual Kernel::Arguments newCurriedArguments( ) const;
      virtual void call( Kernel::EvalState * evalState, const Kernel::ValueRef & arg1, const Ast::SourceLocation & callLoc ) const;
      virtual void call( Kernel::EvalState * evalState, const Kernel::ValueRef & arg1, const Kernel::ValueRef & arg2, const Ast::SourceLocation & callLoc ) const;
      virtual void call( const RefCountPtr< const Lang::Function > & selfRef, Kernel::EvalState * evalState, const Kernel::VariableHandle & arg1, const Ast::SourceLocation & callLoc ) const;
      virtual void call( const RefCountPtr< const Lang::Function > & selfRef, Kernel::EvalState * evalState, const Kernel::VariableHandle & arg1, const Kernel::VariableHandle & arg2, const Ast::SourceLocation & callLoc ) const;
      
      TYPEINFODECL;
      DISPATCHDECL;
    private:
      static Ast::ArgListExprs * oneExprArgList;
      static Ast::ArgListExprs * twoExprsArgList;
    };
  
    class CuteFunction : public Lang::Function
    {
      RefCountPtr< const Lang::Function > callee_;
      Kernel::Arguments someArgs_;
    public:
      CuteFunction( RefCountPtr< const Lang::Function > _callee, const Kernel::Arguments & _someArgs );
      virtual ~CuteFunction( );
      virtual RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const;
      virtual RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const;
      virtual Kernel::Arguments newCurriedArguments( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const;
    };

    class ComposedFunction : public Lang::Function
    {
      RefCountPtr< const Lang::Function > second_;
      RefCountPtr< const Lang::Function > first_;
    public:
      ComposedFunction( const RefCountPtr< const Lang::Function > & _second, const RefCountPtr< const Lang::Function > & _first );
      virtual ~ComposedFunction( );
      virtual RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState ) const;
      virtual RefCountPtr< Kernel::CallContInfo > newCallContInfo( const Ast::ArgListExprs * argList, const Kernel::EvalState & evalState, const Kernel::Arguments & curryArgs ) const;
      virtual Kernel::Arguments newCurriedArguments( ) const;
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

    class UserFunction : public Lang::Function
    {
      Ast::Expression * body_;
      Kernel::PassedEnv env_;
      Ast::FunctionMode functionMode_;
    public:
      UserFunction( Kernel::EvaluatedFormals * _formals, Ast::Expression * _body, Kernel::PassedEnv _env, const Ast::FunctionMode & _functionMode );
      virtual ~UserFunction( );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const;
      virtual bool isProcedural( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
    };

    class TransformedFunction2D : public Lang::Function
    {
      Lang::Transform2D tf_;
      RefCountPtr< const Lang::Function > fun_;
    public:
      TransformedFunction2D( const Lang::Transform2D & _tf, const RefCountPtr< const Lang::Function > & _fun );
      virtual ~TransformedFunction2D( );
      virtual void gcMark( Kernel::GCMarkedSet & marked );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const;
    };

    class VectorFunction : public Lang::Function
    {
      RefCountPtr< const std::vector< Kernel::ValueRef > > mem_;
      mutable RefCountPtr< const std::vector< double > > memNumeric_;
      static const char * title_;
    public:
      VectorFunction( const std::vector< Kernel::ValueRef > * mem );
      virtual ~VectorFunction( );
      virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const;
      RefCountPtr< const std::vector< double > > getNumeric( const Ast::SourceLocation & callLoc ) const;
    };

    class BinaryOperatorFunction : public Lang::Function
    {
      Ast::BinaryInfixExpr * opExpr_;
      const char * title_;
    public:
      BinaryOperatorFunction( Ast::BinaryInfixExpr * opExpr, const char * title );
      virtual ~BinaryOperatorFunction( );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
    };

    class UnaryOperatorFunction : public Lang::Function
    {
      Ast::UnaryExpr * opExpr_;
      const char * title_;
    public:
      UnaryOperatorFunction( Ast::UnaryExpr * opExpr, const char * title );
      virtual ~UnaryOperatorFunction( );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual bool isTransforming( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
    };

  }
}

#endif
