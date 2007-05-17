#ifndef environment_h
#define environment_h

#include <cstring>
#include <climits>

#include "MetaPDF_Kernel_decls.h"
#include "MetaPDF_Lang_decls.h"
#include "MetaPDF_Ast_decls.h"

#include "refcount.h"
#include "charptrless.h"
#include "ptrowner.h"
#include "sourcelocation.h"
#include "metapdfexceptions.h"
#include "metapdfvalue.h"
// #include "dynamicenvironment.h"

#include <map>
#include <set>
#include <vector>


namespace MetaPDF
{

  namespace Computation
  {

    enum SpecialUnit { SPECIALU_DIST = 0x0001, SPECIALU_CIRC = 0x0002, SPECIALU_CORR = 0x0004, SPECIALU_NOINFLEX = 0x0008 };

  }

  namespace Kernel
  {

    class Thunk
    {
      Kernel::PassedEnv env_;
      Kernel::PassedDyn dyn_;
      Ast::Expression * expr_;
      mutable bool forced_;
    public:
      Thunk( Kernel::PassedEnv _env, Kernel::PassedDyn _dyn, Ast::Expression * _expr );
      void force( Kernel::EvalState * evalState, bool onlyOnce = true ) const;
      Thunk * deepCopy( );
      void gcMark( Kernel::GCMarkedSet & marked );
      Ast::Expression * getExpr( );
      void printEnv( std::ostream & os ) const;
    };
    
    /* Although there are several kinds of variables, I don't use c++ polymorphism for efficiency reasons.
     */
    class Variable
    {
      static const unsigned char THUNK =    0x03;
      static const unsigned char FORCING =  0x02;
      static const unsigned char COLD =     0x00;
      mutable Kernel::Thunk * thunk_;                     /* thunk != 0 means that the thunk waits to be forced */
      mutable RefCountPtr< const Lang::Value > val_;
      mutable unsigned char state_;
    private:
      Variable( const Kernel::Variable & orig );
    public:
      Variable( const RefCountPtr< const Lang::Value > & _val );
      Variable( Kernel::Thunk * _thunk );
      ~Variable( );
      void force( Kernel::VariableHandle & selfRef, Kernel::EvalState * evalState ) const;
      RefCountPtr< const Lang::Value > & getUntyped( ) const;
      template< class T >
	RefCountPtr< T > getVal( ) const
	{
	  if( val_ == NullPtr< const Lang::Value >( ) )
	    {
	      throw Exceptions::InternalError( strrefdup( "The value is not ready to be get without continuation." ) );
	    }
	  RefCountPtr< T > res = val_.down_cast< T >( );
	  if( res == NullPtr< T >( ) )
	    {
	      throw Exceptions::TypeMismatch( val_->getTypeName( ), T::staticTypeName( ) );
	    }
	  return res;
	}
      template< class T >
	RefCountPtr< T > getVal( const char * hint ) const
	{
	  if( val_ == NullPtr< const Lang::Value >( ) )
	    {
	      throw Exceptions::InternalError( strrefdup( "The value is not ready to be get without continuation." ) );
	    }
	  RefCountPtr< T > res = val_.down_cast< T >( );
	  if( res == NullPtr< T >( ) )
	    {
	      throw Exceptions::TypeMismatch( Ast::THE_UNKNOWN_LOCATION, hint, val_->getTypeName( ), T::staticTypeName( ) );
	    }
	  return res;
	}
      template< class T >
	RefCountPtr< T > getVal( const Ast::SourceLocation & loc ) const
	{
	  if( val_ == NullPtr< const Lang::Value >( ) )
	    {
	      throw Exceptions::InternalError( strrefdup( "The value is not ready to be get without continuation." ) );
	    }
	  RefCountPtr< T > res = val_.down_cast< T >( );
	  if( res == NullPtr< T >( ) )
	    {
	      throw Exceptions::TypeMismatch( loc, val_->getTypeName( ), T::staticTypeName( ) );
	    }
	  return res;
	}
      template< class T >
	RefCountPtr< T > tryVal( ) const
	{
	  if( val_ == NullPtr< const Lang::Value >( ) )
	    {
	      throw Exceptions::InternalError( strrefdup( "The value is not ready to be get without continuation." ) );
	    }
	  RefCountPtr< T > res = val_.down_cast< T >( );
	  if( res == NullPtr< T >( ) )
	    {
	      throw NonLocalExit::NotThisType( );
	    }
	  return res;
	}
      bool isThunk( ) const;
      Kernel::Thunk * copyThunk( ) const;
      void gcMark( Kernel::GCMarkedSet & marked );
    private:
      void setValue( const RefCountPtr< const Lang::Value > & _val ) const;
      friend class Kernel::StoreVariableContinuation;
      friend class Kernel::StmtStoreVariableContinuation;
    };

    class State
    {
      bool alive_;

    private:
      State( const Kernel::State & orig );
    public:
      State( );
      virtual ~State( );
      void tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
      void peek( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
      void freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );

      virtual const RefCountPtr< const ::MetaPDF::Lang::Class > & getClass( ) const = 0;
      RefCountPtr< const char > getTypeName( ) const;
      bool isAlive( ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ) = 0;

    protected:
      virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc ) = 0;
      virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc ) = 0;
      virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc ) = 0;
    };

    class DynamicVariableProperties
    {
    protected:
      const char * name_;
    public:
    DynamicVariableProperties( const char * name )
      : name_( name )
      { }
      virtual ~DynamicVariableProperties( );
      const char * getName( ) const { return name_; }
      virtual Kernel::VariableHandle fetch( const Kernel::PassedDyn & dyn ) const = 0;
      virtual bool forceValue( ) const { return true; };
      virtual void makeBinding( Kernel::VariableHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const = 0;
    };

    class DynamicStateProperties
    {
    protected:
      const char * name_;
    public:
    DynamicStateProperties( const char * name )
      : name_( name )
      { }
      virtual ~DynamicStateProperties( );
      const char * getName( ) const { return name_; }
      virtual Kernel::StateHandle fetch( const Kernel::PassedDyn & dyn ) const = 0;
      virtual void makeBinding( Kernel::StateHandle val, Ast::SourceLocation loc, Kernel::EvalState * evalState ) const = 0;
    };

    class Environment
    {
    public:
      class LexicalKey
      {
      public:
	size_t up_;
	size_t pos_;
      LexicalKey( size_t up, size_t pos )
	: up_( up ), pos_( pos )
	{ };
	LexicalKey & oneAbove( )
	  {
	    ++up_;
	    return *this;
	  }
      };
      typedef RefCountPtr< std::vector< VariableHandle > > ValueVector;
      typedef RefCountPtr< std::vector< StateHandle > > StateVector;
    private:
      Environment * parent_;
      typedef std::map< const char *, size_t, charPtrLess > MapType; /* this is not constant to simplify initialization of the global environment */

      MapType * bindings_;
      ValueVector values_;
      MapType * dynamicKeyBindings_;
      std::vector< DynamicVariableProperties * > * dynamicKeyValues_;

      MapType * stateBindings_;
      StateVector states_;
      MapType * dynamicStateKeyBindings_;
      std::vector< DynamicStateProperties * > * dynamicStateKeyValues_;

      PtrOwner_back_Access< std::list< const char * > > charPtrDeletionList_;
      bool functionBoundary_;

      void selfDefineCoreFunction( Lang::CoreFunction * fun );
      void selfDefineCoreFunction( RefCountPtr< const Lang::CoreFunction > fun );
      void selfDefineHandle( const char * id, const Kernel::VariableHandle & val );
      void selfDefine( const char * id, const RefCountPtr< const Lang::Value > & val );
      void selfDefine( const char * id, Kernel::StateHandle state );
      void selfDefineClass( const RefCountPtr< const Lang::Class > & cls );

      void selfDefineDynamic( DynamicVariableProperties * dynProps );
      void selfDefineDynamic( const char * id, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal );
      void selfDefineDynamicHandler( const char * id, const char * msg );

      bool gcMarked_;

    public:
      Environment( std::list< Kernel::Environment * > & garbageArea );
      Environment( std::list< Kernel::Environment * > & garbageArea, Environment * _parent, MapType * _bindings, const ValueVector & _values, MapType * _stateBindings, const StateVector & states );
      ~Environment( );
      void setParent( Environment * _parent );
      void setupDynamicKeyVariables( MapType * _dynamicKeyBindings );
      void setupDynamicStateKeyVariables( MapType * _dynamicStateKeyBindings );
      void activateFunctionBoundary( );
      Kernel::Environment * getParent( );
      const Kernel::Environment * getParent( ) const;
      void clear( );

      void clear_gcMarked( ) { gcMarked_ = false; }
      bool gcMarked( ) const { return gcMarked_; }
      void gcMark( Kernel::GCMarkedSet & marked );

      static void collect( std::list< Kernel::Environment * > & garbageArea );

      size_t findLocalVariablePosition( const Ast::SourceLocation & loc, const char * id ) const;
      LexicalKey findLexicalVariableKey( const Ast::SourceLocation & loc, const char * id ) const;
      void define( size_t pos, const Kernel::VariableHandle & val );
      void lookup( const LexicalKey & lexKey, Kernel::EvalState * evalState ) const;
      void lookup( size_t pos, Kernel::EvalState * evalState ) const;
      VariableHandle getVarHandle( const LexicalKey & lexKey );
      VariableHandle getVarHandle( size_t pos );

      // These only differ from those above by the error messages they may generate.
      LexicalKey findLexicalTypeKey( const Ast::SourceLocation & loc, const char * id ) const;

      size_t findLocalStatePosition( const Ast::SourceLocation & loc, const char * id ) const;
      LexicalKey findLexicalStateKey( const Ast::SourceLocation & loc, const char * id ) const;
      void introduceState( size_t pos, Kernel::State * state );
      void freeze( size_t pos, Kernel::EvalState * evalState, const Ast::SourceLocation & loc );
      void peek( const LexicalKey & lexKey, Kernel::EvalState * evalState, const Ast::SourceLocation & loc );
      void tackOn( const LexicalKey & lexKey, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc );
      StateHandle getStateHandle( const LexicalKey & lexKey );
      StateHandle getStateHandle( size_t pos );

      size_t findLocalDynamicPosition( const Ast::SourceLocation & loc, const char * id ) const;
      void defineDynamic( const char * debugName, size_t pos, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal );

      LexicalKey findLexicalDynamicKey( const Ast::SourceLocation & loc, const char * id ) const;
      const DynamicVariableProperties & lookupDynamicVariable( const LexicalKey & lexKey ) const;
      const DynamicVariableProperties & lookupDynamicVariable( size_t pos ) const;


      size_t findLocalDynamicStatePosition( const Ast::SourceLocation & loc, const char * id ) const;
      void defineDynamicState( const char * debugName, size_t pos, Kernel::EvalState * evalState, Ast::StateReference * defaultState );
      
      LexicalKey findLexicalDynamicStateKey( const Ast::SourceLocation & loc, const char * id ) const;
      const DynamicStateProperties & lookupDynamicState( const LexicalKey & lexKey ) const;
      const DynamicStateProperties & lookupDynamicState( size_t pos ) const;


      size_t size( ) const;
    
      static size_t createdCount;
      static size_t liveCount;

      void print( std::ostream & os ) const;
      size_t recursivePrint( std::ostream & os, std::set< MapType::key_type > * shadowed ) const;

      bool isBaseEnvironment( ) const { return parent_ == 0; };

    private:
      const char * reverseMapVariable( size_t pos ) const;
      const char * reverseMapDynamic( size_t pos ) const;
      const char * reverseMapState( size_t pos ) const;
      const char * reverseMapDynamicState( size_t pos ) const;
    };

  }

}

#endif
