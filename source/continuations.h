#ifndef continuations_h
#define continuations_h

#include "metapdfast.h"

namespace MetaPDF
{
  namespace Helpers
  {

    template< class T >
    RefCountPtr< T >
    down_cast_ContinuationArgument( const RefCountPtr< const Lang::Value > & val, const Kernel::Continuation * locCont, bool voidIsNull = false )
    {
      RefCountPtr< T > res = val.down_cast< T >( );
      if( res == NullPtr< T >( ) )
	{
	  if( ! voidIsNull ||
	      dynamic_cast< const Lang::Void * >( val.getPtr( ) ) == 0 )
	    {
	      throw Exceptions::ContinuationTypeMismatch( locCont, val->getTypeName( ), T::staticTypeName( ) );
	    }
	}
      return res;
    }
    
  }

  namespace Kernel
  {

  class ExitContinuation : public Kernel::Continuation
  {
    bool * done_;
  public:
    ExitContinuation( bool * done, const Ast::SourceLocation & traceLoc );
    ~ExitContinuation( );
    virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class ExitVoidContinuation : public Kernel::Continuation
  {
    bool * done_;
  public:
    ExitVoidContinuation( bool * done, const Ast::SourceLocation & traceLoc );
    ~ExitVoidContinuation( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class IfContinuation : public Kernel::Continuation
  {
    Kernel::HandleType consequence_;
    Kernel::HandleType alternative_;
    Kernel::ContRef cont_;
  public:
    IfContinuation( const Kernel::HandleType & consequence, const Kernel::HandleType & alternative, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    ~IfContinuation( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  /* IntroduceColdContinuation is used by the expression IntroduceCold.
   */
  class IntroduceColdContinuation : public Kernel::Continuation
  {
    Kernel::PassedEnv env_;
    size_t * pos_;
    Kernel::ContRef cont_;
  public:
    IntroduceColdContinuation( const Kernel::PassedEnv & _env, size_t * _pos, const Kernel::ContRef & _cont, const Ast::SourceLocation & _traceLoc );
    ~IntroduceColdContinuation( );
    virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  /* IntroduceWarmContinuation is used by the expression IntroduceWarm.
   */
  class IntroduceWarmContinuation : public Kernel::Continuation
  {
    Kernel::PassedEnv env_;
    size_t * pos_;
    Kernel::ContRef cont_;
  public:
    IntroduceWarmContinuation( const Kernel::PassedEnv & _env, size_t * _pos, const Kernel::ContRef & _cont, const Ast::SourceLocation & _traceLoc );
    ~IntroduceWarmContinuation( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  /* StoreValueContinuation will store the returned object at the specified address, and then pass it on
   * to the next continuation.
   * Note that it is the responsibility of someone else to make sure that *res still exists when the continuation is invoked.
   * Generally, StoreVariableContinuation shall be preferred since it has no such problem.
   */
  class StoreValueContinuation : public Kernel::Continuation
  {
    Kernel::ValueRef * res_;
    Kernel::ContRef cont_;
  public:
    StoreValueContinuation( Kernel::ValueRef * res, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    ~StoreValueContinuation( );
    virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class StoreVariableContinuation : public Kernel::Continuation
  {
    Kernel::HandleType dst_;
    Kernel::ContRef cont_;
  public:
    StoreVariableContinuation( const Kernel::HandleType & res, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    ~StoreVariableContinuation( );
    virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  /* The InsertionContinuation sends the returned value to a warm variable, and passes THE_SLOT_VARIABLE (a null value) to the next continuation.
   */
  class InsertionContinuation : public Kernel::Continuation
  {
    mutable Kernel::HandleType dst_; /* This being mutable is actually quite ugly... */
    Kernel::PassedDyn dyn_;
    Kernel::ContRef cont_;
  public:
    InsertionContinuation( const Kernel::HandleType & dst, const Kernel::ContRef & cont, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & traceLoc );
    ~InsertionContinuation( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  /* StmtStoreValueContinuation is like StoreValueContinuation, except that it passes THE_SLOT_VARIABLE (a null value) to the next continuation.
   */
  class StmtStoreValueContinuation : public Kernel::Continuation
  {
    Kernel::ValueRef * res_;
    Kernel::ContRef cont_;
  public:
    StmtStoreValueContinuation( Kernel::ValueRef * res, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    ~StmtStoreValueContinuation( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class StmtStoreVariableContinuation : public Kernel::Continuation
  {
    Kernel::HandleType dst_;
    Kernel::ContRef cont_;
  public:
    StmtStoreVariableContinuation( const Kernel::HandleType & res, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    ~StmtStoreVariableContinuation( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class ForcingContinuation : public Kernel::Continuation
  {
    Kernel::ContRef cont_;
  public:
    ForcingContinuation( const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    ~ForcingContinuation( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class Transform2DCont : public Kernel::Continuation
  {
    Lang::Transform2D tf_;
    Kernel::ContRef cont_;
  public:
    Transform2DCont( Lang::Transform2D tf, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    virtual ~Transform2DCont( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class Transform3DCont : public Kernel::Continuation
  {
    Lang::Transform3D tf_;
    Kernel::ContRef cont_;
  public:
    Transform3DCont( Lang::Transform3D tf, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    virtual ~Transform3DCont( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class PathApplication2DCont : public Kernel::Continuation
  {
    RefCountPtr< const Lang::ElementaryPath2D > path_;
    Kernel::ContRef cont_;
  public:
    PathApplication2DCont( RefCountPtr< const Lang::ElementaryPath2D > path, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    virtual ~PathApplication2DCont( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class PathApplication3DCont : public Kernel::Continuation
  {
    RefCountPtr< const Lang::ElementaryPath3D > path_;
    Kernel::ContRef cont_;
  public:
    PathApplication3DCont( RefCountPtr< const Lang::ElementaryPath3D > path, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
    virtual ~PathApplication3DCont( );
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  class ComposedFunctionCall_cont : public Kernel::Continuation
  {
    RefCountPtr< const Lang::Function > second_;
    Kernel::PassedDyn dyn_;
    Kernel::ContRef cont_;
  public:
    ComposedFunctionCall_cont( const RefCountPtr< const Lang::Function > & second, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc );
    virtual ~ComposedFunctionCall_cont( );
    virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const;
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
    virtual void gcMark( Kernel::GCMarkedSet & marked );
  };

  }

}


#endif
