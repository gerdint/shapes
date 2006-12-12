#include "metapdftypes.h"
#include "metapdfexceptions.h"
#include "metapdfastexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "metapdfastvar.h"
#include "metapdfastclass.h"
#include "globals.h"

using namespace MetaPDF;


Lang::SingleList::SingleList( )
{ }

Lang::SingleList::~SingleList( )
{ }

RefCountPtr< const Lang::Class > Lang::SingleList::TypeID( new Lang::SystemFinalClass( strrefdup( "SingleList" ) ) );
TYPEINFOIMPL( SingleList );

Kernel::HandleType
Lang::SingleList::getField( const char * fieldId, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  RefCountPtr< const Lang::SingleList > typedSelfRef = Helpers::down_cast< const Lang::SingleList >( selfRef, "< SingleList::getField >" );
  if( strcmp( fieldId, "foldl" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::SingleListMethodFoldL( typedSelfRef ) );
    }
  if( strcmp( fieldId, "foldr" ) == 0 )
    {
      return Helpers::newValHandle( new Lang::SingleListMethodFoldR( typedSelfRef ) );
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldId );
}


namespace MetaPDF
{
  namespace Kernel
  {
  
  class SingleFoldLCont : public Kernel::Continuation
  {
    RefCountPtr< const Lang::SingleList > cdr_;
    RefCountPtr< const Lang::Function > op_;
    Kernel::PassedDyn dyn_;
    Kernel::ContRef cont_;
  public:
    SingleFoldLCont( const RefCountPtr< const Lang::SingleList > & cdr, const RefCountPtr< const Lang::Function > & op, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
      : Kernel::Continuation( traceLoc ), cdr_( cdr ), op_( op ), dyn_( dyn ), cont_( cont )
    { }
    virtual ~SingleFoldLCont( ) { }
    virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const
    {
      evalState->dyn_ = dyn_;
      evalState->cont_ = cont_;
      cdr_->foldl( evalState, op_, val, traceLoc_ );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "singly linked list's foldl" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      const_cast< Lang::SingleList * >( cdr_.getPtr( ) )->gcMark( marked );
      const_cast< Lang::Function * >( op_.getPtr( ) )->gcMark( marked );
      dyn_->gcMark( marked );
      cont_->gcMark( marked );
    }
  };

  class SingleFoldRCont : public Kernel::Continuation
  {
    Kernel::HandleType car_;
    RefCountPtr< const Lang::Function > op_;
    Kernel::PassedDyn dyn_;
    Kernel::ContRef cont_;
  public:
    SingleFoldRCont( const Kernel::HandleType & car, const RefCountPtr< const Lang::Function > & op, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
      : Kernel::Continuation( traceLoc ), car_( car ), op_( op ), dyn_( dyn ), cont_( cont )
    { }
    virtual ~SingleFoldRCont( ) { }
    virtual void takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool dummy ) const
    {
      evalState->dyn_ = dyn_;
      evalState->cont_ = cont_;
      op_->call( op_, evalState, val, car_, traceLoc_ );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "singly linked list's foldr" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      car_->gcMark( marked );
      const_cast< Lang::Function * >( op_.getPtr( ) )->gcMark( marked );
      dyn_->gcMark( marked );
      cont_->gcMark( marked );
    }
  };

  }
}


Lang::SingleListPair::SingleListPair( const Kernel::HandleType & car, const RefCountPtr< const Lang::SingleList > & cdr )
  : car_( car ), cdr_( cdr )
{ }

Lang::SingleListPair::~SingleListPair( )
{ }

bool
Lang::SingleListPair::isNull( ) const
{
  return false;
}

void
Lang::SingleListPair::foldl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::HandleType & nullResult, const Ast::SourceLocation & callLoc ) const
{
  evalState->cont_ = Kernel::ContRef( new Kernel::SingleFoldLCont( cdr_, op, evalState->dyn_, evalState->cont_, callLoc ) );

  op->call( op, evalState, nullResult, car_, callLoc );
}

void
Lang::SingleListPair::foldr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::HandleType & nullResult, const Ast::SourceLocation & callLoc ) const
{
  evalState->cont_ = Kernel::ContRef( new Kernel::SingleFoldRCont( car_, op, evalState->dyn_, evalState->cont_, callLoc ) );

  cdr_->foldr( evalState, op, nullResult, callLoc );
}

void
Lang::SingleListPair::gcMark( Kernel::GCMarkedSet & marked )
{
  car_->gcMark( marked );
  const_cast< Lang::SingleList * >( cdr_.getPtr( ) )->gcMark( marked );
}

Lang::SingleListNull::SingleListNull( )
{ }

Lang::SingleListNull::~SingleListNull( )
{ }

bool
Lang::SingleListNull::isNull( ) const
{
  return true;
}

void
Lang::SingleListNull::foldl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::HandleType & nullResult, const Ast::SourceLocation & callLoc ) const
{
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( nullResult,
		    evalState );
}

void
Lang::SingleListNull::foldr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::HandleType & nullResult, const Ast::SourceLocation & callLoc ) const
{
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( nullResult,
		    evalState );
}


Lang::ConsPair::ConsPair( const Kernel::HandleType & car, const Kernel::HandleType & cdr )
  : car_( car ), cdr_( cdr )
{ }

Lang::ConsPair::~ConsPair( )
{ }

RefCountPtr< const Lang::Class > Lang::ConsPair::TypeID( new Lang::SystemFinalClass( strrefdup( "ConsPair" ) ) );
TYPEINFOIMPL( ConsPair );

void
Lang::ConsPair::show( std::ostream & os ) const
{
  os << "< a lazy pair >" ;
}

Kernel::HandleType
Lang::ConsPair::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
  if( strcmp( fieldID, "car" ) == 0 )
    {
      return car_;
    }
  if( strcmp( fieldID, "cdr" ) == 0 )
    {
      return cdr_;
    }
  throw Exceptions::NonExistentMember( getTypeName( ), fieldID );
}

void
Lang::ConsPair::gcMark( Kernel::GCMarkedSet & marked )
{
  car_->gcMark( marked );
  cdr_->gcMark( marked );
}


Lang::SingleListMethodBase::SingleListMethodBase( RefCountPtr< const Lang::SingleList > self, Kernel::EvaluatedFormals * formals )
  : Lang::Function( formals ), self_( self )
{ }

Lang::SingleListMethodBase::~SingleListMethodBase( )
{ }

void
Lang::SingleListMethodBase::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::SingleList * >( self_.getPtr( ) )->gcMark( marked );
}

bool
Lang::SingleListMethodBase::isTransforming( ) const
{
  return false;
}

Lang::SingleListMethodFoldL::SingleListMethodFoldL( RefCountPtr< const Lang::SingleList > self )
  : Lang::SingleListMethodBase( self,
				new Kernel::EvaluatedFormals( strdup( Kernel::MethodId( Lang::SingleList::TypeID, "foldl" ).prettyName( ).getPtr( ) ) ) )
{
  formals_->appendEvaluatedCoreFormal( "op", Kernel::THE_SLOT_VARIABLE, true );
  formals_->appendEvaluatedCoreFormal( "nullRes", Kernel::THE_SLOT_VARIABLE, false );
}

Lang::SingleListMethodFoldL::~SingleListMethodFoldL( )
{ }

void
Lang::SingleListMethodFoldL::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  self_->foldl( evalState,
		Helpers::down_cast_CoreArgument< const Lang::Function >( "< core method foldl >", args, 0, callLoc ),
		args.getHandle( 1 ),
		callLoc );
}

Lang::SingleListMethodFoldR::SingleListMethodFoldR( RefCountPtr< const Lang::SingleList > self )
  : Lang::SingleListMethodBase( self,
				new Kernel::EvaluatedFormals( strdup( Kernel::MethodId( Lang::SingleList::TypeID, "foldr" ).prettyName( ).getPtr( ) ) ) )
{
  formals_->appendEvaluatedCoreFormal( "op", Kernel::THE_SLOT_VARIABLE, true );
  formals_->appendEvaluatedCoreFormal( "nullRes", Kernel::THE_SLOT_VARIABLE, false );
}

Lang::SingleListMethodFoldR::~SingleListMethodFoldR( )
{ }

void
Lang::SingleListMethodFoldR::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  self_->foldr( evalState,
		Helpers::down_cast_CoreArgument< const Lang::Function >( "< core method foldr >", args, 0, callLoc ),
		args.getHandle( 1 ),
		callLoc );
}
