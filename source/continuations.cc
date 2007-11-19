#include "Shapes_Helpers_decls.h"

#include "continuations.h"
#include "hottypes.h"
#include "globals.h"
#include "shapescore.h"
#include "functiontypes.h"

using namespace Shapes;


Kernel::IfContinuation::IfContinuation( const Kernel::VariableHandle & consequence, const Kernel::VariableHandle & alternative, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), consequence_( consequence ), alternative_( alternative ), cont_( cont )
{ }

Kernel::IfContinuation::~IfContinuation( )
{ }

void
Kernel::IfContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	typedef const Lang::Boolean ArgType;
	RefCountPtr< ArgType > arg = Helpers::down_cast_ContinuationArgument< ArgType >( val, this );

	if( arg->val_ )
		{
			evalState->cont_ = cont_;
			cont_->takeHandle( consequence_,
												evalState );
		}
	else
		{
			evalState->cont_ = cont_;
			cont_->takeHandle( alternative_,
												evalState );
		}
}

void
Kernel::IfContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "if" ) );
	cont_->backTrace( trace );
}

void
Kernel::IfContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	consequence_->gcMark( marked );
	alternative_->gcMark( marked );
	cont_->gcMark( marked );
}


Kernel::DefineVariableContinuation::DefineVariableContinuation( const Kernel::PassedEnv & env, size_t * pos, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), env_( env ), pos_( pos ), cont_( cont )
{ }

Kernel::DefineVariableContinuation::~DefineVariableContinuation( )
{ }

void
Kernel::DefineVariableContinuation::takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
{
	// This continuation is only used when the expression is explicitly forced.
	if( val->isThunk( ) )
		{
			val->force( val, evalState );
		}
	else
		{
			env_->define( *pos_, val );
			evalState->cont_ = cont_;
			cont_->takeHandle( Kernel::THE_SLOT_VARIABLE, evalState );
		}
}

void
Kernel::DefineVariableContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "introduce cold" ) );
	cont_->backTrace( trace );
}

void
Kernel::DefineVariableContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	env_->gcMark( marked );
	cont_->gcMark( marked );
}


Kernel::IntroduceStateContinuation::IntroduceStateContinuation( const Kernel::PassedEnv & env, size_t * pos, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), env_( env ), pos_( pos ), cont_( cont )
{ }

Kernel::IntroduceStateContinuation::~IntroduceStateContinuation( )
{ }

void
Kernel::IntroduceStateContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	RefCountPtr< const Lang::Hot > hot( Helpers::down_cast< const Lang::Hot >( val, traceLoc_ ) );
	env_->introduceState( *pos_, hot->newState( ) );
	evalState->cont_ = cont_;
	cont_->takeHandle( Kernel::THE_SLOT_VARIABLE, evalState );
}

void
Kernel::IntroduceStateContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "introduce warm" ) );
	cont_->backTrace( trace );
}

void
Kernel::IntroduceStateContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	env_->gcMark( marked );
	cont_->gcMark( marked );
}


Kernel::StoreValueContinuation::StoreValueContinuation( Kernel::ValueRef * res, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), res_( res ), cont_( cont )
{ }

Kernel::StoreValueContinuation::~StoreValueContinuation( )
{ }

void
Kernel::StoreValueContinuation::takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
{
	if( val->isThunk( ) )
		{
			val->force( val, evalState );
		}
	else
		{
			*res_ = val->getUntyped( );
			evalState->cont_ = cont_;
			cont_->takeHandle( val, evalState );
		}
}

void
Kernel::StoreValueContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "store and return" ) );
	cont_->backTrace( trace );
}

void
Kernel::StoreValueContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	cont_->gcMark( marked );
}


Kernel::StoreVariableContinuation::StoreVariableContinuation( const Kernel::VariableHandle & dst, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), dst_( dst ), cont_( cont )
{ }

Kernel::StoreVariableContinuation::~StoreVariableContinuation( )
{ }

void
Kernel::StoreVariableContinuation::takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
{
	if( val->isThunk( ) )
		{
			val->force( val, evalState );
		}
	else
		{
			/* This class is friends with Variable, hence setValue is accessible. */
			dst_->setValue( val->getUntyped( ) );
			evalState->cont_ = cont_;
			cont_->takeHandle( val, evalState );
		}
}

void
Kernel::StoreVariableContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "store and return" ) );
	cont_->backTrace( trace );
}

void
Kernel::StoreVariableContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	dst_->gcMark( marked );
	cont_->gcMark( marked );
}


Kernel::InsertionContinuation::InsertionContinuation( const Kernel::StateHandle & dst, const Kernel::ContRef & cont, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), dst_( dst ), dyn_( dyn ), cont_( cont )
{ }

Kernel::InsertionContinuation::~InsertionContinuation( )
{ }

void
Kernel::InsertionContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	evalState->cont_ = cont_;
	dst_->tackOn( evalState, val, dyn_, traceLoc_ );
}

void
Kernel::InsertionContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "insertion" ) );
	cont_->backTrace( trace );
}

void
Kernel::InsertionContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	cont_->gcMark( marked );
	dyn_->gcMark( marked );
}


Kernel::StmtStoreValueContinuation::StmtStoreValueContinuation( Kernel::ValueRef * res, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), res_( res ), cont_( cont )
{ }

Kernel::StmtStoreValueContinuation::~StmtStoreValueContinuation( )
{ }

void
Kernel::StmtStoreValueContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	*res_ = val;
	evalState->cont_ = cont_;
	cont_->takeHandle( Kernel::THE_SLOT_VARIABLE, evalState );
}

void
Kernel::StmtStoreValueContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "statement store (value)" ) );
	cont_->backTrace( trace );
}

void
Kernel::StmtStoreValueContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	cont_->gcMark( marked );
}


Kernel::StmtStoreVariableContinuation::StmtStoreVariableContinuation( const Kernel::VariableHandle & dst, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), dst_( dst ), cont_( cont )
{ }

Kernel::StmtStoreVariableContinuation::~StmtStoreVariableContinuation( )
{ }

void
Kernel::StmtStoreVariableContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	/* This class is friends with Variable, hence setValue is accessible. */
	dst_->setValue( val );
	evalState->cont_ = cont_;
	cont_->takeHandle( Kernel::THE_SLOT_VARIABLE, evalState );
}

void
Kernel::StmtStoreVariableContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "statement store (variable)" ) );
	cont_->backTrace( trace );
}

void
Kernel::StmtStoreVariableContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	dst_->gcMark( marked );
	cont_->gcMark( marked );
}



Kernel::ForcingContinuation::ForcingContinuation( const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), cont_( cont )
{ }

Kernel::ForcingContinuation::~ForcingContinuation( )
{ }

void
Kernel::ForcingContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	/* The reason for using this is simply that it only takes values.	Then, the value
	 * is passed to whatever surrounding continuation.
	 */
	evalState->cont_ = cont_;
	cont_->takeValue( val, evalState );
}

void
Kernel::ForcingContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "force evaluation" ) );
	cont_->backTrace( trace );
}

void
Kernel::ForcingContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	cont_->gcMark( marked );
}


Kernel::ExitContinuation::ExitContinuation( bool * done, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), done_( done )
{ }

Kernel::ExitContinuation::~ExitContinuation( )
{ }

void
Kernel::ExitContinuation::takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
{
	*done_ = true;
}

void
Kernel::ExitContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "exit (non-forcing)" ) );
}

void
Kernel::ExitContinuation::gcMark( Kernel::GCMarkedSet & marked )
{ }


Kernel::ExitVoidContinuation::ExitVoidContinuation( bool * done, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), done_( done )
{ }

Kernel::ExitVoidContinuation::~ExitVoidContinuation( )
{ }

void
Kernel::ExitVoidContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	if( val.down_cast< const Lang::Void >( ) == NullPtr< const Lang::Void >( ) )
		{
			throw Exceptions::NonVoidStatement( traceLoc_, val );
		}
	*done_ = true;
}

void
Kernel::ExitVoidContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "exit (forcing)" ) );
}

void
Kernel::ExitVoidContinuation::gcMark( Kernel::GCMarkedSet & marked )
{ }


Kernel::Transform2DCont::Transform2DCont( Lang::Transform2D tf, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), tf_( tf ), cont_( cont )
{ }

Kernel::Transform2DCont::~Transform2DCont( )
{ }

void
Kernel::Transform2DCont::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	RefCountPtr< const Lang::Geometric2D > arg = Helpers::down_cast< const Lang::Geometric2D >( val, traceLoc_ );
	evalState->cont_ = cont_;
	cont_->takeValue( arg->transformed( tf_, arg ),
										evalState );
}

void
Kernel::Transform2DCont::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "2D transform application" ) );
	cont_->backTrace( trace );
}

void
Kernel::Transform2DCont::gcMark( Kernel::GCMarkedSet & marked )
{
	cont_->gcMark( marked );
}


Kernel::Transform3DCont::Transform3DCont( Lang::Transform3D tf, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), tf_( tf ), cont_( cont )
{ }

Kernel::Transform3DCont::~Transform3DCont( )
{ }

void
Kernel::Transform3DCont::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	RefCountPtr< const Lang::Geometric3D > arg = Helpers::down_cast< const Lang::Geometric3D >( val, traceLoc_ );
	evalState->cont_ = cont_;
	cont_->takeValue( arg->transformed( tf_, arg ),
									 evalState );
}

void
Kernel::Transform3DCont::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "3D transform application" ) );
	cont_->backTrace( trace );
}

void
Kernel::Transform3DCont::gcMark( Kernel::GCMarkedSet & marked )
{
	cont_->gcMark( marked );
}


Kernel::PathApplication2DCont::PathApplication2DCont( RefCountPtr< const Lang::ElementaryPath2D > path, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), path_( path ), cont_( cont )
{ }

Kernel::PathApplication2DCont::~PathApplication2DCont( )
{ }

void
Kernel::PathApplication2DCont::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	Concrete::SplineTime t = Helpers::pathTimeCast( path_.getPtr( ), val.getPtr( ), this );
	
	evalState->cont_ = cont_;
	cont_->takeValue( Kernel::ValueRef( new Lang::PathSlider2D( path_, t.t( ) ) ),
									 evalState );
	return;
}

void
Kernel::PathApplication2DCont::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "2D path point selection" ) );
	cont_->backTrace( trace );
}

void
Kernel::PathApplication2DCont::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::ElementaryPath2D * >( path_.getPtr( ) )->gcMark( marked );
	cont_->gcMark( marked );
}


Kernel::PathApplication3DCont::PathApplication3DCont( RefCountPtr< const Lang::ElementaryPath3D > path, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), path_( path ), cont_( cont )
{ }

Kernel::PathApplication3DCont::~PathApplication3DCont( )
{ }

void
Kernel::PathApplication3DCont::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	Concrete::SplineTime t = Helpers::pathTimeCast( path_.getPtr( ), val.getPtr( ), this );
	
	evalState->cont_ = cont_;
	cont_->takeValue( Kernel::ValueRef( new Lang::PathSlider3D( path_, t.t( ) ) ),
										evalState );
	return;
}

void
Kernel::PathApplication3DCont::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "3D path point selection" ) );
	cont_->backTrace( trace );
}

void
Kernel::PathApplication3DCont::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::ElementaryPath3D * >( path_.getPtr( ) )->gcMark( marked );
	cont_->gcMark( marked );
}


Kernel::ComposedFunctionCall_cont::ComposedFunctionCall_cont( const RefCountPtr< const Lang::Function > & second, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & callLoc )
	: Kernel::Continuation( callLoc ), second_( second ), dyn_( dyn ), cont_( cont )
{ }

Kernel::ComposedFunctionCall_cont::~ComposedFunctionCall_cont( )
{ }

void
Kernel::ComposedFunctionCall_cont::takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
{
	evalState->dyn_ = dyn_;
	evalState->cont_ = cont_;
	second_->call( second_, evalState, val, traceLoc_ );
}

void
Kernel::ComposedFunctionCall_cont::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "composed function's second application" ) );
	cont_->backTrace( trace );
}

void
Kernel::ComposedFunctionCall_cont::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Function * >( second_.getPtr( ) )->gcMark( marked );
	dyn_->gcMark( marked );
	cont_->gcMark( marked );
}

