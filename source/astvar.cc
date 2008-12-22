/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

#include "astvar.h"
#include "shapesexceptions.h"
#include "globals.h"
#include "autoonoff.h"
#include "specialunits.h"
#include "astfun.h"
#include "continuations.h"
#include "containertypes.h"

using namespace Shapes;
using namespace std;


Ast::SourceLocationMark::SourceLocationMark( const Ast::SourceLocation & loc )
	: Ast::Node( loc )
{ }

Ast::SourceLocationMark::~SourceLocationMark( )
{ }

void
Ast::SourceLocationMark::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{ }

void
Ast::SourceLocationMark::eval( Kernel::EvalState * evalState ) const
{
	throw Exceptions::InternalError( "SourceLocationMark::eval invoked." );
}

namespace Shapes
{
	namespace Helpers
	{
		class IsSourceLocationMark
		{
		public:
			bool operator () ( const Ast::Node * n ) const
			{
				return dynamic_cast< const Ast::SourceLocationMark * >( n ) != 0;
			}
		};
	}
}

Ast::CodeBracket::CodeBracket( const Ast::SourceLocation & loc, std::list< Ast::Node * > * nodes )
	: Ast::Expression( loc ), nodes_( nodes ), argumentOrder_( new typeof *argumentOrder_ ), dynamicMap_( 0 ), stateOrder_( new typeof *stateOrder_ )
{
	/* First, we remove any source location marks -- we don't need them anymore. */
	nodes_->remove_if( Helpers::IsSourceLocationMark( ) );

	for( std::list< Ast::Node * >::const_iterator i = nodes_->begin( );
			 i != nodes_->end( );
			 ++i )
		{
			typedef const Ast::BindNode T;
			T * tmp = dynamic_cast< T * >( *i );
			if( tmp != 0 )
				{
					const char * name = tmp->id( );
					{
						typedef const Ast::DefineVariable T;
						T * decl = dynamic_cast< T * >( tmp );
						if( decl != 0 )
							{
								if( argumentOrder_->find( name ) != argumentOrder_->end( ) )
									{
										Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExisting( tmp->idLoc( ), name ) );
										continue;
									}
								argumentOrder_->insert( std::pair< const char *, size_t >( name, argumentOrder_->size( ) ) );
								continue;
							}
					}
					{
						typedef const Ast::IntroduceState T;
						T * decl = dynamic_cast< T * >( tmp );
						if( decl != 0 )
							{
								if( stateOrder_->find( name ) != stateOrder_->end( ) )
									{
										Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExisting( tmp->idLoc( ), name ) );
										continue;
									}
								stateOrder_->insert( std::pair< const char *, size_t >( name, stateOrder_->size( ) ) );
								continue;
							}
					}
					{
						typedef const Ast::DynamicVariableDecl T;
						T * dynDecl = dynamic_cast< T * >( tmp );
						if( dynDecl != 0 )
							{
								if( dynamicMap_ == 0 )
									{
										dynamicMap_ = new std::map< const char *, size_t, charPtrLess >;
									}
								if( dynamicMap_->find( name ) != dynamicMap_->end( ) )
									{
										Ast::theAnalysisErrorsList.push_back( new Exceptions::IntroducingExisting( tmp->idLoc( ), name ) );
										continue;
									}
								dynamicMap_->insert( std::pair< const char *, size_t >( name, dynamicMap_->size( ) ) );
								continue;
							}
					}
				}
		}
}

Ast::CodeBracket::~CodeBracket( )
{
	typedef list< Ast::Node * >::iterator I;
	for( I i = nodes_->begin( ); i != nodes_->end( ); ++i )
		{
			delete *i;
		}
	delete nodes_;
	delete argumentOrder_;
	if( dynamicMap_ != 0 )
		{
			delete dynamicMap_;
		}
	delete stateOrder_;
}


void
Ast::CodeBracket::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * parentEnv )
{
	parent_ = parent;

	Ast::AnalysisEnvironment * env( new Ast::AnalysisEnvironment( Ast::theAnalysisEnvironmentList, parentEnv, argumentOrder_, stateOrder_ ) );
	if( dynamicMap_ != 0 )
		{
			env->setupDynamicKeyVariables( dynamicMap_ );
		}


	{
		typedef list< Ast::Node * >::iterator I;
		for( I i = nodes_->begin( ); i != nodes_->end( ); ++i )
			{
				(*i)->analyze( this, env );
			}
	}


	imperative_ = false;
	{
		typedef list< Ast::Node * >::const_iterator I;
		for( I i = nodes_->begin( ); i != nodes_->end( ); ++i )
			{
				if( dynamic_cast< Ast::Expression * >( *i ) != 0 &&
						! (*i)->imperative_ )
					{
						I tmp = i;
						++tmp;
						if( tmp != nodes_->end( ) )
							{
								Ast::theAnalysisErrorsList.push_back( new Exceptions::ExpectedImperative( (*i)->loc( ) ) );
							}
					}
				imperative_ = imperative_ || (*i)->imperative_;
			}
	}
}

void
Ast::CodeBracket::eval( Kernel::EvalState * evalState ) const
{
	if( nodes_->begin( ) == nodes_->end( ) )
		{
			Kernel::ContRef cont = evalState->cont_;
			cont->takeValue( Lang::THE_VOID,
											 evalState );
			return;
		}

	std::vector< Kernel::VariableHandle > * envValues = new std::vector< Kernel::VariableHandle >;
	envValues->reserve( argumentOrder_->size( ) );
	while( envValues->size( ) < argumentOrder_->size( ) )
		{
			envValues->push_back( NullPtr< Kernel::Variable >( ) );
		}

	std::vector< Kernel::StateHandle > * envStates = new std::vector< Kernel::StateHandle >;
	envStates->reserve( stateOrder_->size( ) );
	while( envStates->size( ) < stateOrder_->size( ) )
		{
			envStates->push_back( NullPtr< Kernel::State >( ) );
		}

	evalState->env_ = new Kernel::Environment( Kernel::theEnvironmentList, evalState->env_, argumentOrder_, RefCountPtr< std::vector< Kernel::VariableHandle > >( envValues ), stateOrder_, RefCountPtr< std::vector< Kernel::StateHandle > >( envStates ) );

	if( dynamicMap_ != 0 )
		{
			evalState->env_->setupDynamicKeyVariables( dynamicMap_ );
		}

	RefCountPtr< const Kernel::CodeBracketContInfo > info( new Kernel::CodeBracketContInfo( this, *evalState ) );

	evalAt( info, nodes_->begin( ), evalState );
}

void
Ast::CodeBracket::evalAt( const RefCountPtr< const Kernel::CodeBracketContInfo > & info, const std::list< Ast::Node * >::const_iterator & pos, Kernel::EvalState * evalState ) const
{
	{
		std::list< Ast::Node * >::const_iterator next = pos;
		++next;
		if( next == nodes_->end( ) )
			{
				const Ast::Expression * e = dynamic_cast< const Ast::Expression * >( *pos );
				if( e != 0 &&
						e->immediate_ )
					{
						evalState->cont_ = Kernel::ContRef( new Kernel::ForcingContinuation( info->cont_, (*pos)->loc( ) ) );
					}
				else
					{
						evalState->cont_ = info->cont_;
					}
			}
		else
			{
				evalState->cont_ = Kernel::ContRef( new Kernel::CodeBracketContinuation( (*pos)->loc( ), info, next ) );
			}
	}
	evalState->env_ = info->env_;
	evalState->dyn_ = info->dyn_;
	(*pos)->eval( evalState );
}


Kernel::CodeBracketContInfo::CodeBracketContInfo( const Ast::CodeBracket * bracketExpr, const Kernel::EvalState & evalState )
	: bracketExpr_( bracketExpr ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::CodeBracketContInfo::~CodeBracketContInfo( )
{ }

void
Kernel::CodeBracketContInfo::gcMark( Kernel::GCMarkedSet & marked )
{
	env_->gcMark( marked );
	dyn_->gcMark( marked );
	cont_->gcMark( marked );
}

Kernel::CodeBracketContinuation::CodeBracketContinuation( const Ast::SourceLocation & traceLoc, const RefCountPtr< const Kernel::CodeBracketContInfo > & info, const std::list< Ast::Node * >::const_iterator & pos )
	: Kernel::Continuation( traceLoc ), info_( info ), pos_( pos )
{ }

Kernel::CodeBracketContinuation::~CodeBracketContinuation( )
{ }

void
Kernel::CodeBracketContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	if( val.down_cast< const Lang::Void >( ) == NullPtr< const Lang::Void >( ) )
		{
			throw Exceptions::NonVoidStatement( traceLoc_, val );
		}
	info_->bracketExpr_->evalAt( info_,
															 pos_,
															 evalState );
}

void
Kernel::CodeBracketContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "code bracket" ) );
	info_->cont_->backTrace( trace );
}

void
Kernel::CodeBracketContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Kernel::CodeBracketContInfo * >( info_.getPtr( ) )->gcMark( marked );
}


Ast::LexiographicVariable::LexiographicVariable( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey )
	: Ast::Expression( loc ), id_( id ), idKey_( idKey )
{
	immediate_ = true;
}

Ast::LexiographicVariable::~LexiographicVariable( )
{
	delete id_;
	if( *idKey_ != 0 )
		{
			delete *idKey_;
		}
	delete idKey_;		 //	This can be done only as long as this is not shared!
}

void
Ast::LexiographicVariable::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( *idKey_ == 0 )
		{
			*idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalVariableKey( loc_, id_ ) );
		}

	imperative_ = false;
}

void
Ast::LexiographicVariable::eval( Kernel::EvalState * evalState ) const
{
	evalState->env_->lookup( **idKey_, evalState );
}


Ast::EvalOutsideExpr::EvalOutsideExpr( const Ast::SourceLocation & loc, Ast::Expression * expr )
	: Ast::Expression( loc ), expr_( expr )
{
	immediate_ = true;
}

Ast::EvalOutsideExpr::~EvalOutsideExpr( )
{
	delete expr_;
}

void
Ast::EvalOutsideExpr::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	expr_->analyze( this, env->getParent( ) );

	imperative_ = expr_->imperative_;
}

void
Ast::EvalOutsideExpr::eval( Kernel::EvalState * evalState ) const
{
	evalState->expr_ = expr_;
	evalState->env_ = evalState->env_->getParent( );
}


Ast::MemberReferenceFunction::MemberReferenceFunction( const Ast::SourceLocation & loc, Ast::Expression * variable, const char * fieldID )
	: Lang::Function( new Kernel::EvaluatedFormals( "<>.<>", true ) ), loc_( loc ), variable_( variable ), fieldID_( fieldID )
{ }

Ast::MemberReferenceFunction::~MemberReferenceFunction( )
{
	delete variable_;
	delete fieldID_;
}

void
Ast::MemberReferenceFunction::push_exprs( Ast::ArgListExprs * args ) const
{
	args->orderedExprs_->push_back( variable_ );
}

void
Ast::MemberReferenceFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	/* The variable is analyzed as part of the arguments passed to this function, so nothing needs to be done here...
	 * unless we would be able to figure out the type of the argument, and then check if the field reference is valid.
	 */
}

void
Ast::MemberReferenceFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	Kernel::ContRef cont = evalState->cont_;
	RefCountPtr< const Lang::Value > arg = args.getValue( 0 );
	cont->takeHandle( arg->getField( fieldID_, arg ),
										evalState );
}



Ast::MutatorReference::MutatorReference( const Ast::SourceLocation & mutatorLoc, Ast::StateReference * state, const char * mutatorID )
	: Ast::Expression( mutatorLoc ), mutatorLoc_( mutatorLoc ), state_( state ), mutatorID_( mutatorID )
{ }

Ast::MutatorReference::~MutatorReference( )
{
	/* At the time of implementing this bug-fix, state_ will allways be owned by the Ast::CallExpr that is also the owner of us.
	 * Hence, we do not consider ourselves owners.  Perhaps one should have a flag indicating whether ownership is transferred
	 * when calling the constructor, but at the moment this seems like a waste of resources.
	 */
	//	delete state_;
	delete mutatorID_;
}

void
Ast::MutatorReference::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	state_->analyze( this, env );

	imperative_ = true;

	/* If the type of the state was known here, we should verify that there is a mutator corresponding to the message <mutatorID>.
	 */
}

void
Ast::MutatorReference::eval( Kernel::EvalState * evalState ) const
{
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( state_->getHandle( evalState->env_, evalState->dyn_ )->getMutator( mutatorID_ ),
									 evalState );
}



Ast::SpecialLength::SpecialLength( const Ast::SourceLocation & loc, double val, int sort )
	: Ast::Expression( loc ), val_( val ), sort_( sort )
{ }

Ast::SpecialLength::~SpecialLength( )
{ }

void
Ast::SpecialLength::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	imperative_ = false;
}

void
Ast::SpecialLength::eval( Kernel::EvalState * evalState ) const
{
	Concrete::Length d;
	double a0;
	double a1;

	evalState->dyn_->specialUnitService( & d, & a0, & a1 );

	if( sort_ == Computation::SPECIALU_NOINFLEX )
		{
			Kernel::ContRef cont = evalState->cont_;
			cont->takeValue( Kernel::ValueRef( new Lang::Length( val_ * d * Computation::specialUnitNoInflexion( a0, a1 ) ) ),
											 evalState );
			return;
		}
	if( ! sort_ & Computation::SPECIALU_DIST )
		{
			throw Exceptions::InternalError( strrefdup( "The special unit is neither based on inflexion or distance" ) );
		}

	double res = 1;

	if( sort_ & Computation::SPECIALU_CIRC )
		{
			res *= Computation::specialUnitCircleHandle( a0 );
		}

	if( sort_ & Computation::SPECIALU_CORR )
		{
			res *= Computation::specialUnitCorrection( a0, a1 );
		}

	if( sort_ & Computation::SPECIALU_NOINFLEX )
		{
			res = min( res, Computation::specialUnitNoInflexion( a0, a1 ) );
		}
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Length( val_ * d * res ) ),
									 evalState );
}


Ast::DynamicVariable::DynamicVariable( const Ast::SourceLocation & loc, const char * id )
	: Ast::Expression( loc ), id_( id ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{
	immediate_ = true;
}

Ast::DynamicVariable::~DynamicVariable( )
{
	delete id_;
	if( *idKey_ != 0 )
		{
			delete *idKey_;
		}
	delete idKey_;		 //	This can be done only as long as this is not shared!
}

void
Ast::DynamicVariable::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( *idKey_ == 0 )
		{
			*idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalDynamicKey( loc_, id_ ) );
		}

	imperative_ = false;
}

void
Ast::DynamicVariable::eval( Kernel::EvalState * evalState ) const
{
	const Kernel::DynamicVariableProperties & dynProps = evalState->env_->lookupDynamicVariable( **idKey_ );

	Kernel::VariableHandle res = dynProps.fetch( evalState->dyn_ );

	/* Now, we know that if the value was bound to a dynamic expression, a value was bound, and that value has
	 * a certain type.
	 */
	if( ! res->isThunk( ) )
		{
			try
				{
					typedef const Lang::DynamicExpression DynType;
					RefCountPtr< DynType > dynVal = res->tryVal< DynType >( );
					dynVal->eval( evalState );
					return;
				}
			catch( const NonLocalExit::NotThisType & ball )
				{
					// Never mind.
				}
		}

	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( res,
										evalState );
}


Kernel::DynamicBindingContinuation::DynamicBindingContinuation( const Ast::SourceLocation & traceLoc, const Kernel::PassedEnv & env, const Kernel::Environment::LexicalKey & key, const Ast::SourceLocation & idLoc, const Kernel::ContRef & cont )
	: Kernel::Continuation( traceLoc ), env_( env ), key_( key ), idLoc_( idLoc ), cont_( cont )
{ }

Kernel::DynamicBindingContinuation::~DynamicBindingContinuation( )
{ }

void
Kernel::DynamicBindingContinuation::takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
{
	if( val->isThunk( ) )
		{
			val->force( val, evalState );
			return;
		}
	evalState->cont_ = cont_;
	env_->lookupDynamicVariable( key_ ).makeBinding( val, idLoc_, traceLoc_, evalState );
}

void
Kernel::DynamicBindingContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "dynamic binding" ) );
	cont_->backTrace( trace );
}

void
Kernel::DynamicBindingContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	env_->gcMark( marked );
	cont_->gcMark( marked );
}

Ast::DynamicBindingExpression::DynamicBindingExpression( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey )
	: Ast::Expression( Ast::SourceLocation( idLoc, expr->loc( ) ) ), idLoc_( idLoc ), id_( id ), expr_( expr ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{ }

Ast::DynamicBindingExpression::~DynamicBindingExpression( )
{
	delete id_;
	delete expr_;
	if( *idKey_ != 0 )
		{
			delete *idKey_;
			*idKey_ = 0;
		}
	// Don't delete idKey as it's shared!
}

void
Ast::DynamicBindingExpression::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	expr_->analyze( this, env );
	if( *idKey_ == 0 )
		{
			*idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalDynamicKey( idLoc_, id_ ) );
		}

	imperative_ = expr_->imperative_;
}

void
Ast::DynamicBindingExpression::eval( Kernel::EvalState * evalState ) const
{
	const Kernel::DynamicVariableProperties & dynProps = evalState->env_->lookupDynamicVariable( **idKey_ );

	if( dynProps.forceValue( ) || expr_->immediate_ )
		{
			evalState->expr_ = expr_;
			evalState->cont_ = Kernel::ContRef( new Kernel::DynamicBindingContinuation( expr_->loc( ), evalState->env_, **idKey_, idLoc_, evalState->cont_ ) );
		}
	else
		{
			dynProps.makeBinding( Kernel::VariableHandle( new Kernel::Variable( new Kernel::Thunk( evalState->env_, evalState->dyn_, expr_ ) ) ),
														idLoc_,
														expr_->loc( ),
														evalState );
		}
}


Ast::DynamicStateBindingExpression::DynamicStateBindingExpression( const Ast::SourceLocation & loc, const Ast::SourceLocation & dstLoc, const char * dstId, Ast::StateReference * src )
	: Ast::Expression( loc ), dstLoc_( dstLoc ), dstId_( dstId ), dstIdKey_( new Kernel::Environment::LexicalKey * ( 0 ) ), src_( src )
{ }

Ast::DynamicStateBindingExpression::~DynamicStateBindingExpression( )
{
	delete src_;
	if( *dstIdKey_ != 0 )
		{
			delete *dstIdKey_;
		}
	delete dstIdKey_;		 //	This can be done only as long as this is not shared!
}

void
Ast::DynamicStateBindingExpression::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( *dstIdKey_ == 0 )
		{
			*dstIdKey_ = new Kernel::Environment::LexicalKey( env->findLexicalDynamicKey( dstLoc_, dstId_ ) );
		}

	imperative_ = false;
}

void
Ast::DynamicStateBindingExpression::eval( Kernel::EvalState * evalState ) const
{
	const Kernel::DynamicStateProperties & dstDynProps = evalState->env_->lookupDynamicState( **dstIdKey_ );

	dstDynProps.makeBinding( src_->getHandle( evalState->env_, evalState->dyn_ ), dstLoc_, evalState );
}


Kernel::WithDynamicContinuation::WithDynamicContinuation( const Ast::SourceLocation & traceLoc, Ast::Expression * expr, const Kernel::EvalState & evalState )
	: Kernel::Continuation( traceLoc ), expr_( expr ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::WithDynamicContinuation::~WithDynamicContinuation( )
{ }

void
Kernel::WithDynamicContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	evalState->dyn_ = Kernel::PassedDyn( new Kernel::DynamicEnvironment( dyn_, *Helpers::down_cast< const Lang::DynamicBindings >( val, traceLoc_ ) ) );
	evalState->env_ = env_;
	evalState->expr_ = expr_;
	evalState->cont_ = cont_;
}

void
Kernel::WithDynamicContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "with dynamic bindings" ) );
	cont_->backTrace( trace );
}

void
Kernel::WithDynamicContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	env_->gcMark( marked );
	dyn_->gcMark( marked );
	cont_->gcMark( marked );
}


Ast::WithDynamicExpr::WithDynamicExpr( const Ast::SourceLocation & loc, Ast::Expression * bindings, Ast::Expression * expr )
	: Ast::Expression( loc ), bindings_( bindings ), expr_( expr )
{ }

Ast::WithDynamicExpr::~WithDynamicExpr( )
{ }

void
Ast::WithDynamicExpr::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	bindings_->analyze( this, env );
	expr_->analyze( this, env );

	imperative_ = bindings_->imperative_ || expr_->imperative_;
}

void
Ast::WithDynamicExpr::eval( Kernel::EvalState * evalState ) const
{
	evalState->expr_ = bindings_;
	evalState->cont_ = Kernel::ContRef( new Kernel::WithDynamicContinuation( bindings_->loc( ), expr_, *evalState ) );
}


Ast::DynamicVariableDecl::DynamicVariableDecl( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * filterExpr, Ast::Expression * defaultExpr )
	: Ast::BindNode( loc, idLoc, id ), idPos_( new size_t * ( 0 ) )
{
	/* This type of expression is an Ast::BindNode so that it is easy to recognize and extract the identifier for static analysis
	 * and similar tasks.
	 *
	 * The expression is implemented as a function call, since there are two subexpressions that may need evaluation.
	 */

	Ast::ArgListExprs * args = new Ast::ArgListExprs( false );
	Ast::DynamicVariableDeclFunction * res = new Ast::DynamicVariableDeclFunction( id, filterExpr, defaultExpr, idPos_ );
	res->push_exprs( args );
	impl_ = new Ast::CallExpr( loc,
														 RefCountPtr< const Lang::Function >( res ),
														 args );
}

Ast::DynamicVariableDecl::~DynamicVariableDecl( )
{ }

void
Ast::DynamicVariableDecl::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	impl_->analyze( this, env );
	if( *idPos_ == 0 )
		{
			*idPos_ = new size_t( env->findLocalDynamicPosition( idLoc_, id_ ) );
		}

	imperative_ = impl_->imperative_;
}

void
Ast::DynamicVariableDecl::eval( Kernel::EvalState * evalState ) const
{
	evalState->expr_ = impl_;
}


Ast::DynamicVariableDeclFunction::DynamicVariableDeclFunction( const char * id, Ast::Expression * filterExpr, Ast::Expression * defaultExpr, size_t ** idPos )
	: Lang::Function( new Kernel::EvaluatedFormals( "< dynamic variable declaration >" ) ), id_( id ), filterExpr_( filterExpr ), defaultExpr_( defaultExpr ), idPos_( idPos )
{
	formals_->appendEvaluatedCoreFormal( "filter", Kernel::THE_SLOT_VARIABLE, true );
	formals_->appendEvaluatedCoreFormal( "default", Kernel::THE_SLOT_VARIABLE, false );
}

Ast::DynamicVariableDeclFunction::~DynamicVariableDeclFunction( )
{
	delete filterExpr_;
	delete defaultExpr_;
}

void
Ast::DynamicVariableDeclFunction::push_exprs( Ast::ArgListExprs * args ) const
{
	args->orderedExprs_->push_back( filterExpr_ );
	args->orderedExprs_->push_back( defaultExpr_ );
}

void
Ast::DynamicVariableDeclFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	/* The analysis is carried out by the DynamicVariableDecl expression.
	 */
}

void
Ast::DynamicVariableDeclFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	static const char * title = "< dynamic variable declaration >";
	typedef const Lang::Function FilterType;
	evalState->env_->defineDynamic( id_,
																	**idPos_,
																	Helpers::down_cast_CoreArgument< FilterType >( title, args, 0, callLoc ),
																	args.getHandle( 1 ) );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
										evalState );
}

Kernel::DynamicVariableDeclContinuation::DynamicVariableDeclContinuation( const Ast::SourceLocation & traceLoc, const Ast::DynamicVariableDecl * declExpr, Kernel::EvalState & evalState )
	: Kernel::Continuation( traceLoc ), declExpr_( declExpr ), env_( evalState.env_ ), dyn_( evalState.dyn_ ), cont_( evalState.cont_ )
{ }

Kernel::DynamicVariableDeclContinuation::~DynamicVariableDeclContinuation( )
{ }

void
	Kernel::DynamicVariableDeclContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	evalState->env_ = env_;
	evalState->dyn_ = dyn_;
	evalState->cont_ = cont_;
	throw Exceptions::NotImplemented( "Deprecated: DynamicVariableDeclContinuation" );
	//	declExpr_->callBack( Helpers::down_cast< const Lang::Function >( val, traceLoc_ ),
	//											 evalState );
}

void
Kernel::DynamicVariableDeclContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "dynamic variable declaration" ) );
	cont_->backTrace( trace );
}

void
Kernel::DynamicVariableDeclContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	env_->gcMark( marked );
	dyn_->gcMark( marked );
	cont_->gcMark( marked );
}


Ast::DynamicStateDecl::DynamicStateDecl( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Ast::StateReference * defaultState, size_t ** idPos )
	: Ast::BindNode( loc, idLoc, id ), idPos_( idPos ), defaultState_( defaultState )
{ }

Ast::DynamicStateDecl::~DynamicStateDecl( )
{ }


void
Ast::DynamicStateDecl::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( *idPos_ == 0 )
		{
			*idPos_ = new size_t( env->findLocalDynamicStatePosition( idLoc_, id_ ) );
		}

	imperative_ = false;
}

void
Ast::DynamicStateDecl::eval( Kernel::EvalState * evalState ) const
{
	evalState->env_->defineDynamicState( id_,
																			 **idPos_,
																			 evalState,
																			 defaultState_ );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
										evalState );
}


Ast::EvalSymbolFunction::EvalSymbolFunction( const Ast::SourceLocation & loc, Ast::Expression * expr )
	: Lang::Function( new Kernel::EvaluatedFormals( "< symbol evaluation >", true ) ), loc_( loc ), expr_( expr )
{ }

Ast::EvalSymbolFunction::~EvalSymbolFunction( )
{ 
	delete expr_;
}

void
Ast::EvalSymbolFunction::push_exprs( Ast::ArgListExprs * args ) const
{
	args->orderedExprs_->push_back( expr_ );
}

void
Ast::EvalSymbolFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	analysisEnv_ = env;

	/* expr_ shall be analyzed from the calling expression.
	 * Here, it is only used to locate errors.
	 */
}

void
Ast::EvalSymbolFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	RefCountPtr< const Lang::Value > untypedVal = args.getValue( 0 );
	typedef const Lang::Symbol ArgType;
	ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
	if( val == 0 )
		{
			throw Exceptions::TypeMismatch( expr_->loc( ), untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
		}
	if( val->isUnique( ) )
		{
			throw Exceptions::OutOfRange( expr_->loc( ), strrefdup( "Unique symbols can't denote variables." ) );
		}


	Kernel::Environment::LexicalKey key = analysisEnv_->findLexicalVariableKey( loc_, val->name( ).getPtr( ) );

	Kernel::PassedEnv env = evalState->env_;
	env->lookup( key, evalState );
}


Ast::DefineVariable::DefineVariable( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos )
	: Ast::BindNode( Ast::SourceLocation( idLoc, expr->loc( ) ), idLoc, id ), expr_( expr ), idPos_( idPos )
{ }

Ast::DefineVariable::~DefineVariable( )
{
	delete expr_;

	/* idPos_ is shared and will be a memory leak which must not be deleted.
	 * It would be easy to fix the leak using RefCountPtr< size_t >, but the leakage is constant space, so silly efficiency is prioritized.
	 */
}


void
Ast::DefineVariable::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	expr_->analyze( this, env );
	if( *idPos_ == 0 )
		{
			*idPos_ = new size_t( env->findLocalVariablePosition( idLoc_, id_ ) );
		}

	imperative_ = expr_->imperative_;
}

void
Ast::DefineVariable::eval( Kernel::EvalState * evalState ) const
{
	if( expr_->immediate_ || expr_->imperative_ )
		{
			evalState->cont_ = Kernel::ContRef( new Kernel::DefineVariableContinuation( evalState->env_,
																																									*idPos_,
																																									evalState->cont_,
																																									expr_->loc( ) ) );
			evalState->expr_ = expr_;
		}
	else
		{
			evalState->env_->define( **idPos_,
															 Kernel::VariableHandle( new Kernel::Variable( new Kernel::Thunk( evalState->env_,
																																																evalState->dyn_,
																																																expr_ ) ) ) );
			Kernel::ContRef cont = evalState->cont_;
			cont->takeHandle( Kernel::THE_SLOT_VARIABLE, evalState );
		}
}


Kernel::AssertStructureContinuation::AssertStructureContinuation( const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc )
	: Kernel::Continuation( traceLoc ), cont_( cont )
{ }

Kernel::AssertStructureContinuation::~AssertStructureContinuation( )
{ }

void
Kernel::AssertStructureContinuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
{
	evalState->cont_ = cont_;
	cont_->takeValue( Helpers::down_cast_ContinuationArgument< const Lang::Structure >( val, this ), evalState );
}

void
Kernel::AssertStructureContinuation::backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
{
	trace->push_front( Kernel::Continuation::BackTraceElem( this, "Assert type is struct" ) );
	cont_->backTrace( trace );
}

void
Kernel::AssertStructureContinuation::gcMark( Kernel::GCMarkedSet & marked )
{
	cont_->gcMark( marked );
}


Ast::StructSplitReference::StructSplitReference( Ast::SourceLocation fieldLoc, const char * fieldId, Ast::Expression * defaultExpr )
	: Ast::Expression( fieldLoc ),
		structLoc_( Ast::THE_UNKNOWN_LOCATION ), // This is a dummy value!	The correct value is set later.
		fieldId_( fieldId ),
		defaultExpr_( defaultExpr )
{ }

Ast::StructSplitReference::StructSplitReference( Ast::SourceLocation fieldLoc, size_t fieldPos, Ast::Expression * defaultExpr )
	: Ast::Expression( fieldLoc ),
		structLoc_( Ast::THE_UNKNOWN_LOCATION ), // This is a dummy value!	The correct value is set later.
		fieldId_( 0 ), fieldPos_( fieldPos ),
		defaultExpr_( defaultExpr )
{ }

Ast::StructSplitReference::~StructSplitReference( )
{
	if( fieldId_ != 0 )
		{
			delete fieldId_;
		}
	if( defaultExpr_ != 0 )
		{
			delete defaultExpr_;
		}
}

void
Ast::StructSplitReference::setStruct( Ast::SourceLocation structLoc, size_t ** structPos )
{
	structLoc_ = structLoc;
	structPos_ = structPos;
}

void
Ast::StructSplitReference::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( defaultExpr_ != 0 )
		{
			defaultExpr_->analyze( this, env );
			imperative_ = defaultExpr_->imperative_;
		}
	else
		{
			imperative_ = false;
		}
}

void
Ast::StructSplitReference::eval( Kernel::EvalState * evalState ) const
{
	Kernel::VariableHandle structHandle = evalState->env_->getVarHandle( **structPos_ );
	typedef const Lang::Structure StructType;
	RefCountPtr< StructType > structVal = structHandle->getVal< StructType >( "Type-checked value in StructSplitReference::eval." );

	Kernel::ContRef cont = evalState->cont_;
	if( fieldId_ != 0 )
		{
			try
				{
					cont->takeHandle( structVal->getField( fieldId_, structVal ),
														evalState );
					return;
				}
			catch( const Exceptions::NonExistentMember & ball )
				{
					if( defaultExpr_ == 0 )
						{
							throw;
						}
					// Never mind, we use the default instead.	See below.
				}
		}
	else
		{
			try
				{
					cont->takeHandle( structVal->getPosition( fieldPos_, structVal ),
														evalState );
					return;
				}
			catch( const Exceptions::NonExistentPosition & ball )
				{
					if( defaultExpr_ == 0 )
						{
							throw;
						}
					// Never mind, we use the default instead.	See below.
				}
		}

	if( defaultExpr_ == 0 )
		{
			throw Exceptions::InternalError( "Just about to use null pointer defaultExpr_ in StructSplitReference::eval." );
		}
	evalState->expr_ = defaultExpr_;
}

Ast::StructSplitSink::StructSplitSink( )
	: Ast::Expression( Ast::THE_UNKNOWN_LOCATION ), structLoc_( Ast::THE_UNKNOWN_LOCATION )
{ }

Ast::StructSplitSink::~StructSplitSink( )
{ }

void
Ast::StructSplitSink::setStruct( Ast::SourceLocation structLoc, size_t ** structPos, size_t consumedArguments )
{
	structLoc_ = structLoc;
	structPos_ = structPos;
	consumedArguments_ = consumedArguments;
}

void
Ast::StructSplitSink::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	imperative_ = false;
}

void
Ast::StructSplitSink::eval( Kernel::EvalState * evalState ) const
{
	Kernel::VariableHandle structHandle = evalState->env_->getVarHandle( **structPos_ );
	typedef const Lang::Structure StructType;
	RefCountPtr< StructType > structVal = structHandle->getVal< StructType >( "Type-checked value in StructSplitReference::eval." );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( structVal->getSink( consumedArguments_ ),
									 evalState );
}


Ast::AssertNoSinkNeeded::AssertNoSinkNeeded( const Ast::SourceLocation & loc, size_t orderedCount, Ast::SourceLocation structLoc, size_t ** structPos )
	: Ast::Expression( loc ), orderedCount_( orderedCount ), structLoc_( structLoc ), structPos_( structPos )
{ }

Ast::AssertNoSinkNeeded::~AssertNoSinkNeeded( )
{ }

void
Ast::AssertNoSinkNeeded::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	imperative_ = true;
}

void
Ast::AssertNoSinkNeeded::eval( Kernel::EvalState * evalState ) const
{
	Kernel::VariableHandle structHandle = evalState->env_->getVarHandle( **structPos_ );
	typedef const Lang::Structure StructType;
	RefCountPtr< StructType > structVal = structHandle->getVal< StructType >( "Type-checked value in StructSplitReference::eval." );

	if( structVal->argList_->orderedExprs_->size( ) > orderedCount_ )
		{
			throw Exceptions::SinkRequired( loc_, orderedCount_, structVal->argList_->orderedExprs_->size( ) );
		}

	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( Kernel::THE_SLOT_VARIABLE, evalState );
}

size_t Ast::SplitDefineVariables::splitVarCount = 0;
PtrOwner_back_Access< std::list< const char * > > Ast::SplitDefineVariables::mem;

Ast::SplitDefineVariables::SplitDefineVariables( )
	: sinkDefine_( 0 ), sinkExpr_( 0 ), seenNamed_( false ), seenDefault_( false )
{
	std::ostringstream oss;
	oss << Kernel::SPLIT_VAR_PREFIX << splitVarCount ;
	splitVarId_ = strdup( oss.str( ).c_str( ) );
	mem.push_back( splitVarId_ );
	++splitVarCount;
}

const char *
Ast::SplitDefineVariables::newSplitVarId( ) const
{
	return strdup( splitVarId_ );
}


Ast::StateReference::StateReference( const Ast::SourceLocation & loc )
	: Ast::Node( loc )
{ }

Ast::StateReference::~StateReference( )
{ }

void
Ast::StateReference::eval( Kernel::EvalState * evalState ) const
{
	throw Exceptions::InternalError( "A state reference was evaluated." );
}


Ast::LexiographicState::LexiographicState( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey )
	: Ast::StateReference( loc ), id_( id ), idKey_( idKey )
{ }

Ast::LexiographicState::~LexiographicState( )
{
	delete id_;
	if( *idKey_ != 0 )
		{
			delete *idKey_;
		}
	delete idKey_;		 //	This can be done only as long as this is not shared!
}

void
Ast::LexiographicState::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( *idKey_ == 0 )
		{
			*idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalStateKey( loc_, id_ ) );
		}

	imperative_ = true;
}

Kernel::StateHandle
Ast::LexiographicState::getHandle( Kernel::PassedEnv env, Kernel::PassedDyn dyn ) const
{
	return env->getStateHandle( **idKey_ );
}


Ast::DynamicState::DynamicState( const Ast::SourceLocation & loc, const char * id )
	: Ast::StateReference( loc ), id_( id ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{ }

Ast::DynamicState::~DynamicState( )
{
	delete id_;
	if( *idKey_ != 0 )
		{
			delete *idKey_;
		}
	delete idKey_;		 //	This can be done only as long as this is not shared!
}

void
Ast::DynamicState::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	/* It would make sense to check the reference and...
	 */
	imperative_ = true;
}

Kernel::StateHandle
Ast::DynamicState::getHandle( Kernel::PassedEnv env, Kernel::PassedDyn dyn ) const
{
	throw Exceptions::NotImplemented( "Referencing dynamic states" );
}


Ast::IntroduceState::IntroduceState( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos )
	: Ast::BindNode( Ast::SourceLocation( idLoc, expr->loc( ) ), idLoc, id ), expr_( expr ), idPos_( idPos )
{ }

Ast::IntroduceState::~IntroduceState( )
{
	delete expr_;

	/* idPos_ shared and will be a memory leak which must not be deleted.
	 * It would be easy to fix the leak using RefCountPtr< size_t >, but the leakage is constant space, so silly efficiency is prioritized.
	 */
}


void
Ast::IntroduceState::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	expr_->analyze( this, env );

	if( *idPos_ == 0 )
		{
			*idPos_ = new size_t( env->findLocalStatePosition( idLoc_, id_ ) );
		}

	imperative_ = expr_->imperative_;
}

void
Ast::IntroduceState::eval( Kernel::EvalState * evalState ) const
{
	evalState->cont_ = Kernel::ContRef( new Kernel::IntroduceStateContinuation( evalState->env_,
																																							*idPos_,
																																							evalState->cont_,
																																							expr_->loc( ) ) );
	evalState->expr_ = expr_;
}


Ast::Insertion::Insertion( Ast::StateReference * stateRef, Ast::Expression * expr )
	: Ast::Node( Ast::SourceLocation( stateRef->loc( ), expr->loc( ) ) ), stateRef_( stateRef ), expr_( expr )
{ }

Ast::Insertion::~Insertion( )
{ }

void
Ast::Insertion::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	stateRef_->analyze( this, env );
	expr_->analyze( this, env );

	imperative_ = true;
}

void
Ast::Insertion::eval( Kernel::EvalState * evalState ) const
{
	evalState->cont_ = Kernel::ContRef( new Kernel::InsertionContinuation( stateRef_->getHandle( evalState->env_, evalState->dyn_ ),
																																				 evalState->cont_,
																																				 evalState->dyn_,
																																				 expr_->loc( ) ) );
	evalState->expr_ = expr_;
}

Ast::Freeze::Freeze( const Ast::SourceLocation & idLoc, const char * id, size_t ** idPos )
	: Ast::Expression( idLoc ), id_( id ), idPos_( idPos )
{
	immediate_ = true;
}

Ast::Freeze::~Freeze( )
{
	/* idPos shared and will be a memory leak which must not be deleted.
	 * It would be easy to fix the leak using RefCountPtr< size_t >, but the leakage is constant space, so silly efficiency is prioritized.
	 */
}

void
Ast::Freeze::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( *idPos_ == 0 )
		{
			*idPos_ = new size_t( env->findLocalStatePosition( loc( ), id_ ) );
		}

	imperative_ = true;
}

void
Ast::Freeze::eval( Kernel::EvalState * evalState ) const
{
	evalState->env_->freeze( **idPos_, evalState, loc( ) );
}


Ast::Peek::Peek( const Ast::SourceLocation & idLoc, Ast::StateReference * stateRef )
	: Ast::Expression( idLoc ), stateRef_( stateRef )
{
	immediate_ = true;
}

Ast::Peek::~Peek( )
{
	/* idPos shared and will be a memory leak which must not be deleted.
	 * It would be easy to fix the leak using RefCountPtr< size_t >, but the leakage is constant space, so silly efficiency is prioritized.
	 */
}

void
Ast::Peek::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	stateRef_->analyze( this, env );

	imperative_ = true;
}

void
Ast::Peek::eval( Kernel::EvalState * evalState ) const
{
	stateRef_->getHandle( evalState->env_, evalState->dyn_ )->peek( evalState, loc( ) );
}


Ast::DynamicExpression::DynamicExpression( const Ast::SourceLocation & loc, Ast::Expression * expr )
	: Ast::Expression( loc ), expr_( expr )
{
	immediate_ = true;
}

Ast::DynamicExpression::~DynamicExpression( )
{ }

void
Ast::DynamicExpression::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	expr_->analyze( this, env );

	if( expr_->imperative_ )
		{
			Ast::theAnalysisErrorsList.push_back( new Exceptions::IllegalImperative( expr_->loc( ) ) );
		}
	imperative_ = false;
}

void
Ast::DynamicExpression::eval( Kernel::EvalState * evalState ) const
{
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::DynamicExpression( evalState->env_, expr_ ) ),
									 evalState );
}


Ast::LexiographicType::LexiographicType( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey )
	: Ast::Expression( loc ), id_( id ), idKey_( idKey )
{
	immediate_ = true;
}

Ast::LexiographicType::~LexiographicType( )
{
	delete id_;
	if( *idKey_ != 0 )
		{
			delete *idKey_;
		}
	delete idKey_;		 //	This can be done only as long as this is not shared!
}

void
Ast::LexiographicType::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
	parent_ = parent;

	if( *idKey_ == 0 )
		{
			*idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalTypeKey( loc_, id_ ) );
		}

	imperative_ = false;
}

void
Ast::LexiographicType::eval( Kernel::EvalState * evalState ) const
{
	evalState->env_->lookup( **idKey_, evalState );
}


