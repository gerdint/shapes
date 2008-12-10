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

#include <cmath>

#include "environment.h"
#include "shapesexceptions.h"
#include "shapescore.h"
#include "consts.h"
#include "globals.h"
#include "shapesvalue.h"
#include "classtypes.h"
#include "hottypes.h"
#include "continuations.h"
#include "statetypes.h"
#include "multipage.h"
#include "errorhandlers.h"
#include "debuglog.h"

using namespace Shapes;
using namespace std;


size_t Kernel::Environment::createdCount = 0;
size_t Kernel::Environment::liveCount = 0;
Kernel::Environment::LexicalKey Kernel::Environment::theMissingKey( 0, std::numeric_limits< size_t >::max( ) );

template< class T >
class Binary
{
public:
	T val;
	Binary( const T & _val ) : val( _val ) { }
};

template< class T >
std::ostream &
operator << ( std::ostream & os, const Binary< T > & self )
{
	for( int i = 8*sizeof( T ) - 1; i >= 0; --i )
		{
			if( (( self.val & (T(1)<<i) )) != 0 )
				{
					os << 1 ;
				}
			else
				{
					os << '_' ;
				}
		}
	return os;
}



Kernel::Thunk::Thunk( Kernel::PassedEnv env, Kernel::PassedDyn dyn, Ast::Expression * expr )
	: env_( env ), dyn_( dyn ), expr_( expr ), forced_( false )
{ }

void
Kernel::Thunk::force( Kernel::EvalState * evalState, bool onlyOnce ) const
{
	if( onlyOnce )
		{
			if( forced_ )
				{
					throw Exceptions::InternalError( "It's unwise to force a thunk twice!	It should be deleted right after the first force." );
				}
			forced_ = true;
		}

	evalState->expr_ = expr_;
	evalState->env_ = env_;
	evalState->dyn_ = dyn_;

	/* The continuation is not se by the thunk! */
}

Kernel::Thunk *
Kernel::Thunk::deepCopy( )
{
	return new Thunk( env_, dyn_, expr_ );
}


void
Kernel::Thunk::gcMark( Kernel::GCMarkedSet & marked )
{
	if( ! forced_ )
		{
			env_->gcMark( marked );
			dyn_->gcMark( marked );
		}
}

Ast::Expression *
Kernel::Thunk::getExpr( )
{
	return expr_;
}

void
Kernel::Thunk::printEnv( std::ostream & os ) const
{
	env_->print( os );
}


Kernel::Variable::Variable( const RefCountPtr< const Lang::Value > & val )
	: thunk_( 0 ), val_( val ), state_( Kernel::Variable::COLD )
{ }

Kernel::Variable::Variable( Kernel::Thunk * thunk )
	: thunk_( thunk ), val_( NullPtr< const Lang::Value >( ) ), state_( Kernel::Variable::THUNK )
{ }

Kernel::Variable::~Variable( )
{
	if( thunk_ != 0 )
		{
			delete thunk_;
		}
}

Kernel::State::State( )
	: alive_( true )
{ }

Kernel::State::~State( )
{ }

void
Kernel::State::tackOn( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc )
{
	if( ! alive_ )
		{
			throw Exceptions::DeadStateAccess( );
		}
	this->tackOnImpl( evalState, piece, callLoc );
}

void
Kernel::State::peek( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc )
{
	if( ! alive_ )
		{
			throw Exceptions::DeadStateAccess( );
		}
	this->peekImpl( evalState, callLoc );
}

void
Kernel::State::freeze( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc )
{
	if( ! alive_ )
		{
			throw Exceptions::DeadStateAccess( );
		}
	/* It would make sense to have an intermediate state here.
	 */
	alive_ = false;

	// Perhaps, it would be smart to use a continuation to erase the implementation once it is used...
	//	evalState->cont_ = Kernel::ContRef( new Kernel::StmtStoreVariableContinuation( selfRef, evalState->cont_, callLoc ) );

	this->freezeImpl( evalState, callLoc );
}

RefCountPtr< const Lang::Function >
Kernel::State::getMutator( const char * mutatorID )
{
	return getClass( )->getMutator( mutatorID );
}

RefCountPtr< const char >
Kernel::State::getTypeName( ) const
{
	return this->getClass( )->getPrettyName( );
}

bool
Kernel::State::isAlive( ) const
{
	return alive_;
}


void
Kernel::Variable::force( Kernel::VariableHandle & selfRef, Kernel::EvalState * evalState ) const
{
	if( state_ == Kernel::Variable::THUNK )
		{
			state_ = Kernel::Variable::FORCING;
			evalState->cont_ = Kernel::ContRef( new Kernel::StoreVariableContinuation( selfRef, evalState->cont_, thunk_->getExpr( )->loc( ) ) );
			thunk_->force( evalState );
			delete thunk_;
			thunk_ = 0;
			return;
		}
	if( val_ == NullPtr< const Lang::Value >( ) )
		{
			if( state_ == Kernel::Variable::FORCING )
				{
					throw Exceptions::MiscellaneousRequirement( "Cyclic forcing." );
				}
			throw Exceptions::InternalError( "Force failed.	No value, not forcing." );
		}
	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( selfRef, evalState );
}

RefCountPtr< const Lang::Value > &
Kernel::Variable::getUntyped( ) const
{
	if( val_ == NullPtr< const Lang::Value >( ) )
		{
			throw Exceptions::InternalError( "The value is not ready to be get without continuation." );
		}
	return val_;
}


bool
Kernel::Variable::isThunk( ) const
{
	return ( state_ & Kernel::Variable::FORCING ) != 0;
}

Kernel::Thunk *
Kernel::Variable::copyThunk( ) const
{
	if( thunk_ == 0 )
		{
			throw Exceptions::InternalError( "Variable::copyThunk: There was no thunk to copy." );
		}
	return thunk_->deepCopy( );
}

void
Kernel::Variable::gcMark( Kernel::GCMarkedSet & marked )
{
	if( thunk_ != 0 )
		{
			thunk_->gcMark( marked );
			return;
		}
	if( val_ != NullPtr< const Lang::Value >( ) )
		{
			const_cast< Lang::Value * >( val_.getPtr( ) )->gcMark( marked );
		}
}

void
Kernel::Variable::setValue( const RefCountPtr< const Lang::Value > & val ) const
{
	val_ = val;
	state_ = Kernel::Variable::COLD;
}


Kernel::DynamicVariableProperties::~DynamicVariableProperties( )
{ }

Kernel::DynamicStateProperties::~DynamicStateProperties( )
{ }


void
Kernel::Environment::initDefineCoreFunction( Lang::CoreFunction * fun )
{
	initDefineHandle( fun->getTitle( ), Kernel::VariableHandle( new Kernel::Variable( RefCountPtr< const Lang::Value >( fun ) ) ) );
}

void
Kernel::Environment::initDefineCoreFunction( RefCountPtr< const Lang::CoreFunction > fun )
{
	initDefineHandle( fun->getTitle( ), Kernel::VariableHandle( new Kernel::Variable( fun ) ) );
}

void
Kernel::Environment::initDefineHandle( const char * id, const Kernel::VariableHandle & val )
{
	if( Interaction::logGlobals )
		{
			Kernel::theDebugLog.os( ) << "--log-globals> variable: " << id << std::endl ;
		}
	if( bindings_->find( id ) != bindings_->end( ) )
		{
			throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< Initialization >" ), id );
		}

	(*bindings_)[ id ] = values_->size( );

	values_->push_back( val );
}

void
Kernel::Environment::initDefine( const char * id, const RefCountPtr< const Lang::Value > & val )
{
	initDefineHandle( id, Kernel::VariableHandle( new Kernel::Variable( val ) ) );
}

void
Kernel::Environment::initDefine( const char * id, Kernel::StateHandle state )
{
	if( Interaction::logGlobals )
		{
			Kernel::theDebugLog.os( ) << "--log-globals> state: " << id << std::endl ;
		}
	if( stateBindings_->find( id ) != stateBindings_->end( ) )
		{
			throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< Initialization >" ), id );
		}

	(*stateBindings_)[ id ] = states_->size( );

	states_->push_back( state );
}

void
Kernel::Environment::initDefineClass( const RefCountPtr< const Lang::Class > & cls )
{
	RefCountPtr< const char > idRef = cls->getPrettyName( );
	const char * id = strdup( idRef.getPtr( ) );
	charPtrDeletionList_.push_back( id );
	initDefineHandle( id, Kernel::VariableHandle( new Kernel::Variable( cls ) ) );
}

void
Kernel::Environment::initDefineDynamic( DynamicVariableProperties * dynProps )
{
	if( Interaction::logGlobals )
		{
			Kernel::theDebugLog.os( ) << "--log-globals> dynamic: " << dynProps->getName( ) << std::endl ;
		}
	if( dynamicKeyBindings_->find( dynProps->getName( ) ) != dynamicKeyBindings_->end( ) )
		{
			throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< System dynamic variable initialization >" ), dynProps->getName( ) );
		}

	(*dynamicKeyBindings_)[ dynProps->getName( ) ] = dynamicKeyValues_->size( );

	dynamicKeyValues_->push_back( dynProps );
}

void
Kernel::Environment::initDefineDynamic( const char * id, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal )
{
	if( dynamicKeyBindings_->find( id ) != dynamicKeyBindings_->end( ) )
		{
			throw Exceptions::IntroducingExisting( Ast::SourceLocation( "< System dynamic variable initialization >" ), id );
		}

	Kernel::DynamicEnvironmentKeyType key = dynamicKeyValues_->size( );
	(*dynamicKeyBindings_)[ id ] = key;

	dynamicKeyValues_->push_back( new Kernel::UserDynamicVariableProperties( id, key, filter, defaultVal ) );
}

void
Kernel::Environment::initDefineDynamicHandler( const char * id, const char * msg )
{
	initDefineDynamic( id,
										 Lang::THE_IDENTITY,
										 Helpers::newValHandle( new Lang::ExceptionWrapper< Exceptions::HandlerError >( id, msg ) ) );
}

Kernel::Environment::Environment( std::list< Kernel::Environment * > & garbageArea )
	: parent_( 0 ),
		bindings_( new Kernel::Environment::MapType ),
		values_( new std::vector< Kernel::VariableHandle >( ) ),
		dynamicKeyBindings_( new Kernel::Environment::MapType ),
		dynamicKeyValues_( new std::vector< DynamicVariableProperties * > ),
		stateBindings_( new Kernel::Environment::MapType ),
		states_( new std::vector< Kernel::StateHandle >( ) ),
		dynamicStateKeyBindings_( new Kernel::Environment::MapType ),
		dynamicStateKeyValues_( new std::vector< DynamicStateProperties * > ),
		functionBoundary_( false )
{
	garbageArea.push_back( this );
	++createdCount;
	++liveCount;
}

Kernel::Environment::Environment( std::list< Kernel::Environment * > & garbageArea, Environment * parent, MapType * bindings, const RefCountPtr< std::vector< VariableHandle > > & values, MapType * stateBindings, const RefCountPtr< std::vector< StateHandle > > & states )
	: parent_( parent ),
		bindings_( bindings ), values_( values ), dynamicKeyBindings_( 0 ),
		stateBindings_( stateBindings ), states_( states ), dynamicStateKeyBindings_( 0 ),
		functionBoundary_( false )
																 //, unitMap_( NullPtr< Kernel::Environment::UnitMapType >( ) )
{
	garbageArea.push_back( this );
	//	if( parent_ != 0 )
	//		{
	//			unitMap_ = parent->unitMap_;
	//		}
	++createdCount;
	++liveCount;
}

Kernel::Environment::~Environment( )
{
	clear( );
	if( dynamicKeyBindings_ != 0 )
		{
			if( parent_ == 0 )
				{
					/* The condition means that this is the global evironment, which created its own map.
					 */
					delete dynamicKeyBindings_;
				}
			/* However, the values will always be owned by the environment itself, and be defined whenever dynamicKeyBindings != 0
			 */
			for( std::vector< DynamicVariableProperties * >::iterator i = dynamicKeyValues_->begin( ); i != dynamicKeyValues_->end( ); ++i )
				{
					if( *i != 0 )
						{
							delete *i;
						}
				}
			delete dynamicKeyValues_;
		}
	if( stateBindings_ != 0 )
		{
			if( parent_ == 0 )
				{
					/* The condition means that this is the global evironment, which created its own map.
					 */
					delete stateBindings_;
				}
			/* However, the values will always be owned by the environment itself, and be defined whenever dynamicKeyBindings != 0
			 */
			for( std::vector< StateHandle >::iterator i = states_->begin( ); i != states_->end( ); ++i )
				{
					if( *i != 0 )
						{
							delete *i;
						}
				}
		}
	if( dynamicStateKeyBindings_ != 0 )
		{
			if( parent_ == 0 )
				{
					/* The condition means that this is the global evironment, which created its own map.
					 */
					delete dynamicStateKeyBindings_;
				}
			/* However, the values will always be owned by the environment itself, and be defined whenever dynamicKeyBindings != 0
			 */
			for( std::vector< DynamicStateProperties * >::iterator i = dynamicStateKeyValues_->begin( ); i != dynamicStateKeyValues_->end( ); ++i )
				{
					if( *i != 0 )
						{
							delete *i;
						}
				}
			delete dynamicStateKeyValues_;
		}
	--liveCount;
}

void
Kernel::Environment::setParent( Kernel::Environment * parent )
{
	parent_ = parent;
	//	unitMap_ = parent.unitMap_;
}

Kernel::Environment *
Kernel::Environment::getParent( )
{
	if( isBaseEnvironment( ) )
		{
			throw Exceptions::MiscellaneousRequirement( "Trying to find the parent of the top level environment." );
		}
	return parent_;
}

const Kernel::Environment *
Kernel::Environment::getParent( ) const
{
	if( isBaseEnvironment( ) )
		{
			throw Exceptions::MiscellaneousRequirement( "Trying to find the parent of the top level environment." );
		}
	return parent_;
}

void
Kernel::Environment::setupDynamicKeyVariables( MapType * dynamicKeyBindings )
{
	dynamicKeyBindings_ = dynamicKeyBindings;
	dynamicKeyValues_ = new std::vector< DynamicVariableProperties * >;
	size_t theSize = dynamicKeyBindings_->size( );
	dynamicKeyValues_->reserve( theSize );
	while( dynamicKeyValues_->size( ) < theSize )
		{
			dynamicKeyValues_->push_back( 0 );
		}
}

void
Kernel::Environment::setupDynamicStateKeyVariables( MapType * dynamicStateKeyBindings )
{
	dynamicStateKeyBindings_ = dynamicStateKeyBindings;
	dynamicStateKeyValues_ = new std::vector< DynamicStateProperties * >;
	size_t theSize = dynamicStateKeyBindings_->size( );
	dynamicStateKeyValues_->reserve( theSize );
	while( dynamicStateKeyValues_->size( ) < theSize )
		{
			dynamicStateKeyValues_->push_back( 0 );
		}
}

void
Kernel::Environment::activateFunctionBoundary( )
{
	functionBoundary_ = true;
}

void
Kernel::Environment::clear( )
{
	/* Strange that this is empty...
	 * The idea, though, is that the reference counting of this->values shall take care of deletion.
	 */
}

Ast::AnalysisEnvironment *
Kernel::Environment::newAnalysisEnvironment( ) const
{
	Ast::AnalysisEnvironment * res = new Ast::AnalysisEnvironment( Ast::theAnalysisEnvironmentList, 0, bindings_, stateBindings_ );
	if( dynamicKeyBindings_ != 0 )
		{
			res->setupDynamicKeyVariables( dynamicKeyBindings_ );
		}
	if( dynamicStateKeyBindings_ != 0 )
		{
			res->setupDynamicStateKeyVariables( dynamicStateKeyBindings_ );
		}

	return res;
}

void
Kernel::Environment::gcMark( Kernel::GCMarkedSet & marked )
{
	if( gcMarked_ )
		{
			return;
		}
	gcMarked_ = true;
	marked.insert( marked.begin( ), this );

	values_->clear( );
}

void
Kernel::Environment::collect( std::list< Kernel::Environment * > & garbageArea )
{
	for( std::list< Kernel::Environment * >::iterator i = garbageArea.begin( ); i != garbageArea.end( ); ++i )
		{
			(*i)->clear_gcMarked( );
		}

	Kernel::GCMarkedSet marked;
	for( std::list< Kernel::Environment * >::iterator i = garbageArea.begin( ); i != garbageArea.end( ); ++i )
		{
			(*i)->gcMark( marked );
		}

	for( std::list< Kernel::Environment * >::iterator i = garbageArea.begin( ); i != garbageArea.end( ); )
		{
			if( (*i)->gcMarked( ) )
				{
					++i;
					continue;
				}
			std::list< Kernel::Environment * >::iterator tmp = i;
			++i;
			delete *tmp;
			garbageArea.erase( tmp );
		}
}


Ast::AnalysisEnvironment::AnalysisEnvironment( PtrOwner_back_Access< std::list< Ast::AnalysisEnvironment * > > & deleter, const Ast::AnalysisEnvironment * parent, const MapType * bindings, const MapType * stateBindings )
	: parent_( parent ),
		bindings_( bindings ), dynamicKeyBindings_( 0 ),
		stateBindings_( stateBindings ), dynamicStateKeyBindings_( 0 ),
		functionBoundary_( false )
{
	deleter.push_back( this );
}

Ast::AnalysisEnvironment::~AnalysisEnvironment( )
{ }


const Ast::AnalysisEnvironment *
Ast::AnalysisEnvironment::getParent( ) const
{
	if( isBaseEnvironment( ) )
		{
			throw Exceptions::MiscellaneousRequirement( "Trying to find the parent of the top level analysis environment." );
		}
	return parent_;
}

void
Ast::AnalysisEnvironment::activateFunctionBoundary( )
{
	functionBoundary_ = true;
}

void
Ast::AnalysisEnvironment::setupDynamicKeyVariables( const MapType * dynamicKeyBindings )
{
	dynamicKeyBindings_ = dynamicKeyBindings;
}

void
Ast::AnalysisEnvironment::setupDynamicStateKeyVariables( const MapType * dynamicStateKeyBindings )
{
	dynamicStateKeyBindings_ = dynamicStateKeyBindings;
}

size_t
Ast::AnalysisEnvironment::findLocalVariablePosition( const Ast::SourceLocation & loc, const char * id ) const
{
	MapType::const_iterator i = bindings_->find( id );
	if( i == bindings_->end( ) )
		{
			throw Exceptions::InternalError( loc, "Environment::findLocalPosition failed" );
		}
	return i->second;
}

void
Kernel::Environment::define( size_t pos, const Kernel::VariableHandle & val )
{
	if( (*values_)[ pos ] != NullPtr< Kernel::Variable >( ) )
		{
			throw Exceptions::RedefiningLexical( reverseMapVariable( pos ) );
		}

	(*values_)[ pos ] = val;
}

Kernel::Environment::LexicalKey
Ast::AnalysisEnvironment::findLexicalVariableKey( const Ast::SourceLocation & loc, const char * id ) const
{
	MapType::const_iterator i = bindings_->find( id );
	if( i == bindings_->end( ) )
		{
			if( isBaseEnvironment( ) )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknown( loc, strrefdup( id ), Exceptions::LookupUnknown::VARIABLE ) );
					return Kernel::Environment::theMissingKey;
				}
			return parent_->findLexicalVariableKey( loc, id ).oneAbove( );
		}

	return LexicalKey( 0, i->second );
}

void
Kernel::Environment::lookup( const Kernel::Environment::LexicalKey & lexKey, Kernel::EvalState * evalState ) const
{
	const Environment * env = this;
	for( size_t i = lexKey.up_; i > 0; --i )
		{
			env = env->getParent( );
		}

	env->lookup( lexKey.pos_, evalState );
}

void
Kernel::Environment::lookup( size_t pos, Kernel::EvalState * evalState ) const
{
	Kernel::VariableHandle res = (*values_)[ pos ];
	if( res == NullPtr< Kernel::Variable >( ) )
		{
			throw Exceptions::UninitializedAccess( );
		}

	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( res, evalState );
}

Kernel::VariableHandle
Kernel::Environment::getVarHandle( const Kernel::Environment::LexicalKey & lexKey )
{
	Environment * env = this;
	for( size_t i = lexKey.up_; i > 0; --i )
		{
			env = env->getParent( );
		}

	return env->getVarHandle( lexKey.pos_ );
}

Kernel::VariableHandle
Kernel::Environment::getVarHandle( size_t pos )
{
	return (*values_)[ pos ];
}

Kernel::Environment::LexicalKey
Ast::AnalysisEnvironment::findLexicalTypeKey( const Ast::SourceLocation & loc, const char * id ) const
{
	MapType::const_iterator i = bindings_->find( id );
	if( i == bindings_->end( ) )
		{
			if( isBaseEnvironment( ) )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknown( loc, strrefdup( id ), Exceptions::LookupUnknown::TYPE ) );
					return Kernel::Environment::theMissingKey;
				}
			return parent_->findLexicalTypeKey( loc, id ).oneAbove( );
		}

	return LexicalKey( 0, i->second );
}


size_t
Ast::AnalysisEnvironment::findLocalStatePosition( const Ast::SourceLocation & loc, const char * id ) const
{
	MapType::const_iterator i = stateBindings_->find( id );
	if( i == stateBindings_->end( ) )
		{
			throw Exceptions::InternalError( loc, "Environment::findLocalStatePosition failed" );
		}
	return i->second;
}

Kernel::Environment::LexicalKey
Ast::AnalysisEnvironment::findLexicalStateKey( const Ast::SourceLocation & loc, const char * id ) const
{
	MapType::const_iterator i = stateBindings_->find( id );
	if( i == stateBindings_->end( ) )
		{
			if( isBaseEnvironment( ) )
				{
					Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknown( loc, strrefdup( id ), Exceptions::LookupUnknown::STATE ) );
					return Kernel::Environment::theMissingKey;
				}
			if( functionBoundary_ )
				{
					// If the state is not found at all, this will throw an error.
					parent_->findLexicalStateKey( loc, id ).oneAbove( );	// Ignore the result!
					// If no error is thrown, we inform the user that the state is outside a function boundary.
					Ast::theAnalysisErrorsList.push_back( new Exceptions::StateBeyondFunctionBoundary( loc, strrefdup( id ) ) );
					return Kernel::Environment::theMissingKey;
				}
			return parent_->findLexicalStateKey( loc, id ).oneAbove( );
		}

	return LexicalKey( 0, i->second );
}

void
Kernel::Environment::introduceState( size_t pos, Kernel::State * state )
{
	if( (*states_)[ pos ] != NullPtr< Kernel::State >( ) )
		{
			throw Exceptions::InternalError( "Better error message needed when a state is introduced more than once." );
			//			throw Exceptions::RedefiningLexical( reverseMap( pos ) );
		}

	(*states_)[ pos ] = state;
}

void
Kernel::Environment::freeze( size_t pos, Kernel::EvalState * evalState, const Ast::SourceLocation & loc )
{
	if( (*states_)[ pos ] == NullPtr< Kernel::State >( ) )
		{
			// This is a static inconsistency, so it should be detected before we reach here...
			throw Exceptions::FreezingUndefined( loc, reverseMapState( pos ) );
		}

	Kernel::StateHandle & state = (*states_)[ pos ];

	state->freeze( evalState, loc );
}

void
Kernel::Environment::peek( const LexicalKey & lexKey, Kernel::EvalState * evalState, const Ast::SourceLocation & loc )
{
	getStateHandle( lexKey )->peek( evalState, loc );
}

void
Kernel::Environment::tackOn( const LexicalKey & lexKey, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc )
{
	getStateHandle( lexKey )->tackOn( evalState, piece, callLoc );
}

Kernel::StateHandle
Kernel::Environment::getStateHandle( const LexicalKey & lexKey )
{
	Environment * env = this;
	for( size_t i = lexKey.up_; i > 0; --i )
		{
			env = env->getParent( );
		}

	return env->getStateHandle( lexKey.pos_ );
}

Kernel::StateHandle
Kernel::Environment::getStateHandle( size_t pos )
{
	Kernel::StateHandle res = (*states_)[ pos ];
	if( res == NullPtr< Kernel::State >( ) )
		{
			throw Exceptions::UninitializedAccess( );
		}
	return res;
}



size_t
Ast::AnalysisEnvironment::findLocalDynamicPosition( const Ast::SourceLocation & loc, const char * id ) const
{
	if( dynamicKeyBindings_ == 0 )
		{
			throw Exceptions::InternalError( "Environment::findLocalDynamicPosition called with dynamicKeyBindings_ == 0." );
		}
	MapType::const_iterator i = dynamicKeyBindings_->find( id );
	if( i == dynamicKeyBindings_->end( ) )
		{
			throw Exceptions::InternalError( loc, "Environment::findLocalDynamicPosition failed" );
		}
	return i->second;
}

void
Kernel::Environment::defineDynamic( const char * debugName, size_t pos, const RefCountPtr< const Lang::Function > & filter, const Kernel::VariableHandle & defaultVal )
{
	if( dynamicKeyValues_ == 0 )
		{
			throw Exceptions::InternalError( "Environment::defineDynamic called with dynamicKeyValues_ == 0." );
		}
	if( pos > dynamicKeyValues_->size( ) )
		{
			throw Exceptions::InternalError( "Environment::defineDynamic called with pos out of range." );
		}
	if( (*dynamicKeyValues_)[ pos ] != 0 )
		{
			throw Exceptions::RedefiningDynamic( reverseMapDynamic( pos ) );
		}

	(*dynamicKeyValues_)[ pos ] = new Kernel::UserDynamicVariableProperties( debugName,
																																					 Kernel::DynamicEnvironment::getFreshKey( ),
																																					 filter,
																																					 defaultVal );
}

Kernel::Environment::LexicalKey
Ast::AnalysisEnvironment::findLexicalDynamicKey( const Ast::SourceLocation & loc, const char * id ) const
{
	if( dynamicKeyBindings_ == 0 )
		{
			if( isBaseEnvironment( ) )
				{
					goto error;
				}
			return parent_->findLexicalDynamicKey( loc, id ).oneAbove( );
		}
	{
		MapType::const_iterator i = dynamicKeyBindings_->find( id );
		if( i == dynamicKeyBindings_->end( ) )
			{
				if( isBaseEnvironment( ) )
					{
						goto error;
					}
				return parent_->findLexicalDynamicKey( loc, id ).oneAbove( );
			}

		return LexicalKey( 0, i->second );
	}

 error:
	char * msg = new char[ strlen( id ) + 2 ];
	strcpy( msg, "@" );
	strcpy( msg + 1, id );
	Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknown( loc, RefCountPtr< const char >( msg ), Exceptions::LookupUnknown::DYNAMIC_VARIABLE ) );
	return Kernel::Environment::theMissingKey;

}

const Kernel::DynamicVariableProperties &
Kernel::Environment::lookupDynamicVariable( const LexicalKey & lexKey ) const
{
	const Environment * env = this;
	for( size_t i = lexKey.up_; i > 0; --i )
		{
			env = env->getParent( );
		}

	return env->lookupDynamicVariable( lexKey.pos_ );
}

const Kernel::DynamicVariableProperties &
Kernel::Environment::lookupDynamicVariable( size_t pos ) const
{
	const DynamicVariableProperties * res = (*dynamicKeyValues_)[ pos ];
	if( res == 0 )
		{
			throw Exceptions::UninitializedAccess( );
		}
	return *res;
}

size_t
Ast::AnalysisEnvironment::findLocalDynamicStatePosition( const Ast::SourceLocation & loc, const char * id ) const
{
	if( dynamicStateKeyBindings_ == 0 )
		{
			throw Exceptions::InternalError( "Environment::findLocalDynamicStatePosition called with dynamicStateKeyBindings_ == 0." );
		}
	MapType::const_iterator i = dynamicStateKeyBindings_->find( id );
	if( i == dynamicStateKeyBindings_->end( ) )
		{
			throw Exceptions::InternalError( loc, "Environment::findLocalDynamicStatePosition failed" );
		}
	return i->second;
}

void
Kernel::Environment::defineDynamicState( const char * debugName, size_t pos, Kernel::EvalState * evalState, Ast::StateReference * defaultState )
{
	if( dynamicStateKeyValues_ == 0 )
		{
			throw Exceptions::InternalError( "Environment::defineDynamicState called with dynamicStateKeyValues_ == 0." );
		}
	if( pos > dynamicStateKeyValues_->size( ) )
		{
			throw Exceptions::InternalError( "Environment::defineDynamicState called with pos out of range." );
		}
	if( (*dynamicStateKeyValues_)[ pos ] != 0 )
		{
			throw Exceptions::RedefiningDynamic( reverseMapDynamicState( pos ) );
		}

	(*dynamicStateKeyValues_)[ pos ] = new Kernel::UserDynamicStateProperties( debugName,
																																						 Kernel::DynamicEnvironment::getFreshKey( ),
																																						 evalState->env_,
																																						 evalState->dyn_, 
																																						 defaultState );
}


Kernel::Environment::LexicalKey
Ast::AnalysisEnvironment::findLexicalDynamicStateKey( const Ast::SourceLocation & loc, const char * id ) const
{
	if( dynamicStateKeyBindings_ == 0 )
		{
			return parent_->findLexicalDynamicStateKey( loc, id ).oneAbove( );
		}

	MapType::const_iterator i = dynamicStateKeyBindings_->find( id );
	if( i == dynamicStateKeyBindings_->end( ) )
		{
			if( isBaseEnvironment( ) )
				{
					char * msg = new char[ strlen( id ) + 2 ];
					strcpy( msg, "@#" );
					strcpy( msg + 1, id );
					Ast::theAnalysisErrorsList.push_back( new Exceptions::LookupUnknown( loc, RefCountPtr< const char >( msg ), Exceptions::LookupUnknown::DYNAMIC_STATE ) );
					return Kernel::Environment::theMissingKey;
				}
			return parent_->findLexicalDynamicStateKey( loc, id ).oneAbove( );
		}

	return LexicalKey( 0, i->second );
}

const Kernel::DynamicStateProperties &
Kernel::Environment::lookupDynamicState( const LexicalKey & lexKey ) const
{
	const Environment * env = this;
	for( size_t i = lexKey.up_; i > 0; --i )
		{
			env = env->getParent( );
		}

	return env->lookupDynamicState( lexKey.pos_ );
}

const Kernel::DynamicStateProperties &
Kernel::Environment::lookupDynamicState( size_t pos ) const
{
	const DynamicStateProperties * res = (*dynamicStateKeyValues_)[ pos ];
	if( res == 0 )
		{
			throw Exceptions::UninitializedAccess( );
		}
	return *res;
}

const char *
Kernel::Environment::reverseMapVariable( size_t pos ) const
{
	for( MapType::const_iterator i = bindings_->begin( ); i != bindings_->end( ); ++i )
		{
			if( i->second == pos )
				{
					return i->first;
				}
		}
	throw Exceptions::InternalError( "Environment::reverseMapVariable failure." );
}

const char *
Kernel::Environment::reverseMapDynamic( size_t pos ) const
{
	for( MapType::const_iterator i = dynamicKeyBindings_->begin( ); i != dynamicKeyBindings_->end( ); ++i )
		{
			if( i->second == pos )
				{
					return i->first;
				}
		}
	throw Exceptions::InternalError( "Environment::reverseMapDynamic failure." );
}

const char *
Kernel::Environment::reverseMapState( size_t pos ) const
{
	for( MapType::const_iterator i = stateBindings_->begin( ); i != stateBindings_->end( ); ++i )
		{
			if( i->second == pos )
				{
					return i->first;
				}
		}
	throw Exceptions::InternalError( "Environment::reverseMapState failure." );
}

const char *
Kernel::Environment::reverseMapDynamicState( size_t pos ) const
{
	for( MapType::const_iterator i = dynamicStateKeyBindings_->begin( ); i != dynamicStateKeyBindings_->end( ); ++i )
		{
			if( i->second == pos )
				{
					return i->first;
				}
		}
	throw Exceptions::InternalError( "Environment::reverseMapDynamicState failure." );
}


size_t
Kernel::Environment::size( ) const
{
	return bindings_->size( );
}


void
Kernel::Environment::print( std::ostream & os ) const
{
	std::set< MapType::key_type > shadowed;
	recursivePrint( os, & shadowed );
}

size_t
Kernel::Environment::recursivePrint( std::ostream & os, std::set< MapType::key_type > * shadowed ) const
{
	std::set< MapType::key_type > shadowedBefore( *shadowed );

	size_t depth = 0;
	if( ! isBaseEnvironment( ) )
		{
			for( MapType::const_iterator i = bindings_->begin( ); i != bindings_->end( ); ++i )
				{
					shadowed->insert( shadowed->begin( ), i->first );
				}
			depth = parent_->recursivePrint( os, shadowed ) + 1;
		}

	std::string indentation = string( depth, ' ' );

	os << indentation << "--------------------" << endl ;
	if( ! isBaseEnvironment( ) )
		{
			for( MapType::const_iterator i = bindings_->begin( ); i != bindings_->end( ); ++i )
				{
					os << indentation << i->second ;
					if( shadowedBefore.find( i->first ) != shadowedBefore.end( ) )
						{
							os << "#" ;
						}
					else
						{
							os << " " ;
						}
					os << " " << i->first << " : " ;
					if( (*values_)[ i->second ] == NullPtr< Kernel::Variable >( ) )
						{
							os << "< Uninitialized >" ;
						}
					else if( (*values_)[ i->second ]->isThunk( ) )
						{
							os << "< thunk >" ;
						}
					else if( dynamic_cast< const Lang::Instance * >( (*values_)[ i->second ]->getUntyped( ).getPtr( ) ) == 0 )
						{
							(*values_)[ i->second ]->getUntyped( )->show( os );
						}
					else
						{
							os << "..." ;
						}
					os << endl ;
				}
		}
	else
		{
			os << indentation << "<< global env >>" << endl ;
		}

	os << indentation << "--------------------" << endl ;

	return depth;
}

bool
Kernel::Environment::isProceduralParentOf( Kernel::PassedEnv child ) const
{
	for( const Environment * current = child; current != 0; current = current->parent_ )
		{
			if( current == this )
				{
					return true;
				}
			if( current->functionBoundary_ )
				{
					return false;
				}
		}
	return false;
}
