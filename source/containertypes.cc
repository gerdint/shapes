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

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "consts.h"
#include "angleselect.h"
#include "astvar.h"
#include "astclass.h"
#include "globals.h"

using namespace Shapes;


Lang::SingleList::SingleList( )
{ }

Lang::SingleList::~SingleList( )
{ }

RefCountPtr< const Lang::Class > Lang::SingleList::TypeID( new Lang::SystemFinalClass( strrefdup( "SingleList" ) ) );
TYPEINFOIMPL( SingleList );

Kernel::VariableHandle
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
	if( strcmp( fieldId, "foldsl" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::SingleListMethodFoldSL( typedSelfRef ) );
		}
	if( strcmp( fieldId, "foldsr" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::SingleListMethodFoldSR( typedSelfRef ) );
		}
	throw Exceptions::NonExistentMember( getTypeName( ), fieldId );
}


namespace Shapes
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
		virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
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
		Kernel::VariableHandle car_;
		RefCountPtr< const Lang::Function > op_;
		Kernel::PassedDyn dyn_;
		Kernel::ContRef cont_;
	public:
		SingleFoldRCont( const Kernel::VariableHandle & car, const RefCountPtr< const Lang::Function > & op, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
			: Kernel::Continuation( traceLoc ), car_( car ), op_( op ), dyn_( dyn ), cont_( cont )
		{ }
		virtual ~SingleFoldRCont( ) { }
		virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
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

	class SingleFoldSLCont : public Kernel::Continuation
	{
		RefCountPtr< const Lang::SingleList > cdr_;
		RefCountPtr< const Lang::Function > op_;
		Kernel::StateHandle state_;
		Kernel::PassedDyn dyn_;
		Kernel::ContRef cont_;
	public:
		SingleFoldSLCont( const RefCountPtr< const Lang::SingleList > & cdr, const RefCountPtr< const Lang::Function > & op, Kernel::StateHandle state, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
			: Kernel::Continuation( traceLoc ), cdr_( cdr ), op_( op ), state_( state ), dyn_( dyn ), cont_( cont )
		{ }
		virtual ~SingleFoldSLCont( ) { }
		virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
		{
			evalState->dyn_ = dyn_;
			evalState->cont_ = cont_;
			cdr_->foldsl( evalState, op_, val, state_, traceLoc_ );
		}
		virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
		{
			trace->push_front( Kernel::Continuation::BackTraceElem( this, "singly linked list's foldsl" ) );
			cont_->backTrace( trace );
		}
		virtual void gcMark( Kernel::GCMarkedSet & marked )
		{
			const_cast< Lang::SingleList * >( cdr_.getPtr( ) )->gcMark( marked );
			const_cast< Lang::Function * >( op_.getPtr( ) )->gcMark( marked );
			state_->gcMark( marked );
			dyn_->gcMark( marked );
			cont_->gcMark( marked );
		}
	};

	class SingleFoldSRCont : public Kernel::Continuation
	{
		Kernel::VariableHandle car_;
		RefCountPtr< const Lang::Function > op_;
		Kernel::StateHandle state_;
		Kernel::PassedDyn dyn_;
		Kernel::ContRef cont_;
	public:
		SingleFoldSRCont( const Kernel::VariableHandle & car, const RefCountPtr< const Lang::Function > & op, Kernel::StateHandle state, const Kernel::PassedDyn & dyn, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
			: Kernel::Continuation( traceLoc ), car_( car ), op_( op ), state_( state ),dyn_( dyn ), cont_( cont )
		{ }
		virtual ~SingleFoldSRCont( ) { }
		virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const
		{
			evalState->dyn_ = dyn_;
			evalState->cont_ = cont_;
			op_->call( op_, evalState, val, car_, state_, traceLoc_ );
		}
		virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
		{
			trace->push_front( Kernel::Continuation::BackTraceElem( this, "singly linked list's foldsr" ) );
			cont_->backTrace( trace );
		}
		virtual void gcMark( Kernel::GCMarkedSet & marked )
		{
			car_->gcMark( marked );
			const_cast< Lang::Function * >( op_.getPtr( ) )->gcMark( marked );
			state_->gcMark( marked );
			dyn_->gcMark( marked );
			cont_->gcMark( marked );
		}
	};

	}
}


Lang::SingleListPair::SingleListPair( const Kernel::VariableHandle & car, const RefCountPtr< const Lang::SingleList > & cdr )
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
Lang::SingleListPair::foldl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const
{
	evalState->cont_ = Kernel::ContRef( new Kernel::SingleFoldLCont( cdr_, op, evalState->dyn_, evalState->cont_, callLoc ) );

	op->call( op, evalState, nullResult, car_, callLoc );
}

void
Lang::SingleListPair::foldr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const
{
	evalState->cont_ = Kernel::ContRef( new Kernel::SingleFoldRCont( car_, op, evalState->dyn_, evalState->cont_, callLoc ) );

	cdr_->foldr( evalState, op, nullResult, callLoc );
}

void
Lang::SingleListPair::foldsl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const
{
	evalState->cont_ = Kernel::ContRef( new Kernel::SingleFoldSLCont( cdr_, op, state, evalState->dyn_, evalState->cont_, callLoc ) );

	op->call( op, evalState, nullResult, car_, state, callLoc );
}

void
Lang::SingleListPair::foldsr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const
{
	evalState->cont_ = Kernel::ContRef( new Kernel::SingleFoldSRCont( car_, op, state, evalState->dyn_, evalState->cont_, callLoc ) );

	cdr_->foldsr( evalState, op, nullResult, state, callLoc );
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
Lang::SingleListNull::foldl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const
{
	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( nullResult,
										evalState );
}

void
Lang::SingleListNull::foldr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const
{
	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( nullResult,
										evalState );
}

void
Lang::SingleListNull::foldsl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const
{
	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( nullResult,
										evalState );
}

void
Lang::SingleListNull::foldsr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const
{
	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( nullResult,
										evalState );
}


Lang::ConsPair::ConsPair( const Kernel::VariableHandle & car, const Kernel::VariableHandle & cdr )
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

Kernel::VariableHandle
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

Lang::SingleListMethodFoldSL::SingleListMethodFoldSL( RefCountPtr< const Lang::SingleList > self )
	: Lang::SingleListMethodBase( self,
																new Kernel::EvaluatedFormals( strdup( Kernel::MethodId( Lang::SingleList::TypeID, "foldsl" ).prettyName( ).getPtr( ) ) ) )
{
	formals_->appendEvaluatedCoreFormal( "op", Kernel::THE_SLOT_VARIABLE, true );
	formals_->appendEvaluatedCoreFormal( "nullRes", Kernel::THE_SLOT_VARIABLE, false );
	formals_->appendCoreStateFormal( "state" );
}

Lang::SingleListMethodFoldSL::~SingleListMethodFoldSL( )
{ }

void
Lang::SingleListMethodFoldSL::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	args.applyDefaults( );

	self_->foldsl( evalState,
								 Helpers::down_cast_CoreArgument< const Lang::Function >( "< core method foldl >", args, 0, callLoc ),
								 args.getHandle( 1 ),
								 args.getState( 0 ),
								 callLoc );
}

Lang::SingleListMethodFoldSR::SingleListMethodFoldSR( RefCountPtr< const Lang::SingleList > self )
	: Lang::SingleListMethodBase( self,
																new Kernel::EvaluatedFormals( strdup( Kernel::MethodId( Lang::SingleList::TypeID, "foldsr" ).prettyName( ).getPtr( ) ) ) )
{
	formals_->appendEvaluatedCoreFormal( "op", Kernel::THE_SLOT_VARIABLE, true );
	formals_->appendEvaluatedCoreFormal( "nullRes", Kernel::THE_SLOT_VARIABLE, false );
	formals_->appendCoreStateFormal( "state" );
}

Lang::SingleListMethodFoldSR::~SingleListMethodFoldSR( )
{ }

void
Lang::SingleListMethodFoldSR::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	args.applyDefaults( );

	self_->foldsr( evalState,
								 Helpers::down_cast_CoreArgument< const Lang::Function >( "< core method foldr >", args, 0, callLoc ),
								 args.getHandle( 1 ),
								 args.getState( 0 ),
								 callLoc );

}

Lang::Structure::Structure( const Ast::ArgListExprs * argList, const RefCountPtr< const Lang::SingleList > & values, bool argListOwner )
	: argListOwner_( argListOwner ), argList_( argList ), values_( values )
{ }

Lang::Structure::~Structure( )
{
	if( argListOwner_ )
		{
			delete argList_;
		}
}

RefCountPtr< const Lang::Class > Lang::Structure::TypeID( new Lang::SystemFinalClass( strrefdup( "Union" ) ) );
TYPEINFOIMPL( Structure );

Kernel::VariableHandle
Lang::Structure::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const
{
	return argList_->findNamed( values_, fieldID );
}

Kernel::VariableHandle
Lang::Structure::getPosition( size_t pos, const RefCountPtr< const Lang::Value > & selfRef ) const
{
	return argList_->getOrdered( values_, pos );
}

RefCountPtr< const Lang::Structure >
Lang::Structure::getSink( size_t consumedArguments ) const
{
	if( argList_->orderedExprs_->size( ) <= consumedArguments )
		{
			return Lang::THE_EMPTY_STRUCT;
		}

	static std::vector< const Ast::ArgListExprs * > argLists;
	size_t resSize = argList_->orderedExprs_->size( ) - consumedArguments;
	if( resSize >= argLists.size( ) )
		{
			argLists.reserve( resSize + 1 );
			while( argLists.size( ) <= resSize )
				{
					argLists.push_back( new Ast::ArgListExprs( argLists.size( ) ) );
				}
		}

	RefCountPtr< const Lang::SingleList > resValues = values_;
	try
		{
			for( size_t i = 0; i < consumedArguments; ++i )
				{
					resValues = Helpers::try_cast_CoreArgument< const Lang::SingleListPair >( resValues )->cdr_;
				}
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			throw Exceptions::InternalError( "When constructing the sink, there was not enough arguments." );
		}
	return RefCountPtr< const Lang::Structure >( new Lang::Structure( argLists[ resSize ], resValues, false ) );
}

void
Lang::Structure::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::SingleList * >( values_.getPtr( ) )->gcMark( marked );
}


Kernel::StructureFactory::StructureFactory( const std::list< const char * > & fields )
{
	init( fields );
}

Kernel::StructureFactory::StructureFactory( const char * field1 )
{
	std::list< const char * > fields;
	fields.push_back( field1 );
	init( fields );
}

Kernel::StructureFactory::StructureFactory( const char * field1, const char * field2 )
{
	std::list< const char * > fields;
	fields.push_back( field1 );
	fields.push_back( field2 );
	init( fields );
}

Kernel::StructureFactory::StructureFactory( const char * field1, const char * field2, const char * field3 )
{
	std::list< const char * > fields;
	fields.push_back( field1 );
	fields.push_back( field2 );
	fields.push_back( field3 );
	init( fields );
}

Kernel::StructureFactory::StructureFactory( const char * field1, const char * field2, const char * field3, const char * field4 )
{
	std::list< const char * > fields;
	fields.push_back( field1 );
	fields.push_back( field2 );
	fields.push_back( field3 );
	fields.push_back( field4 );
	init( fields );
}

void
Kernel::StructureFactory::init( const std::list< const char * > & fields )
{
	Ast::ArgListExprs * tmp = new Ast::ArgListExprs( false );
	typedef typeof fields ListType;
	for( ListType::const_iterator i = fields.begin( ); i != fields.end( ); ++i )
		{
			(*tmp->namedExprs_)[ *i ] = 0;
			typedef typeof values_ MapType;
			values_.insert( MapType::value_type( *i, Kernel::THE_VOID_VARIABLE ) );
		}
	argList_ = tmp;
}

void
Kernel::StructureFactory::clear( )
{
	typedef typeof values_ MapType;
	for( MapType::iterator i = values_.begin( ); i != values_.end( ); ++i )
		{
			i->second = Kernel::THE_VOID_VARIABLE;
		}
}

void
Kernel::StructureFactory::set( const char * field, const Kernel::VariableHandle & value )
{
	typedef typeof values_ MapType;
	MapType::iterator i = values_.find( field );
	if( i == values_.end( ) )
		{
			throw Exceptions::InternalError( "Kernel::StructureFactory::set: Field was not declared." );
		}
	i->second = value;
}

RefCountPtr< const Lang::Structure >
Kernel::StructureFactory::build( )
{
	RefCountPtr< const Lang::SingleList > tmp = Lang::THE_CONS_NULL;
	typedef typeof values_ MapType;
	for( MapType::iterator i = values_.begin( ); i != values_.end( ); ++i )
		{
			tmp = RefCountPtr< const Lang::SingleList >( new Lang::SingleListPair( i->second, tmp ) );
		}
	return RefCountPtr< const Lang::Structure >( new Lang::Structure( argList_, tmp ) );
}
