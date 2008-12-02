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

#include "shapestypes.h"
#include "shapesexceptions.h"
#include "astexpr.h"
#include "globals.h"
#include "consts.h"
#include "angleselect.h"
#include "astvar.h"
#include "astclass.h"
#include "autoonoff.h"
#include "shapescore.h"
#include "astfun.h"
#include "methodbase.h"

#include <ctype.h>
#include <stack>
#include <string>

using namespace Shapes;
using namespace std;


Kernel::MethodId::MethodId( const RefCountPtr< const Lang::Class > & _myClass, const char * _msg )
	: myClass( _myClass ), msg( _msg )
{ }

Kernel::MethodId::~MethodId( )
{ }

const char *
Kernel::MethodId::getIdentifier( ) const
{
	return msg;
}


Lang::Instance::Instance( Kernel::PassedEnv _env, Kernel::PassedEnv _privateEnv, RefCountPtr< const Lang::Class > _myClass, bool _protectedAccess, const Kernel::PassedDyn & _my_dyn )
	: env( _env ), privateEnv( _privateEnv ), warm2D( 0 ), warm3D( 0 ), my_dyn( _my_dyn ), myClass( _myClass ), prepared( false ), protectedAccess( _protectedAccess )
{ }

DISPATCHIMPL( Instance );

Lang::Instance::~Instance( )
{ }

void
Lang::Instance::gcMark( Kernel::GCMarkedSet & marked )
{
	env->gcMark( marked );
	privateEnv->gcMark( marked );
	my_dyn->gcMark( marked );
}


void
Lang::Instance::prepare( Kernel::EvalState * evalState )
{
	if( prepared )
		{
			return;
		}
	prepared = true;
	{
		for( std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > >::iterator i = parents->begin( ); i != parents->end( ); ++i )
			{
				i->second->prepare( evalState );
			}
	}

	myClass->prepareInstance( evalState, privateEnv );
}

Kernel::VariableHandle
Lang::Instance::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & dummySelfRef ) const
{
	if( ! myClass->getFinal( ) )
		{
			throw Exceptions::IllegalFinalReference( myClass->getPrettyName( ), fieldID );
		}

	return getLocalField( fieldID );
}

Kernel::VariableHandle
Lang::Instance::getLocalField( const char * fieldID ) const
{
	if( protectedAccess )
		{
			if( myClass->isInProtectedGetSet( fieldID ) )
				{
					throw Exceptions::NotImplemented( "Instance::getLocalField" );
					//					Kernel::Environment::LexicalKey key = env->findLexicalVariableKey( Ast::THE_UNKNOWN_LOCATION, fieldID );
					//					return env->getVarHandle( key );
				}
			throw Exceptions::NonExistentMember( myClass->getPrettyName( ), fieldID );
		}
	else
		{
			if( myClass->isInPublicGetSet( fieldID ) )
				{
					throw Exceptions::NotImplemented( "Instance::getLocalField" );
					//					Kernel::Environment::LexicalKey key = env->findLexicalVariableKey( Ast::THE_UNKNOWN_LOCATION, fieldID );
					//					return env->getVarHandle( key );
				}
			if( myClass->isInProtectedGetSet( fieldID ) )
				{
					throw Exceptions::ProtectedMemberPublicScope( myClass->getPrettyName( ), fieldID );
				}
			throw Exceptions::NonExistentMember( myClass->getPrettyName( ), fieldID );
		}
}

void
Lang::Instance::tackOn( const char * fieldID, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc )
{
	if( ! myClass->getFinal( ) )
		{
			throw Exceptions::IllegalFinalReference( myClass->getPrettyName( ), fieldID );
		}

	tackOnLocal( fieldID, evalState, piece, callLoc );
}

void
Lang::Instance::tackOnLocal( const char * fieldID, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc )
{
	if( protectedAccess )
		{
			if( myClass->isInProtectedSetSet( fieldID ) )
				{
					throw Exceptions::NotImplemented( "Instance::tackOnLocal" );
					//					Kernel::Environment::LexicalKey key = env->findLexicalVariableKey( Ast::THE_UNKNOWN_LOCATION, fieldID );
					//					env->tackOn( key, evalState, piece, callLoc );
					return;
				}
			if( myClass->isInProtectedGetSet( fieldID ) )
				{
					throw Exceptions::MemberNotAssignable( myClass->getPrettyName( ), fieldID, Interaction::PROTECTED_SCOPE_NAME );
				}
			throw Exceptions::NonExistentMember( myClass->getPrettyName( ), fieldID );
		}
	else
		{
			if( myClass->isInPublicSetSet( fieldID ) )
				{
					throw Exceptions::NotImplemented( "Instance::tackOnLocal" );
					//					Kernel::Environment::LexicalKey key = env->findLexicalVariableKey( Ast::THE_UNKNOWN_LOCATION, fieldID );
					//					env->tackOn( key, evalState, piece, callLoc );
					return;
				}
			if( myClass->isInPublicGetSet( fieldID ) )
				{
					throw Exceptions::MemberNotAssignable( myClass->getPrettyName( ), fieldID, Interaction::PUBLIC_SCOPE_NAME );
				}
			if( myClass->isInProtectedGetSet( fieldID ) )
				{
					throw Exceptions::ProtectedMemberPublicScope( myClass->getPrettyName( ), fieldID );
				}
			throw Exceptions::NonExistentMember( myClass->getPrettyName( ), fieldID );
		}
}

RefCountPtr< const Lang::Function >
Lang::Instance::getMethod( Kernel::MethodId fieldID ) const
{
	typedef typeof methodTable MethodMapType;
	MethodMapType::iterator i = methodTable.find( fieldID );
	if( i != methodTable.end( ) )
		{
			return i->second;
		}

	const RefCountPtr< const Lang::Class > & defClass = myClass->getMethodDefinitionClass( fieldID );

	RefCountPtr< const Lang::Function > fun = NullPtr< const Lang::Function >( );

	if( defClass == myClass )
		{
			fun = getLocalMethod( fieldID );
		}
	else
		{
			typedef typeof *parents ParentMapType;
			ParentMapType::const_iterator p = parents->find( defClass );
			if( p == parents->end( ) )
				{
					throw Exceptions::InternalError( "The class defining a method was not found among the parents." );
				}
			fun = p->second->getLocalMethod( fieldID );
		}

	methodTable.insert( i, MethodMapType::value_type( fieldID, fun ) );

	return fun;
}

RefCountPtr< const Lang::Function >
Lang::Instance::getLocalMethod( Kernel::MethodId fieldID ) const
{

	if( fieldID.getClass( ) == myClass )
		{
			Kernel::VariableHandle untypedFun = getLocalField( fieldID.getIdentifier( ) );
			return untypedFun->getVal< const Lang::Function >( "getLocalMethod:	The user was (almost) able to retrieve member data from super instance." );
		}

	typedef typeof overrides ClassMapType;
	ClassMapType::const_iterator i = overrides.find( fieldID.getClass( ) );
	if( i == overrides.end( ) )
		{
			throw Exceptions::NoSuchLocalMethod( myClass, fieldID );
		}

	typedef typeof i->second IdMapType;

	IdMapType::const_iterator j = i->second.find( fieldID.getIdentifier( ) );
	if( j == i->second.end( ) )
		{
			throw Exceptions::NoSuchLocalMethod( myClass, fieldID );
		}
	return j->second;
}

RefCountPtr< const Lang::Instance >
Lang::Instance::superReference( RefCountPtr< const Lang::Class > parent ) const
{
	typedef typeof *parents ParentMapType;
	ParentMapType::const_iterator p = parents->find( parent );
	if( p == parents->end( ) )
		{
			throw Exceptions::SuperReferenceClassNotParent( myClass, parent );
		}
	return p->second;
}

RefCountPtr< const Lang::Geometric2D >
Lang::Instance::transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	return RefCountPtr< const Lang::Geometric2D >( new Lang::TransformedInstance( tf, self.down_cast< const Lang::Instance >( ) ) );
}

void
Lang::Instance::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
	if( ! myClass->method_isa( Lang::Drawable2D::TypeID ) )
		{
			throw Exceptions::InternalError( "Invoking Instance::shipout on a non Drawable object should not be possible" );
		}

	if( warm2D == 0 )
		{
			throw Exceptions::InternalError( "Instance::shipout: warm2D == 0." );
		}

	/*
	RefCountPtr< const Lang::Value > val = getMethod( Kernel::MethodId( Lang::Drawable2D::TypeID, Shapes::MESSAGE_DRAWABLE_DRAW_ID ) )->call( dstgroup, my_pdfo, & stateCopy, Kernel::EMPTY_ARGLIST );
	if( dynamic_cast< const Lang::Void * >( val.getPtr( ) ) == 0 )
		{
			throw Exceptions::TypeMismatch( val->getTypeName( ), Lang::Void::staticTypeName( ) );
		}
	*/

	warm2D->getPile( )->shipout( os, pdfState, tf );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::Instance::bbox( ) const
{
	if( ! myClass->method_isa( Lang::Drawable2D::TypeID ) )
		{
			throw Exceptions::InternalError( "Invoking Instance::shipout on a non Drawable object should not be possible" );
		}

	if( warm2D == 0 )
		{
			throw Exceptions::InternalError( "Instance::shipout: warm2D == 0." );
		}

	return warm2D->getPile( )->bbox( );
}

RefCountPtr< const char >
Lang::Instance::staticTypeName( )
{
	return strrefdup( "<Instance>" );
}

const RefCountPtr< const Lang::Class > &
Lang::Instance::getClass( ) const
{
	return myClass;
}

void
Lang::Instance::show( std::ostream & os ) const
{
	os << "Instance of class " << myClass->getPrettyName( ).getPtr( ) ;
}


Lang::TransformedInstance::TransformedInstance( const Lang::Transform2D & _tf, const RefCountPtr< const Lang::Instance > & _obj )
	: tf( _tf ), obj( _obj )
{ }

Lang::TransformedInstance::~TransformedInstance( )
{ }

Kernel::VariableHandle
Lang::TransformedInstance::getField( const char * fieldID, const RefCountPtr< const Lang::Value > & dummySelfRef ) const
{
	if( obj->getClass( )->isInTransformingSet( fieldID ) )
		{
			Kernel::VariableHandle argUntyped = obj->getField( fieldID, obj );
			typedef const Lang::Geometric2D ArgType;
			RefCountPtr< ArgType > arg = argUntyped->getVal< ArgType >( "<inside transformed instance getField>" );
			return Kernel::VariableHandle( new Kernel::Variable( arg->transformed( tf, arg ) ) );
		}
	return obj->getField( fieldID, obj );
}

RefCountPtr< const Lang::Function >
Lang::TransformedInstance::getMethod( Kernel::MethodId fieldID ) const
{
	/* Fetch the method first to make sure it exists, then see whether it is transforming.
	 */
	RefCountPtr< const Lang::Function > arg = obj->getMethod( fieldID );
	if( arg->isTransforming( ) )
		{
			return RefCountPtr< const Lang::Function >( new Lang::TransformedFunction2D( tf, arg ) );
		}
	return arg;
}

RefCountPtr< const Lang::Geometric2D >
Lang::TransformedInstance::transformed( const Lang::Transform2D & tf2, const RefCountPtr< const Lang::Geometric2D > & self ) const
{
	return RefCountPtr< const Lang::Geometric2D >( new Lang::TransformedInstance( Lang::Transform2D( tf2, tf ) , obj ) );
}


void
Lang::TransformedInstance::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tfIn ) const
{
	obj->shipout( os, pdfState, Lang::Transform2D( tfIn, tf ) );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::TransformedInstance::bbox( ) const
{
	return obj->bbox( )->elementaryTransformed( tf );
}

RefCountPtr< const Lang::Class > Lang::TransformedInstance::TypeID( new Lang::SystemFinalClass( strrefdup( "TransformedInstance" ) ) );
TYPEINFOIMPL( TransformedInstance );

void
Lang::TransformedInstance::show( std::ostream & os ) const
{
	os << "Transformed instance.	The class is not available in this implementation." ;
}

void
Lang::TransformedInstance::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Instance * >( obj.getPtr( ) )->gcMark( marked );
}

RefCountPtr< const char >
Kernel::MethodId::prettyName( ) const
{
	string res = myClass->getPrettyName( ).getPtr( );
	res += "#";
	res += msg;
	return strrefdup( res.c_str( ) );
}

bool
Kernel::operator < ( const Kernel::MethodId & mid1, const Kernel::MethodId & mid2 )
{
	if( mid1.myClass < mid2.myClass )
		{
			return true;
		}
	if( mid1.myClass > mid2.myClass )
		{
			return false;
		}
	return strcmp( mid1.msg, mid2.msg ) < 0;
}


Lang::Class::Class( RefCountPtr< const char > _prettyName )
	: prettyName( _prettyName ), selfRef( NullPtr< Lang::Class >( ) ), isFinal( false )
{ }

Lang::Class::~Class( )
{
	/* Knowing that each class is reference counted, the destructor is being called as an effect
	 * of the reference count reaching zero.	Thus, the self reference must simply be detached
	 * from this object, to avoid cyclic destruction.
	 */
	selfRef.abortPtr( );
}

void
Lang::Class::setSelfRef( RefCountPtr< const Lang::Class > _selfRef ) const
{
	selfRef = _selfRef; // selfRef is mutable!
	/* Don't count this cyclic reference!
	 */
	--*selfRef.getCounterPtr( );
}

RefCountPtr< const Lang::Class > Lang::Class::TypeID( new Lang::MetaClass( ) );
TYPEINFOIMPL( Class );

RefCountPtr< const char >
Lang::Class::getPrettyName( ) const
{
	return prettyName;
}

bool
Lang::Class::isInPublicGetSet( const char * field ) const
{
	return false;
}

bool
Lang::Class::isInPublicSetSet( const char * field ) const
{
	return false;
}

bool
Lang::Class::isInProtectedGetSet( const char * field ) const
{
	return false;
}

bool
Lang::Class::isInProtectedSetSet( const char * field ) const
{
	return false;
}

bool
Lang::Class::isInTransformingSet( const char * field ) const
{
	return false;
}

Kernel::VariableHandle
Lang::Class::getMethod( const RefCountPtr< const Lang::Value > & self, const char * methodID ) const
{
	throw Exceptions::InternalError( "Method request not defined for this type of class." );
}

RefCountPtr< const Lang::Function >
Lang::Class::getMutator( const char * mutatorID ) const
{
	throw Exceptions::InternalError( "Mutator request not defined for this type of class." );
}

void
Lang::Class::showAbstractSet( std::ostream & os ) const
{
	os << "{" ;
	typedef typeof abstractSet SetType;
	for( SetType::const_iterator i = abstractSet.begin( ); i != abstractSet.end( ); ++i )
		{
			os << " " << i->prettyName( ) ;
		}
	os << " }" ;
}


void
Lang::Class::show( std::ostream & os ) const
{
	os << "Pretty-name: " << prettyName.getPtr( ) ;

	if( ! abstractSet.empty( ) )
		{
			os << ", +" ;
		}
	else
		{
			os << ", -" ;
		}
	os << "abstract" ;

	if( isFinal )
		{
			os << ", +" ;
		}
	else
		{
			os << ", -" ;
		}
	os << "final" ;

	{
		bool isRepeatable = false;
		try
			{
				isRepeatable = this->isRepeatableBase( );
			}
		catch( const Exceptions::MiscellaneousRequirement & ball )
			{
				/* Probably something saying that MetaClass is not repeatable... */
			}

		if( isRepeatable )
			{
				os << ", +" ;
			}
		else
			{
				os << ", -" ;
			}
		os << "repeatable" ;
	}
}

Kernel::VariableHandle
Lang::Class::getField( const char * fieldId, const RefCountPtr< const Lang::Value > & dummySelfRef ) const
{
	if( strcmp( fieldId, "new" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::ClassMethodNew( selfRef ) );
		}
	if( strcmp( fieldId, "isa" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::ClassMethodIsa( selfRef ) );
		}
	if( strcmp( fieldId, "name" ) == 0 )
		{
			return Helpers::newValHandle( new Lang::String( prettyName ) );
		}
	throw Exceptions::NonExistentMember( getTypeName( ), fieldId );
}

bool
Lang::Class::getFinal( ) const
{
	return isFinal;
}

const Lang::Class::MessageMapType &
Lang::Class::getMessageMap( ) const
{
	return messageMap;
}

const RefCountPtr< const Lang::Class > &
Lang::Class::getMethodDefinitionClass( const Kernel::MethodId & method ) const
{
	MessageMapType::const_iterator i = messageMap.find( method );
	if( i == messageMap.end( ) )
		{
			throw Exceptions::NoSuchMethod( selfRef, method );
		}
	if( i->second.empty( ) )
		{
			throw Exceptions::InternalError( "Asking for class defining abstract method." );
		}
	return *( i->second.begin( ) );
}


void
Lang::Class::findParents( std::set< RefCountPtr< const Lang::Class > > * _allParents, std::set< RefCountPtr< const Lang::Class > > * _multiParents ) const
{
	typedef typeof *_allParents SetType;
	SetType::const_iterator i = _allParents->find( selfRef );
	if( i != _allParents->end( ) )
		{
			_multiParents->insert( selfRef );
		}
	else
		{
			_allParents->insert( i, selfRef );
		}
}

void
Lang::Class::prepareInstance( Kernel::EvalState * evalState, Kernel::PassedEnv privateEnv ) const
{
	/* Only UserClass instances do any preparations. */
}

DISPATCHIMPL( Class );



Lang::Object::Object( )
	: Lang::Class( strrefdup( "Object" ) ), dummyEnv( NullPtr< Kernel::Environment >( ) )
{ }

Lang::Object::~Object( )
{ }

Kernel::ValueRef
Lang::Object::method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	const size_t ARITY = 0;
	CHECK_ARITY( args, ARITY, Kernel::MethodId( Lang::Class::TypeID, "new" ).prettyName( ) );

	return RefCountPtr< const Lang::Geometric2D >( new Lang::Instance( dummyEnv, dummyEnv, selfRef, false, evalState->dyn_ ) );
}

bool
Lang::Object::method_isa( RefCountPtr< const Lang::Class > T ) const
{
	return T == selfRef;
}

void
Lang::Object::findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const
{
	/* There are no parent classes to consider. */
}

void
Lang::Object::assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const
{
	throw Exceptions::OverridingUndeclaredMethod( caller, selfRef, id );
}

void
Lang::Object::superNew( RefCountPtr< Lang::Instance > instanceSelf,
													 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
													 Kernel::Arguments & emptyArglist,
													 Kernel::EvalState * evalState ) const
{
	std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > >::iterator i = createdObjects->find( selfRef );
	if( i != createdObjects->end( ) )
		{
			return;
		}
	Lang::Instance * newObj = new Lang::Instance( dummyEnv, dummyEnv, selfRef, true, evalState->dyn_ );
	newObj->parents = createdObjects;
	createdObjects->insert( i, pair< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > >( selfRef, RefCountPtr< Lang::Instance >( newObj ) ) );
}

bool
Lang::Object::isRepeatableBase( ) const
{
	return true;
}

void
Lang::Object::gcMark( Kernel::GCMarkedSet & marked )
{ }

Lang::MetaClass::MetaClass( )
	: Lang::Class( strrefdup( "MetaClass" ) )
{ }

Lang::MetaClass::~MetaClass( )
{ }

Kernel::ValueRef
Lang::MetaClass::method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	throw Exceptions::MiscellaneousRequirement( "Calling the \"new\" method of MetaClass is actually not the way to create new classes, sorry." );
}

bool
Lang::MetaClass::method_isa( RefCountPtr< const Lang::Class > T ) const
{
	return T == Lang::Class::TypeID || T == Lang::THE_OBJECT;
}

void
Lang::MetaClass::findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const
{
	/* There are no parent classes to consider. */
}

void
Lang::MetaClass::assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const
{
	if( strcmp( id, "new" ) == 0 ||
			strcmp( id, "isa" ) == 0 )
		{
			throw Exceptions::OverridingFinalMethod( caller, selfRef, id );
		}
	throw Exceptions::OverridingUndeclaredMethod( caller, selfRef, id );
}

void
Lang::MetaClass::superNew( RefCountPtr< Lang::Instance > instanceSelf,
															RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
															Kernel::Arguments & emptyArglist,
															Kernel::EvalState * evalState ) const
{
	throw Exceptions::MiscellaneousRequirement( "It is forbidden to inherit from MetaClass, sorry." );
}

void
Lang::MetaClass::findParents( std::set< RefCountPtr< const Lang::Class > > * _allParents, std::set< RefCountPtr< const Lang::Class > > * _multiParents ) const
{
	throw Exceptions::MiscellaneousRequirement( "It is forbidden to inherit from MetaClass, sorry." );
}

bool
Lang::MetaClass::isRepeatableBase( ) const
{
	throw Exceptions::MiscellaneousRequirement( "MetaClass can certainly not be a repeated base." );
}

void
Lang::MetaClass::gcMark( Kernel::GCMarkedSet & marked )
{ }



Lang::SystemFinalClass::SystemFinalClass( RefCountPtr< const char > _prettyName )
	: Lang::Class( _prettyName ), registerFunction_( 0 )
{ }

Lang::SystemFinalClass::SystemFinalClass( RefCountPtr< const char > _prettyName, RegisterFunction registerFunction )
	: Lang::Class( _prettyName ), registerFunction_( registerFunction )
{ }

Lang::SystemFinalClass::~SystemFinalClass( )
{ }

void
Lang::SystemFinalClass::init( )
{
	if( registerFunction_ != 0 )
		{
			registerFunction_( this );
		}
}

void
Lang::SystemFinalClass::registerMethod( Kernel::MethodFactoryBase * factory )
{
	typedef typeof methods_ MapType;
	methods_.insert( MapType::value_type( factory->field( ), RefCountPtr< const Kernel::MethodFactoryBase >( factory ) ) );
}

void
Lang::SystemFinalClass::registerMutator( Lang::CoreFunction * fun )
{
	typedef typeof mutators_ MapType;
	mutators_.insert( MapType::value_type( fun->getTitle( ), RefCountPtr< const Lang::Function >( fun ) ) );
}

Kernel::ValueRef
Lang::SystemFinalClass::method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	throw Exceptions::MiscellaneousRequirement( "Use dedicated syntax rather than calling the \"new\" method of a system class." );
}

bool
Lang::SystemFinalClass::method_isa( RefCountPtr< const Lang::Class > T ) const
{
	return T == selfRef || T == Lang::THE_OBJECT;
}

void
Lang::SystemFinalClass::findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const
{
	/* There are no parent classes to consider. */
}

void
Lang::SystemFinalClass::assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const
{
	throw Exceptions::OverridingUndeclaredMethod( caller, selfRef, id );
}

void
Lang::SystemFinalClass::superNew( RefCountPtr< Lang::Instance > instanceSelf,
																		 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
																		 Kernel::Arguments & emptyArglist,
																		 Kernel::EvalState * evalState ) const
{
	throw Exceptions::MiscellaneousRequirement( "It is forbidden to inherit from SystemFinalClass, sorry." );
}

bool
Lang::SystemFinalClass::isRepeatableBase( ) const
{
	return false;
}

Kernel::VariableHandle
Lang::SystemFinalClass::getMethod( const RefCountPtr< const Lang::Value > & self, const char * methodID ) const
{
	typedef typeof methods_ MapType;
	MapType::const_iterator i = methods_.find( methodID );
	if( i == methods_.end( ) )
		{
			throw Exceptions::NonExistentMember( self->getTypeName( ), methodID );
		}
	return i->second->build( self );
}

RefCountPtr< const Lang::Function >
Lang::SystemFinalClass::getMutator( const char * mutatorID ) const
{
	typedef typeof mutators_ MapType;
	MapType::const_iterator i = mutators_.find( mutatorID );
	if( i == mutators_.end( ) )
		{
			throw Exceptions::NonExistentMutator( staticTypeName( ), mutatorID );
		}
	return i->second;
}

void
Lang::SystemFinalClass::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::SystemVirtualInterface::SystemVirtualInterface( RefCountPtr< const char > _prettyName )
	: Lang::Class( _prettyName )
{ }

Lang::SystemVirtualInterface::~SystemVirtualInterface( )
{ }

void
Lang::SystemVirtualInterface::addVirtual( const char * id )
{
	messageMap[ Kernel::MethodId( selfRef, id ) ];
}

Kernel::ValueRef
Lang::SystemVirtualInterface::method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	throw Exceptions::InstantiatingAbstractClass( selfRef );
}

bool
Lang::SystemVirtualInterface::method_isa( RefCountPtr< const Lang::Class > T ) const
{
	throw Exceptions::InternalError( "method_isa was called on an abstract system class." );
}

void
Lang::SystemVirtualInterface::findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const
{
	/* There are no parent classes to consider. */
}

void
Lang::SystemVirtualInterface::assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const
{
	if( messageMap.find( Kernel::MethodId( selfRef, id ) ) == messageMap.end( ) )
		{
			throw Exceptions::OverridingUndeclaredMethod( caller, selfRef, id );
		}
}

void
Lang::SystemVirtualInterface::superNew( RefCountPtr< Lang::Instance > instanceSelf,
																					 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
																					 Kernel::Arguments & emptyArglist,
																					 Kernel::EvalState * evalState ) const
{ }

bool
Lang::SystemVirtualInterface::isRepeatableBase( ) const
{
	return true;
}

void
Lang::SystemVirtualInterface::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::UserClass::UserClass( const Ast::ClassFunction * _classExpr,
															 Kernel::PassedEnv _env,
															 RefCountPtr< const char > _prettyName,
															 Kernel::EvaluatedFormals * _formals,
															 RefCountPtr< std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > > > _parents,
															 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > > > _overrides,
															 bool _isFinal )
	: Lang::Class( _prettyName ), classExpr( _classExpr ), formals( _formals ), env( _env ), parents( _parents ), overrides( _overrides )
{
	Lang::Class::isFinal = _isFinal;
}

Lang::UserClass::~UserClass( )
{
	delete formals;
}

void
Lang::UserClass::setupAndCheck( bool declaredAbstract )
{
	for( std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > >::const_iterator i = parents->begin( ); i != parents->end( ); ++i )
		{
			typedef typeof immediateParents SetType;
			SetType::const_iterator j = immediateParents.find( i->first );
			if( j != immediateParents.end( ) )
				{
					throw Exceptions::RepeatedImmediateParent( selfRef, i->first );
				}
			if( i->first->getFinal( ) )
				{
					throw Exceptions::InheritingFinal( selfRef, i->first );
				}
			immediateParents.insert( j, i->first );
		}

	findParents( & allParents, & multiParents );

	for( std::set< RefCountPtr< const Lang::Class > >::const_iterator i = multiParents.begin( ); i != multiParents.end( ); ++i )
		{
			if( ! (*i)->isRepeatableBase( ) )
				{
					throw Exceptions::IllegalRepeatedBase( selfRef, *i );
				}
		}

	messageMap = classExpr->getLocalMessageMap( selfRef );

	for( std::set< RefCountPtr< const Lang::Class > >::const_iterator i = immediateParents.begin( ); i != immediateParents.end( ); ++i )
		{
			const MessageMapType & parentMap = (*i)->getMessageMap( );
			for( MessageMapType::const_iterator j = parentMap.begin( ); j != parentMap.end( ); ++j )
				{
					std::set< RefCountPtr< const Lang::Class > > & messageSet = messageMap[ j->first ];
					std::set< RefCountPtr< const Lang::Class > > res;
					set_union( messageSet.begin( ), messageSet.end( ),
										 j->second.begin( ), j->second.end( ),
										 insert_iterator< std::set< RefCountPtr< const Lang::Class > > >( res, res.begin( ) ) );
					messageSet = res;
				}
		}

	{
		typedef typeof *overrides MapType;
		for( MapType::const_iterator i = overrides->begin( ); i != overrides->end( ); ++i )
			{
				typedef typeof allParents ClassSetType;
				{
					ClassSetType::const_iterator j = allParents.find( i->first );
					if( j == allParents.end( ) )
						{
							throw Exceptions::OverridingNonParent( selfRef, i->first );
						}
				}
				{
					typedef typeof i->second ListType;
					for( ListType::const_iterator j = i->second.begin( ); j != i->second.end( ); ++j )
						{
							i->first->assertMethodOverridable( (*j)->id_, selfRef );
							std::set< RefCountPtr< const Lang::Class > > & messageSet = messageMap[ Kernel::MethodId( i->first, (*j)->id_ ) ];
							messageSet.clear( );
							messageSet.insert( messageSet.begin( ), selfRef );
						}
				}
			}
	}


	{
		/*
		std::cerr << "Message map of " << getPrettyName( ) << ":" << std::endl ;
		*/
		std::set< Kernel::MethodId >::iterator j = abstractSet.begin( );
		MessageMapType::iterator k = messageMap.begin( );
		for( MessageMapType::const_iterator i = messageMap.begin( ); i != messageMap.end( ); ++i )
			{
				/*
				std::cerr << "	" << i->first.prettyName( ) << " --> " ;
				for( std::set< RefCountPtr< const Lang::Class > >::const_iterator l = i->second.begin( ); l != i->second.end( ); ++l )
					{
						std::cerr << (*l)->getPrettyName( ) << " " ;
					}
				std::cerr << std::endl ;
				*/
				if( i->second.size( ) == 1 )
					{
						continue;
					}
				if( i->second.empty( ) )
					{
						j = abstractSet.insert( j, i->first );
					}
				else
					{
						throw Exceptions::AmbiguousInheritedMethod( selfRef, i->first.prettyName( ), i->second );
					}
			}
	}

	if( ! declaredAbstract && ! abstractSet.empty( ) )
		{
			throw Exceptions::FailedToDeclareClassAbstract( selfRef, classExpr );
		}
}

Kernel::ValueRef
Lang::UserClass::method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	if( ! abstractSet.empty( ) )
		{
			throw Exceptions::InstantiatingAbstractClass( selfRef );
		}

	throw Exceptions::NotImplemented( "UserClass::method_new" );

//	 //	std::vector< VariableHandle > * envValues = new std::vector< VariableHandle >;
//	 //	envValues->reserve( argumentOrder->size( ) );
//	 //	while( envValues->size( ) < argumentOrder->size( ) )
//	 //		{
//	 //			envValues->push_back( NullPtr< Kernel::Variable >( ) );
//	 //		}
//	 Kernel::PassedEnv instanceEnv( new Kernel::Environment( Shapes::theEnvironmentList, evalState->env_, argumentOrder, RefCountPtr< std::vector< VariableHandle > >( envValues ) ) );

//	 //	std::vector< VariableHandle > * envValues = new std::vector< VariableHandle >;
//	 //	envValues->reserve( argumentOrder->size( ) );
//	 //	while( envValues->size( ) < argumentOrder->size( ) )
//	 //		{
//	 //			envValues->push_back( NullPtr< Kernel::Variable >( ) );
//	 //		}
//	 Kernel::PassedEnv privateEnv( new Kernel::Environment( Shapes::theEnvironmentList, instanceEnv, argumentOrder, RefCountPtr< std::vector< VariableHandle > >( envValues ) ) );

//	 RefCountPtr< Lang::Instance > instanceSelf = RefCountPtr< Lang::Instance >( new Lang::Instance( instanceEnv, privateEnv, selfRef, false, evalState->dyn_ ) );

//	 privateEnv->define( classExpr->loc( ), SELF_ID, RefCountPtr< const Lang::Geometric2D >( instanceSelf ), true ); /* true means constant */

//	 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects( new std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > );
//	 instanceSelf->parents = createdObjects;

//	 for( std::set< RefCountPtr< const Lang::Class > >::const_iterator i = multiParents.begin( ); i != multiParents.end( ); ++i )
//		 {
//			 (*i)->superNew( instanceSelf, createdObjects, EMPTY_ARGLIST, evalState );
//		 }

//	 args.mergeWithFormals( formals )
//	 Kernel::PassedEnv initEnv = new Kernel::Environment( theEnvironmentList, env, formals->formals->argumentOrder, args.orderedArguments );

//	 {
//		 typedef std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > > ListType;
//		 for( ListType::const_iterator i = parents->begin( ); i != parents->end( ); ++i )
//			 {
//				 RefCountPtr< Kernel::Arguments > superArgs( new Kernel::Arguments( true ) );	/* true means constant */
//				 i->second->evaluate( superArgs, dstgroup, pdfo, metaState, initEnv );
//				 i->first->superNew( instanceSelf, createdObjects, *superArgs, evalState );
//			 }
//	 }

//	 classExpr->setupInstance( instanceEnv, privateEnv, dstgroup, pdfo, metaState, initEnv );

//	 addOverrides( instanceSelf, privateEnv, dstgroup, pdfo, metaState );

//	 instanceSelf->prepare( dstgroup, pdfo, metaState );

//	 return RefCountPtr< const Lang::Geometric2D >( instanceSelf );
}

bool
Lang::UserClass::method_isa( RefCountPtr< const Lang::Class > T ) const
{
	if( T == selfRef )
		{
			return true;
		}
	return allParents.find( T ) != allParents.end( );
}

void
Lang::UserClass::findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const
{
	throw Exceptions::NotImplemented( "UserClass::findMultiplyInheritedClasses" );
}

void
Lang::UserClass::assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const
{
	if( classExpr->isInPublicGetSet( id ) || classExpr->isInAbstractSet( id ) )
		{
			if( classExpr->isInFinalSet( id ) )
				{
					throw Exceptions::OverridingFinalMethod( caller, selfRef, id );
				}
		}
	else
		{
			throw Exceptions::OverridingUndeclaredMethod( caller, selfRef, id );
		}
}

void
Lang::UserClass::superNew( RefCountPtr< Lang::Instance > instanceSelf,
															RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
															Kernel::Arguments & args,
															Kernel::EvalState * evalState ) const
{
	std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > >::iterator i = createdObjects->find( selfRef );
	if( i != createdObjects->end( ) )
		{
			return;
		}

	throw Exceptions::NotImplemented( "UserClass::superNew" );

//	 Kernel::PassedEnv instanceEnv( new Kernel::Environment( env ) );
//	 Kernel::PassedEnv privateEnv( new Kernel::Environment( instanceEnv ) );

//	 privateEnv->define( classExpr->loc( ), SELF_ID, RefCountPtr< const Lang::Geometric2D >( instanceSelf ), true ); /* true means constant */

//	 args.mergeWithFormals( formals )
//	 Kernel::PassedEnv initEnv = new Kernel::Environment( theEnvironmentList, env, formals->formals->argumentOrder, args.orderedArguments );

//	 {
//		 typedef std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > > ListType;
//		 for( ListType::const_iterator i = parents->begin( ); i != parents->end( ); ++i )
//			 {
//				 RefCountPtr< Kernel::Arguments > superArgs( new Kernel::Arguments( true ) );	/* true means constant */
//				 i->second->evaluate( superArgs, dstgroup, pdfo, metaState, initEnv );
//				 i->first->superNew( instanceSelf, createdObjects, *superArgs, evalState );
//			 }
//	 }

//	 classExpr->setupInstance( instanceEnv, privateEnv, dstgroup, pdfo, metaState, initEnv );

//	 Lang::Instance * newObj = new Lang::Instance( instanceEnv, privateEnv, selfRef, true, evalState->dyn_ );
//	 newObj->parents = createdObjects;
//	 createdObjects->insert( i, pair< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > >( selfRef, RefCountPtr< Lang::Instance >( newObj ) ) );
}

void
Lang::UserClass::findParents( std::set< RefCountPtr< const Lang::Class > > * _allParents, std::set< RefCountPtr< const Lang::Class > > * _multiParents ) const
{
	Lang::Class::findParents( _allParents, _multiParents );
	typedef typeof immediateParents SetType;
	for( SetType::const_iterator i = immediateParents.begin( ); i != immediateParents.end( ); ++i )
		{
			(*i)->findParents( _allParents, _multiParents );
		}
}

bool
Lang::UserClass::isRepeatableBase( ) const
{
	return classExpr->isRepeatableBase( );
}

void
Lang::UserClass::prepareInstance( Kernel::EvalState * evalState, Kernel::PassedEnv privateEnv ) const
{
	classExpr->prepareInstance( evalState, privateEnv );
}


bool
Lang::UserClass::isInPublicGetSet( const char * field ) const
{
	return classExpr->isInPublicGetSet( field );
}

bool
Lang::UserClass::isInPublicSetSet( const char * field ) const
{
	return classExpr->isInPublicSetSet( field );
}

bool
Lang::UserClass::isInProtectedGetSet( const char * field ) const
{
	return classExpr->isInProtectedGetSet( field );
}

bool
Lang::UserClass::isInProtectedSetSet( const char * field ) const
{
	return classExpr->isInProtectedSetSet( field );
}

bool
Lang::UserClass::isInTransformingSet( const char * field ) const
{
	return classExpr->isInTransformingSet( field );
}


void
Lang::UserClass::addOverrides( Kernel::EvalState * evalState, RefCountPtr< Lang::Instance > instance, Kernel::PassedEnv privateEnv ) const
{
	typedef typeof *overrides DefMapType;
	for( DefMapType::const_iterator i = overrides->begin( ); i != overrides->end( ); ++i )
		{
			typedef std::map< const char *, RefCountPtr< const Lang::Function >, charPtrLess > InstanceMapType;
			InstanceMapType & instanceMap = instance->overrides[ i->first ];
			typedef typeof i->second DefListType;
			InstanceMapType::iterator lastInsert = instanceMap.begin( );
			for( DefListType::const_iterator j = i->second.begin( ); j != i->second.end( ); ++j )
				{
					throw Exceptions::NotImplemented( "addOverrides" );
//					 RefCountPtr< const Lang::Value > untypedMethod = (*j)->init->value( dstgroup, pdfo, metaState, privateEnv );
//					 typedef const Lang::Function FunType;
//					 RefCountPtr< FunType > typedMethod = untypedMethod.down_cast< FunType >( );
//					 if( typedMethod == NullPtr< FunType >( ) )
//						 {
//							 throw Exceptions::TypeMismatch( (*j)->init, untypedMethod->getTypeName( ), FunType::staticTypeName( ) );
//						 }
//					 lastInsert = instanceMap.insert( lastInsert, InstanceMapType::value_type( (*j)->id, typedMethod ) );
				}
		}
}

void
Lang::UserClass::gcMark( Kernel::GCMarkedSet & marked )
{
	throw Exceptions::NotImplemented( "UserClass::gcMark" );
}




Lang::ClassMethodBase::ClassMethodBase( RefCountPtr< const Lang::Class > _self )
	: Lang::Function( 0 ), self( _self )
{
	std::cerr << "Warning: Lang::ClassMethodBase::ClassMethodBase initializes Lang::Function with 0 pointer to formals." << std::endl ;
}

Lang::ClassMethodBase::~ClassMethodBase( )
{ }

void
Lang::ClassMethodBase::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Class * >( self.getPtr( ) )->gcMark( marked );
}

bool
Lang::ClassMethodBase::isTransforming( ) const
{
	return false;
}

Lang::ClassMethodNew::ClassMethodNew( RefCountPtr< const Lang::Class > _self )
	: Lang::ClassMethodBase( _self )
{ }

void
Lang::ClassMethodNew::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	self->method_new( evalState, args, callLoc );
}

Lang::ClassMethodIsa::ClassMethodIsa( RefCountPtr< const Lang::Class > _self )
	: Lang::ClassMethodBase( _self )
{ }

void
Lang::ClassMethodIsa::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, Kernel::MethodId( Lang::Class::TypeID, "isa" ).prettyName( ) );

	typedef const Lang::Class ArgType;
	RefCountPtr< ArgType > T = args.getValue( 0 ).down_cast< ArgType >( );
	if( T == NullPtr< ArgType >( ) )
		{
			throw Exceptions::CoreTypeMismatch( callLoc, Kernel::MethodId( Lang::Class::TypeID, "isa" ).prettyName( ), args, 0, ArgType::staticTypeName( ) );
		}

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Boolean( self->method_isa( T ) ) ),
									 evalState );
}
