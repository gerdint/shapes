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

#define UNARYDISPATCHCASE( T )\
	case ::Shapes::Kernel::TYPEID_ ## T :								 \
	{\
		RefCountPtr< const ::Shapes::Lang::T > arg = self.down_cast< const ::Shapes::Lang::T >( ); \
		if( arg == NullPtr< const ::Shapes::Lang::T >( ) )																\
			{\
				throw Exceptions::InternalError( strrefdup( "Downcast in unary dispatch failed, " #T ) ); \
			}\
		return op->op( arg, dyn ); \
	}\
	break;

#define UNARYDISPATCHBASEDECL static RefCountPtr< const Lang::Value > unaryDispatch( RefCountPtr< const Lang::Value > self, const Kernel::PassedDyn & dyn, const Shapes::Ast::UnaryExpr * op );

#define UNARYDISPATCHBASEIMPL \
	RefCountPtr< const Lang::Value >																\
	Lang::Value::unaryDispatch( RefCountPtr< const Lang::Value > self, const Kernel::PassedDyn & dyn, const Shapes::Ast::UnaryExpr * op ) \
{\
	switch( self->getTypeID( ) )\
		{\
			SINGLELOOP1( CLASSTREE1_ROOT, UNARYDISPATCHCASE );								\
		default:\
			throw Exceptions::InternalError( strrefdup( "QuickTypeID out of range in unary dispatch." ) ); \
		}\
}

#define UNARYDISPATCHDECL

#define UNARYDISPATCHIMPL( T )


#define BINARYDISPATCHCASE_LEVEL1( T1 )\
	case ::Shapes::Kernel::TYPEID_ ## T1 :								 \
	{\
		RefCountPtr< const ::Shapes::Lang::T1 > arg1 = self.down_cast< const ::Shapes::Lang::T1 >( ); \
		if( arg1 == NullPtr< const ::Shapes::Lang::T1 >( ) )												\
			{\
				throw Exceptions::InternalError( strrefdup( "Downcast in binary dispatch level 1 failed, " #T1 ) ); \
			}\
		return binaryDispatch2( arg1, other, dyn, op ); \
	}\
	break;

#define BINARYDISPATCH2CASE_LEVEL2( T2 )\
	case ::Shapes::Kernel::TYPEID_ ## T2 :								 \
	{\
		RefCountPtr< const ::Shapes::Lang::T2 > arg2 = other.down_cast< const ::Shapes::Lang::T2 >( ); \
		if( arg2 == NullPtr< const ::Shapes::Lang::T2 >( ) )												\
			{\
				throw Exceptions::InternalError( strrefdup( "Downcast in binary dispatch level 2 failed, " #T2 ) ); \
			}\
		return op->op( arg1, arg2, dyn ); \
	}\
	break;

#define BINARYDISPATCH2CASE_LEVEL1( T1 )\
	RefCountPtr< const Lang::Value >																\
	Lang::Value::binaryDispatch2( RefCountPtr< const ::Shapes::Lang::T1 > arg1, RefCountPtr< const Lang::Value > other, const Kernel::PassedDyn & dyn, const ::Shapes::Ast::BinaryInfixExpr * op ) \
{\
	switch( other->getTypeID( ) )																								\
		{																																				\
			SINGLELOOP2( CLASSTREE2_ROOT, BINARYDISPATCH2CASE_LEVEL2 );				\
		default:																																\
			throw Exceptions::InternalError( strrefdup( "QuickTypeID out of range in binary dispatch level 2." ) ); \
		}																																				\
}


#define BINARYDISPATCH2DECL( T ) static RefCountPtr< const Lang::Value > binaryDispatch2( RefCountPtr< const ::Shapes::Lang::T > self, RefCountPtr< const Lang::Value > other, const Kernel::PassedDyn & dyn, const ::Shapes::Ast::BinaryInfixExpr * op );

#define BINARYDISPATCHBASEDECL \
	static RefCountPtr< const Lang::Value > binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const Kernel::PassedDyn & dyn, const ::Shapes::Ast::BinaryInfixExpr * op ); \
	SINGLELOOP1( CLASSTREE1_ROOT, BINARYDISPATCH2DECL )

#define BINARYDISPATCHBASEIMPL \
	RefCountPtr< const Lang::Value >																\
	Lang::Value::binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const Kernel::PassedDyn & dyn, const ::Shapes::Ast::BinaryInfixExpr * op ) \
{\
	switch( self->getTypeID( ) )\
		{\
			SINGLELOOP1( CLASSTREE1_ROOT, BINARYDISPATCHCASE_LEVEL1 );								\
		default:\
			throw Exceptions::InternalError( strrefdup( "QuickTypeID out of range in binary dispatch level 1." ) ); \
		}\
}\
	SINGLELOOP1( CLASSTREE1_ROOT, BINARYDISPATCH2CASE_LEVEL1 )

#define BINARYDISPATCHDECL

#define BINARYDISPATCHIMPL( Ts )
