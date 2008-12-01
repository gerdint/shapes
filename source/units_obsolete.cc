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

	class UserLength : public Expression 
	{
		Ast::SourceLocation loc;
		double val;
		const char * unit;
	public:
		UserLength( const Ast::SourceLocation & _loc, double _val, const char * _unit );
		virtual ~UserLength( );
		virtual void eval( Kernel::EvalState * evalState ) const;
		virtual const Ast::SourceLocation & firstLoc( ) const;
		virtual const Ast::SourceLocation & lastLoc( ) const;
	};

	class IntroduceUnitStmt : public Expression
	{
		Ast::SourceLocation idLoc;
		const char * id;
		Ast::Expression * expr;
	public:
		IntroduceUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr );
		virtual ~IntroduceUnitStmt( );
		virtual void eval( Kernel::EvalState * evalState ) const;
		virtual const Ast::SourceLocation & firstLoc( ) const;
		virtual const Ast::SourceLocation & lastLoc( ) const;
	};




Shapes::UserLength::UserLength( const Ast::SourceLocation & _loc, double _val, const char * _unit )
	: loc( _loc ), val( _val ), unit( _unit )
{ }

Shapes::UserLength::~UserLength( )
{
	delete unit;
}

void
Shapes::UserLength::eval( Kernel::EvalState * evalState ) const
{
	Kernel::ContRef cont = evalState->cont_;
	(*cont)( Kernel::ValueRef( new Lang::Length( val * evalState->env->lookupUnit( loc, unit ) ) ),
					 evalState );
}

const Ast::SourceLocation &
Shapes::UserLength::firstLoc( ) const
{
	return loc;
}

const Ast::SourceLocation &
Shapes::UserLength::lastLoc( ) const
{
	return loc;
}


Shapes::IntroduceUnitStmt::IntroduceUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr )
	: idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

Shapes::IntroduceUnitStmt::~IntroduceUnitStmt( )
{
	delete id;
	delete expr;
}

RefCountPtr< const Lang::Value >
Shapes::IntroduceUnitStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
	RefCountPtr< const Lang::Value > untypedVal = expr->value( dstgroup, pdfo, metaState, env );
	typedef const Lang::Length ArgType;
	ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
	if( val == 0 )
		{
			throw Exceptions::TypeMismatch( expr, untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
		}
	env->defineUnit( idLoc, id, val->getVal( ) );
	return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::IntroduceUnitStmt::firstLoc( ) const
{
	return idLoc;
}

const Ast::SourceLocation &
Shapes::IntroduceUnitStmt::lastLoc( ) const
{
	return expr->lastLoc( );
}


Shapes::AssignUnitStmt::AssignUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr )
	: idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

Shapes::AssignUnitStmt::~AssignUnitStmt( )
{
	delete id;
	delete expr;
}

RefCountPtr< const Lang::Value >
Shapes::AssignUnitStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
	RefCountPtr< const Lang::Value > untypedVal = expr->value( dstgroup, pdfo, metaState, env );
	typedef const Lang::Length ArgType;
	ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
	if( val == 0 )
		{
			throw Exceptions::TypeMismatch( expr, untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
		}
	env->redefineUnit( idLoc, id, val->getVal( ) );
	return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::AssignUnitStmt::firstLoc( ) const
{
	return idLoc;
}

const Ast::SourceLocation &
Shapes::AssignUnitStmt::lastLoc( ) const
{
	return expr->lastLoc( );
}

