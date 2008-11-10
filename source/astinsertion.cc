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

#include "astinsertion.h"



Shapes::MemberAssignStmt::MemberAssignStmt( const Ast::SourceLocation & _loc, Ast::Expression * _variable, Shapes::IdentifierNode * _fieldID, Ast::Expression * _expr )
	: loc( _loc ), variable( _variable ), fieldID( _fieldID ), expr( _expr )
{ }

Shapes::MemberAssignStmt::~MemberAssignStmt( )
{
	delete variable;
	delete fieldID;
	delete expr;
}

RefCountPtr< const Lang::Value >
Shapes::MemberAssignStmt::value( Kernel::Environment::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
	Kernel::ValueRef untypedVar = variable->value( dstgroup, pdfo, metaState, env );

	typedef const Lang::Instance VarType;
	VarType * typedVar = dynamic_cast< VarType * >( untypedVar.getPtr( ) );
	if( typedVar == 0 )
		{
			throw Exceptions::NonObjectMemberAssignment( loc, untypedVar->getTypeName( ) );
		}

	typedVar->assignMember( fieldID->identifier( dstgroup, pdfo, metaState, env ),
													expr->value( dstgroup, pdfo, metaState, env ) );

	return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::MemberAssignStmt::firstLoc( ) const
{
	return loc;
}

const Ast::SourceLocation &
Shapes::MemberAssignStmt::lastLoc( ) const
{
	return loc;
}




