#include "astinsertion.h"



MetaPDF::MemberAssignStmt::MemberAssignStmt( const Ast::SourceLocation & _loc, Ast::Expression * _variable, MetaPDF::IdentifierNode * _fieldID, Ast::Expression * _expr )
  : loc( _loc ), variable( _variable ), fieldID( _fieldID ), expr( _expr )
{ }

MetaPDF::MemberAssignStmt::~MemberAssignStmt( )
{
  delete variable;
  delete fieldID;
  delete expr;
}

RefCountPtr< const Lang::Value >
MetaPDF::MemberAssignStmt::value( Kernel::Environment::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
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

  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::MemberAssignStmt::firstLoc( ) const
{
  return loc;
}

const Ast::SourceLocation &
MetaPDF::MemberAssignStmt::lastLoc( ) const
{
  return loc;
}




