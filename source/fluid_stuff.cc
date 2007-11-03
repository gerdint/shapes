| T_identifier T_flassign Expr
{
  $$ = new Shapes::FluidLetAssignStmt( @1, $1, $3 );
}
| Expr '.' T_identifier T_flassign Expr
{
  $$ = new Shapes::MemberFluidLetAssignStmt( @$, $1, $3, $5 );
}



Shapes::ResetVariableStmt::ResetVariableStmt( const Ast::SourceLocation & _idLoc, Kernel::VariableHandle _var, Kernel::ValueRef _val )
  : idLoc( _idLoc ), var( _var ), val( _val )
{ }

Shapes::ResetVariableStmt::~ResetVariableStmt( )
{ }

RefCountPtr< const Lang::Value >
Shapes::ResetVariableStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  var->quickSetVal( val );
  return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::ResetVariableStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
Shapes::ResetVariableStmt::lastLoc( ) const
{
  return idLoc;
}


Shapes::ResetMemberStmt::ResetMemberStmt( const Ast::SourceLocation & _loc, RefCountPtr< const Lang::Instance > _var, RefCountPtr< const char > _fieldID, Kernel::ValueRef _val )
  : loc( _loc ), var( _var ), fieldID( _fieldID ), val( _val )
{ }

Shapes::ResetMemberStmt::~ResetMemberStmt( )
{ }

RefCountPtr< const Lang::Value >
Shapes::ResetMemberStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  var->assignMember( fieldID, val );
  return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::ResetMemberStmt::firstLoc( ) const
{
  return loc;
}

const Ast::SourceLocation &
Shapes::ResetMemberStmt::lastLoc( ) const
{
  return loc;
}


Shapes::ResetUnitStmt::ResetUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, double _val )
  : idLoc( _idLoc ), id( _id ), val( _val )
{ }

Shapes::ResetUnitStmt::~ResetUnitStmt( )
{ }

RefCountPtr< const Lang::Value >
Shapes::ResetUnitStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  env->redefineUnit( idLoc, id, val );
  return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::ResetUnitStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
Shapes::ResetUnitStmt::lastLoc( ) const
{
  return idLoc;
}



Shapes::FluidLetAssignStmt::FluidLetAssignStmt( const Ast::SourceLocation & _idLoc, Shapes::IdentifierNode * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

Shapes::FluidLetAssignStmt::~FluidLetAssignStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
Shapes::FluidLetAssignStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  RefCountPtr< const char > idStr = id->identifier( dstgroup, pdfo, metaState, env );
  Kernel::VariableHandle var = env->getVarHandle( idLoc, idStr );
  metaState->doAtEndOfGroup->push_front( new Shapes::ResetVariableStmt( idLoc, var, env->lookup( idLoc, id->identifier( dstgroup, pdfo, metaState, env ) ) ) );
  var->setVal( idLoc, idStr, expr->value( dstgroup, pdfo, metaState, env ) );
  return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::FluidLetAssignStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
Shapes::FluidLetAssignStmt::lastLoc( ) const
{
  return expr->lastLoc( );
}




Shapes::MemberFluidLetAssignStmt::MemberFluidLetAssignStmt( const Ast::SourceLocation & _loc, Ast::Expression * _variable, Shapes::IdentifierNode * _fieldID, Ast::Expression * _expr )
  : loc( _loc ), variable( _variable ), fieldID( _fieldID ), expr( _expr )
{ }

Shapes::MemberFluidLetAssignStmt::~MemberFluidLetAssignStmt( )
{
  delete variable;
  delete fieldID;
  delete expr;
}

RefCountPtr< const Lang::Value >
Shapes::MemberFluidLetAssignStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  Kernel::ValueRef untypedVar = variable->value( dstgroup, pdfo, metaState, env );

  typedef const Lang::Instance VarType;
  RefCountPtr< VarType > typedVar = untypedVar.down_cast< VarType >( );
  if( typedVar == NullPtr< VarType >( ) )
    {
      throw Exceptions::NonObjectMemberAssignment( loc, untypedVar->getTypeName( ) );
    }

  RefCountPtr< const char > id = fieldID->identifier( dstgroup, pdfo, metaState, env );

  metaState->doAtEndOfGroup->push_front( new Shapes::ResetMemberStmt( loc, typedVar, id, typedVar->getField( id ) ) );
  typedVar->assignMember( id, expr->value( dstgroup, pdfo, metaState, env ) );

  return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::MemberFluidLetAssignStmt::firstLoc( ) const
{
  return loc;
}

const Ast::SourceLocation &
Shapes::MemberFluidLetAssignStmt::lastLoc( ) const
{
  return loc;
}



Shapes::FluidLetAssignUnitStmt::FluidLetAssignUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

Shapes::FluidLetAssignUnitStmt::~FluidLetAssignUnitStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
Shapes::FluidLetAssignUnitStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  RefCountPtr< const Lang::Value > untypedVal = expr->value( dstgroup, pdfo, metaState, env );
  typedef const Lang::Length ArgType;
  ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
  if( val == 0 )
    {
      throw Exceptions::TypeMismatch( expr, untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
    }
  metaState->doAtEndOfGroup->push_front( new Shapes::ResetUnitStmt( idLoc, id, env->lookupUnit( idLoc, id ) ) );
  env->redefineUnit( idLoc, id, val->getVal( ) );
  return Shapes::THE_VOID;
}

const Ast::SourceLocation &
Shapes::FluidLetAssignUnitStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
Shapes::FluidLetAssignUnitStmt::lastLoc( ) const
{
  return expr->lastLoc( );
}


Shapes::IntroduceCold::IntroduceCold( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr, size_t ** _idPos )
  : Ast::BindNode( Ast::SourceLocation( _idLoc, _expr->loc( ) ), _idLoc, _id ), expr( _expr ), idPos( _idPos )
{ }

Shapes::IntroduceCold::~IntroduceCold( )
{
  delete id;
  delete expr;
}

void
Shapes::IntroduceCold::eval( Kernel::EvalState * evalState ) const
{
  if( *idPos == 0 )
    {
      *idPos = new size_t( evalState->env->findLocalPosition( loc, id ) );
    }

  evalState->env->define( idLoc, id->identifier( dstgroup, pdfo, metaState, env ), expr->value( dstgroup, pdfo, metaState, env ), false ); /* false means not constant */
}


Shapes::AssignStmt::AssignStmt( const Ast::SourceLocation & _idLoc, Shapes::IdentifierNode * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

Shapes::AssignStmt::~AssignStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
Shapes::AssignStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  env->redefine( idLoc, id->identifier( dstgroup, pdfo, metaState, env ), expr->value( dstgroup, pdfo, metaState, env ) );
  return Shapes::THE_VOID;
}
