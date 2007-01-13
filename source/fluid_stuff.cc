| T_identifier T_flassign Expr
{
  $$ = new MetaPDF::FluidLetAssignStmt( @1, $1, $3 );
}
| Expr '.' T_identifier T_flassign Expr
{
  $$ = new MetaPDF::MemberFluidLetAssignStmt( @$, $1, $3, $5 );
}



MetaPDF::ResetVariableStmt::ResetVariableStmt( const Ast::SourceLocation & _idLoc, Kernel::VariableHandle _var, Kernel::ValueRef _val )
  : idLoc( _idLoc ), var( _var ), val( _val )
{ }

MetaPDF::ResetVariableStmt::~ResetVariableStmt( )
{ }

RefCountPtr< const Lang::Value >
MetaPDF::ResetVariableStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  var->quickSetVal( val );
  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::ResetVariableStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
MetaPDF::ResetVariableStmt::lastLoc( ) const
{
  return idLoc;
}


MetaPDF::ResetMemberStmt::ResetMemberStmt( const Ast::SourceLocation & _loc, RefCountPtr< const Lang::Instance > _var, RefCountPtr< const char > _fieldID, Kernel::ValueRef _val )
  : loc( _loc ), var( _var ), fieldID( _fieldID ), val( _val )
{ }

MetaPDF::ResetMemberStmt::~ResetMemberStmt( )
{ }

RefCountPtr< const Lang::Value >
MetaPDF::ResetMemberStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  var->assignMember( fieldID, val );
  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::ResetMemberStmt::firstLoc( ) const
{
  return loc;
}

const Ast::SourceLocation &
MetaPDF::ResetMemberStmt::lastLoc( ) const
{
  return loc;
}


MetaPDF::ResetUnitStmt::ResetUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, double _val )
  : idLoc( _idLoc ), id( _id ), val( _val )
{ }

MetaPDF::ResetUnitStmt::~ResetUnitStmt( )
{ }

RefCountPtr< const Lang::Value >
MetaPDF::ResetUnitStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  env->redefineUnit( idLoc, id, val );
  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::ResetUnitStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
MetaPDF::ResetUnitStmt::lastLoc( ) const
{
  return idLoc;
}



MetaPDF::FluidLetAssignStmt::FluidLetAssignStmt( const Ast::SourceLocation & _idLoc, MetaPDF::IdentifierNode * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

MetaPDF::FluidLetAssignStmt::~FluidLetAssignStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
MetaPDF::FluidLetAssignStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  RefCountPtr< const char > idStr = id->identifier( dstgroup, pdfo, metaState, env );
  Kernel::VariableHandle var = env->getVarHandle( idLoc, idStr );
  metaState->doAtEndOfGroup->push_front( new MetaPDF::ResetVariableStmt( idLoc, var, env->lookup( idLoc, id->identifier( dstgroup, pdfo, metaState, env ) ) ) );
  var->setVal( idLoc, idStr, expr->value( dstgroup, pdfo, metaState, env ) );
  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::FluidLetAssignStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
MetaPDF::FluidLetAssignStmt::lastLoc( ) const
{
  return expr->lastLoc( );
}




MetaPDF::MemberFluidLetAssignStmt::MemberFluidLetAssignStmt( const Ast::SourceLocation & _loc, Ast::Expression * _variable, MetaPDF::IdentifierNode * _fieldID, Ast::Expression * _expr )
  : loc( _loc ), variable( _variable ), fieldID( _fieldID ), expr( _expr )
{ }

MetaPDF::MemberFluidLetAssignStmt::~MemberFluidLetAssignStmt( )
{
  delete variable;
  delete fieldID;
  delete expr;
}

RefCountPtr< const Lang::Value >
MetaPDF::MemberFluidLetAssignStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  Kernel::ValueRef untypedVar = variable->value( dstgroup, pdfo, metaState, env );

  typedef const Lang::Instance VarType;
  RefCountPtr< VarType > typedVar = untypedVar.down_cast< VarType >( );
  if( typedVar == NullPtr< VarType >( ) )
    {
      throw Exceptions::NonObjectMemberAssignment( loc, untypedVar->getTypeName( ) );
    }

  RefCountPtr< const char > id = fieldID->identifier( dstgroup, pdfo, metaState, env );

  metaState->doAtEndOfGroup->push_front( new MetaPDF::ResetMemberStmt( loc, typedVar, id, typedVar->getField( id ) ) );
  typedVar->assignMember( id, expr->value( dstgroup, pdfo, metaState, env ) );

  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::MemberFluidLetAssignStmt::firstLoc( ) const
{
  return loc;
}

const Ast::SourceLocation &
MetaPDF::MemberFluidLetAssignStmt::lastLoc( ) const
{
  return loc;
}



MetaPDF::FluidLetAssignUnitStmt::FluidLetAssignUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

MetaPDF::FluidLetAssignUnitStmt::~FluidLetAssignUnitStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
MetaPDF::FluidLetAssignUnitStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  RefCountPtr< const Lang::Value > untypedVal = expr->value( dstgroup, pdfo, metaState, env );
  typedef const Lang::Length ArgType;
  ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
  if( val == 0 )
    {
      throw Exceptions::TypeMismatch( expr, untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
    }
  metaState->doAtEndOfGroup->push_front( new MetaPDF::ResetUnitStmt( idLoc, id, env->lookupUnit( idLoc, id ) ) );
  env->redefineUnit( idLoc, id, val->getVal( ) );
  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::FluidLetAssignUnitStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
MetaPDF::FluidLetAssignUnitStmt::lastLoc( ) const
{
  return expr->lastLoc( );
}


MetaPDF::IntroduceCold::IntroduceCold( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr, size_t ** _idPos )
  : Ast::BindNode( Ast::SourceLocation( _idLoc, _expr->loc( ) ), _idLoc, _id ), expr( _expr ), idPos( _idPos )
{ }

MetaPDF::IntroduceCold::~IntroduceCold( )
{
  delete id;
  delete expr;
}

void
MetaPDF::IntroduceCold::eval( Kernel::EvalState * evalState ) const
{
  if( *idPos == 0 )
    {
      *idPos = new size_t( evalState->env->findLocalPosition( loc, id ) );
    }

  evalState->env->define( idLoc, id->identifier( dstgroup, pdfo, metaState, env ), expr->value( dstgroup, pdfo, metaState, env ), false ); /* false means not constant */
}


MetaPDF::AssignStmt::AssignStmt( const Ast::SourceLocation & _idLoc, MetaPDF::IdentifierNode * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

MetaPDF::AssignStmt::~AssignStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
MetaPDF::AssignStmt::value( Kernel::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  env->redefine( idLoc, id->identifier( dstgroup, pdfo, metaState, env ), expr->value( dstgroup, pdfo, metaState, env ) );
  return MetaPDF::THE_VOID;
}
