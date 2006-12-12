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




MetaPDF::UserLength::UserLength( const Ast::SourceLocation & _loc, double _val, const char * _unit )
  : loc( _loc ), val( _val ), unit( _unit )
{ }

MetaPDF::UserLength::~UserLength( )
{
  delete unit;
}

void
MetaPDF::UserLength::eval( Kernel::EvalState * evalState ) const
{
  Kernel::ContRef cont = evalState->cont_;
  (*cont)( Kernel::ValueRef( new Lang::Length( val * evalState->env->lookupUnit( loc, unit ) ) ),
	   evalState );
}

const Ast::SourceLocation &
MetaPDF::UserLength::firstLoc( ) const
{
  return loc;
}

const Ast::SourceLocation &
MetaPDF::UserLength::lastLoc( ) const
{
  return loc;
}


MetaPDF::IntroduceUnitStmt::IntroduceUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

MetaPDF::IntroduceUnitStmt::~IntroduceUnitStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
MetaPDF::IntroduceUnitStmt::value( Kernel::HandleType dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  RefCountPtr< const Lang::Value > untypedVal = expr->value( dstgroup, pdfo, metaState, env );
  typedef const Lang::Length ArgType;
  ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
  if( val == 0 )
    {
      throw Exceptions::TypeMismatch( expr, untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
    }
  env->defineUnit( idLoc, id, val->getVal( ) );
  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::IntroduceUnitStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
MetaPDF::IntroduceUnitStmt::lastLoc( ) const
{
  return expr->lastLoc( );
}


MetaPDF::AssignUnitStmt::AssignUnitStmt( const Ast::SourceLocation & _idLoc, const char * _id, Ast::Expression * _expr )
  : idLoc( _idLoc ), id( _id ), expr( _expr )
{ }

MetaPDF::AssignUnitStmt::~AssignUnitStmt( )
{
  delete id;
  delete expr;
}

RefCountPtr< const Lang::Value >
MetaPDF::AssignUnitStmt::value( Kernel::HandleType dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
{
  RefCountPtr< const Lang::Value > untypedVal = expr->value( dstgroup, pdfo, metaState, env );
  typedef const Lang::Length ArgType;
  ArgType * val = dynamic_cast< ArgType * >( untypedVal.getPtr( ) );
  if( val == 0 )
    {
      throw Exceptions::TypeMismatch( expr, untypedVal->getTypeName( ), ArgType::staticTypeName( ) );
    }
  env->redefineUnit( idLoc, id, val->getVal( ) );
  return MetaPDF::THE_VOID;
}

const Ast::SourceLocation &
MetaPDF::AssignUnitStmt::firstLoc( ) const
{
  return idLoc;
}

const Ast::SourceLocation &
MetaPDF::AssignUnitStmt::lastLoc( ) const
{
  return expr->lastLoc( );
}

