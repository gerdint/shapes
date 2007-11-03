#define UNARYDISPATCHBASEDECL static RefCountPtr< const Lang::Value > unaryDispatch( RefCountPtr< const Lang::Value > self, const ::Shapes::Ast::UnaryExpr * op );

#define UNARYDISPATCHBASEIMPL \
  RefCountPtr< const Lang::Value >				\
  Lang::Value::unaryDispatch( RefCountPtr< const Lang::Value > self, const ::Shapes::Ast::UnaryExpr * op ) \
{\
  return THE_VOID;\
}

#define UNARYDISPATCHDECL

#define UNARYDISPATCHIMPL( T )


#define BINARYDISPATCHBASEDECL static RefCountPtr< const Lang::Value > binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::Shapes::Ast::BinaryInfixExpr * op );

#define BINARYDISPATCHBASEIMPL \
  RefCountPtr< const Lang::Value >				\
  Lang::Value::binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::Shapes::Ast::BinaryInfixExpr * op ) \
{\
  return THE_VOID;\
}

#define BINARYDISPATCHDECL

#define BINARYDISPATCHIMPL( Ts )
