#define UNARYDISPATCHBASEDECL static RefCountPtr< const Lang::Value > unaryDispatch( RefCountPtr< const Lang::Value > self, const ::Shapes::Ast::UnaryExpr * op );

#define UNARYDISPATCHBASEIMPL

#define UNARYDISPATCHDECL

#define UNARYDISPATCHIMPL( T )


#define BINARYDISPATCHBASEDECL static RefCountPtr< const Lang::Value > binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::Shapes::Ast::BinaryInfixExpr * op );

#define BINARYDISPATCHBASEIMPL

#define BINARYDISPATCHDECL

#define BINARYDISPATCHIMPL( Ts )
