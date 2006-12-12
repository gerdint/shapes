#define UNARYDISPATCHBASEDECL virtual RefCountPtr< const Lang::Value > unaryDispatch( RefCountPtr< const Lang::Value > self, const ::MetaPDF::Ast::UnaryExpr * op ) const = 0;

#define UNARYDISPATCHBASEIMPL

#define UNARYDISPATCHDECL virtual RefCountPtr< const Lang::Value > unaryDispatch( RefCountPtr< const Lang::Value > self, const ::MetaPDF::Ast::UnaryExpr * op ) const;

#define UNARYDISPATCHIMPL( T )					\
  RefCountPtr< const Lang::Value > ::MetaPDF::Lang::T::unaryDispatch( RefCountPtr< const Lang::Value > self, const ::MetaPDF::Ast::UnaryExpr * op ) const \
  {\
    RefCountPtr< const ::MetaPDF::Lang::T > typedSelf = self.down_cast< const ::MetaPDF::Lang::T >( ); \
    if( typedSelf == NullPtr< const ::MetaPDF::Lang::T >( ) )		\
      {\
	throw Exceptions::InternalError( strrefdup( "Downcast in unaryDispatch failed." ) ); \
      }\
    return op->op( typedSelf ); \
  }




#define BINARYDISPATCH1BASEDECL_ virtual RefCountPtr< const Lang::Value > binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::MetaPDF::Ast::BinaryInfixExpr * op ) const = 0;
#define BINARYDISPATCH2BASEDECL_( To ) virtual RefCountPtr< const Lang::Value > binaryDispatch2( RefCountPtr< const ::MetaPDF::Lang::To > other, RefCountPtr< const Lang::Value > self, const ::MetaPDF::Ast::BinaryInfixExpr * op ) const = 0;

#define BINARYDISPATCHBASEDECL BINARYDISPATCH1BASEDECL_ FORALLCLASSESM( BINARYDISPATCH2BASEDECL_ )

#define BINARYDISPATCHBASEIMPL


#define BINARYDISPATCH1DECL_ virtual RefCountPtr< const Lang::Value > binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::MetaPDF::Ast::BinaryInfixExpr * op ) const;

#define BINARYDISPATCH1IMPL_( Ts ) \
  RefCountPtr< const Lang::Value > ::MetaPDF::Ts::binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::MetaPDF::Ast::BinaryInfixExpr * op ) const \
  {\
    RefCountPtr< const ::MetaPDF::Lang::Ts > typedSelf = self.down_cast< const ::MetaPDF::Lang::Ts >( ); \
    if( typedSelf == NullPtr< const ::MetaPDF::Lang::Ts >( ) )		\
      {\
	throw Exceptions::InternalError( strrefdup( "Downcast in binaryDispatch1 failed." ) ); \
      }\
    return other->binaryDispatch2( typedSelf, other, op ); \
  } 


#define BINARYDISPATCH2DECL_( To ) virtual RefCountPtr< const Lang::Value > binaryDispatch2( RefCountPtr< const ::MetaPDF::Lang::To > other, RefCountPtr< const Lang::Value > self, const ::MetaPDF::Ast::BinaryInfixExpr * op ) const;

#define BINARYDISPATCH2IMPL_( Ts, To ) \
  RefCountPtr< const Lang::Value > ::MetaPDF::Lang::Ts::binaryDispatch2( RefCountPtr< const ::MetaPDF::Lang::To > other, RefCountPtr< const Lang::Value > self, const ::MetaPDF::Ast::BinaryInfixExpr * op ) const \
  {\
    RefCountPtr< const ::MetaPDF::Lang::Ts > typedSelf = self.down_cast< const ::MetaPDF::Lang::Ts >( ); \
    if( typedSelf == NullPtr< const ::MetaPDF::Lang::Ts >( ) )		\
      {\
	throw Exceptions::InternalError( strrefdup( "Downcast in binaryDispatch2 failed." ) ); \
      }\
    return op->op( other, typedSelf ); \
  }

#define BINARYDISPATCHDECL BINARYDISPATCH1DECL_ FORALLCLASSESM( BINARYDISPATCH2DECL_ )
#define BINARYDISPATCHIMPL( Ts ) BINARYDISPATCH1IMPL_( Ts ) FORALLCLASSESMT( BINARYDISPATCH2IMPL_, Ts )
