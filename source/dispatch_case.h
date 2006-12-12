#define UNARYDISPATCHCASE( T )\
  case ::MetaPDF::Kernel::TYPEID_ ## T :		 \
  {\
    RefCountPtr< const ::MetaPDF::Lang::T > arg = self.down_cast< const ::MetaPDF::Lang::T >( ); \
    if( arg == NullPtr< const ::MetaPDF::Lang::T >( ) )				\
      {\
	throw Exceptions::InternalError( strrefdup( "Downcast in unary dispatch failed, " #T ) ); \
      }\
    return op->op( arg ); \
  }\
  break;

#define UNARYDISPATCHBASEDECL static RefCountPtr< const Lang::Value > unaryDispatch( RefCountPtr< const Lang::Value > self, const MetaPDF::Ast::UnaryExpr * op );

#define UNARYDISPATCHBASEIMPL \
  RefCountPtr< const Lang::Value >				\
  Lang::Value::unaryDispatch( RefCountPtr< const Lang::Value > self, const MetaPDF::Ast::UnaryExpr * op ) \
{\
  switch( self->getTypeID( ) )\
    {\
      SINGLELOOP1( CLASSTREE1_ROOT, UNARYDISPATCHCASE );		\
    default:\
      throw Exceptions::InternalError( strrefdup( "QuickTypeID out of range in unary dispatch." ) ); \
    }\
}

#define UNARYDISPATCHDECL

#define UNARYDISPATCHIMPL( T )


#define BINARYDISPATCHCASE_LEVEL1( T1 )\
  case ::MetaPDF::Kernel::TYPEID_ ## T1 :		 \
  {\
    RefCountPtr< const ::MetaPDF::Lang::T1 > arg1 = self.down_cast< const ::MetaPDF::Lang::T1 >( ); \
    if( arg1 == NullPtr< const ::MetaPDF::Lang::T1 >( ) )			\
      {\
	throw Exceptions::InternalError( strrefdup( "Downcast in binary dispatch level 1 failed, " #T1 ) ); \
      }\
    return binaryDispatch2( arg1, other, op ); \
  }\
  break;

#define BINARYDISPATCH2CASE_LEVEL2( T2 )\
  case ::MetaPDF::Kernel::TYPEID_ ## T2 :		 \
  {\
    RefCountPtr< const ::MetaPDF::Lang::T2 > arg2 = other.down_cast< const ::MetaPDF::Lang::T2 >( ); \
    if( arg2 == NullPtr< const ::MetaPDF::Lang::T2 >( ) )			\
      {\
	throw Exceptions::InternalError( strrefdup( "Downcast in binary dispatch level 2 failed, " #T2 ) ); \
      }\
    return op->op( arg1, arg2 ); \
  }\
  break;

#define BINARYDISPATCH2CASE_LEVEL1( T1 )\
  RefCountPtr< const Lang::Value >				\
  Lang::Value::binaryDispatch2( RefCountPtr< const ::MetaPDF::Lang::T1 > arg1, RefCountPtr< const Lang::Value > other, const ::MetaPDF::Ast::BinaryInfixExpr * op ) \
{\
  switch( other->getTypeID( ) )						\
    {									\
      SINGLELOOP2( CLASSTREE2_ROOT, BINARYDISPATCH2CASE_LEVEL2 );	\
    default:								\
      throw Exceptions::InternalError( strrefdup( "QuickTypeID out of range in binary dispatch level 2." ) ); \
    }									\
}


#define BINARYDISPATCH2DECL( T ) static RefCountPtr< const Lang::Value > binaryDispatch2( RefCountPtr< const ::MetaPDF::Lang::T > self, RefCountPtr< const Lang::Value > other, const ::MetaPDF::Ast::BinaryInfixExpr * op );

#define BINARYDISPATCHBASEDECL \
  static RefCountPtr< const Lang::Value > binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::MetaPDF::Ast::BinaryInfixExpr * op ); \
  SINGLELOOP1( CLASSTREE1_ROOT, BINARYDISPATCH2DECL )

#define BINARYDISPATCHBASEIMPL \
  RefCountPtr< const Lang::Value >				\
  Lang::Value::binaryDispatch1( RefCountPtr< const Lang::Value > self, RefCountPtr< const Lang::Value > other, const ::MetaPDF::Ast::BinaryInfixExpr * op ) \
{\
  switch( self->getTypeID( ) )\
    {\
      SINGLELOOP1( CLASSTREE1_ROOT, BINARYDISPATCHCASE_LEVEL1 );		\
    default:\
      throw Exceptions::InternalError( strrefdup( "QuickTypeID out of range in binary dispatch level 1." ) ); \
    }\
}\
  SINGLELOOP1( CLASSTREE1_ROOT, BINARYDISPATCH2CASE_LEVEL1 )

#define BINARYDISPATCHDECL

#define BINARYDISPATCHIMPL( Ts )
