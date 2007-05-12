/* Use the two looping macros above in a nested manner to produce a default method declaration
 * for each pair combination of classes.
 */
#define DEFAULTUNARYOPDECL_( Ta ) virtual RefCountPtr< const Lang::Value > op( const RefCountPtr< const Lang::Ta > & arg, const Kernel::PassedDyn & dyn ) const;
#define DEFAULTUNARYOPDECL FORALLCLASSESM( DEFAULTUNARYOPDECL_ )

/* ... and an implementation.
 */
#define DEFAULTUNARYOPIMPL_( Ts, Ta ) RefCountPtr< const Lang::Value > Ast::Ts::op( const RefCountPtr< const Lang::Ta > & arg, const Kernel::PassedDyn & dyn ) const { return this->throwNotApplicable( arg.getPtr( ) ); }
#define DEFAULTUNARYOPIMPL( Ts ) FORALLCLASSESMT( DEFAULTUNARYOPIMPL_, Ts )

/* Define a macro for calling the implementation of an unary operator.
 */
#define UNARYCALLIMPL_( T ) virtual RefCountPtr< const Lang::Value > op( const RefCountPtr< const Lang::T > & arg, const Kernel::PassedDyn & dyn ) const { return impl( arg.getPtr( ), arg, dyn ); }
#define UNARYCALLIMPL( T ) SINGLELOOP1( T, UNARYCALLIMPL_ )



/* Use the two looping macros above in a nested manner to produce a default method declaration
 * for each pair combination of classes.
 */
#define DEFAULTBINARYOPDECL__( Ta, Tb ) virtual RefCountPtr< const Lang::Value > op( const RefCountPtr< const Lang::Ta > & arg1, const RefCountPtr< const Lang::Tb > & arg2, const Kernel::PassedDyn & dyn ) const;
#define DEFAULTBINARYOPDECL_( Ta ) FORALLCLASSESMT( DEFAULTBINARYOPDECL__, Ta )
#define DEFAULTBINARYOPDECL FORALLCLASSESM( DEFAULTBINARYOPDECL_ )

/* ... and an implementation.
 */
#define DEFAULTBINARYOPIMPL__( Ta, Tb ) RefCountPtr< const Lang::Value > Ast::BinaryInfixExpr::op( const RefCountPtr< const Lang::Ta > & arg1, const RefCountPtr< const Lang::Tb > & arg2, const Kernel::PassedDyn & dyn ) const { return throwNotApplicable( arg1.getPtr( ), arg2.getPtr( ) ); }
#define DEFAULTBINARYOPIMPL_( Ta ) FORALLCLASSESMT( DEFAULTBINARYOPIMPL__, Ta )
#define DEFAULTBINARYOPIMPL FORALLCLASSESM( DEFAULTBINARYOPIMPL_ )

#define DUMMYANDREF( T ) T *, const RefCountPtr< T > &
/* Define a macro for calling the implementation of a binary operator.
 */
#define CALLIMPL_( Ta, Tb ) virtual RefCountPtr< const Lang::Value > op( const RefCountPtr< const Lang::Ta > & arg1, const RefCountPtr< const Lang::Tb > & arg2, const Kernel::PassedDyn & dyn ) const { return impl( arg1.getPtr( ), arg1, arg2.getPtr( ), arg2, dyn ); }
#define CALLIMPL( Sa, Sb ) DOUBLELOOP2( Sa, Sb, CALLIMPL_ )
