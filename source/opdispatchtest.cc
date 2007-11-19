#include <iostream>

#include "/home/rt/tidefelt/include/refcount.h"

using namespace std;

class Value;
class A;
class B;
class C;

/* The closest to the one level recursion needed is to start from two sets of symmetric macros.
 * One set ends in "1", the other ends in "2".
 */

/* Define the class heierarcy in macros:
 */
#define CLASSTREE_A_1( Ma, S, Mb ) Ma( S, Mb, A )
#define CLASSTREE_B_1( Ma, S, Mb ) Ma( S, Mb, B ) CLASSTREE_C_1( Ma, S, Mb )
#define CLASSTREE_C_1( Ma, S, Mb ) Ma( S, Mb, C )
#define FORALLCLASSES1( Ma, S, Mb ) \
	CLASSTREE_A_1( Ma, S, Mb )\
	CLASSTREE_B_1( Ma, S, Mb )

#define CLASSTREE_A_2( Ma, S, Mb ) Ma( S, Mb, A )
#define CLASSTREE_B_2( Ma, S, Mb ) Ma( S, Mb, B ) CLASSTREE_C_2( Ma, S, Mb )
#define CLASSTREE_C_2( Ma, S, Mb ) Ma( S, Mb, C )
#define FORALLCLASSES2( Ma, S, Mb ) \
	CLASSTREE_A_2( Ma, S, Mb )\
	CLASSTREE_B_2( Ma, S, Mb )


/* Define convenient macros for looping.
 */
#define SINGLELOOP1_( Sb, M, T ) M( T )
#define SINGLELOOP1( Sa, M ) Sa( SINGLELOOP1_,, M )
#define DOUBLELOOP1__( M, Ta, Tb ) M( Ta, Tb )
#define DOUBLELOOP1_( Sb, M, Ta ) Sb( DOUBLELOOP1__, M, Ta )
#define DOUBLELOOP1( Sa, Sb, M ) Sa( DOUBLELOOP1_, Sb, M )

#define SINGLELOOP2_( Sb, M, T ) M( T )
#define SINGLELOOP2( Sa, M ) Sa( SINGLELOOP2_,, M )
#define DOUBLELOOP2__( M, Ta, Tb ) M( Ta, Tb )
#define DOUBLELOOP2_( Sb, M, Ta ) Sb( DOUBLELOOP2__, M, Ta )
#define DOUBLELOOP2( Sa, Sb, M ) Sa( DOUBLELOOP2_, Sb, M )

/* Define a macro that only uses the first argument,
 * resulting in a macro that calls another macro for each class.
 */
#define FORALLCLASSESM( M ) SINGLELOOP1( FORALLCLASSES1, M )

/* Define a macro that calls another macro with a given first argument,
 * varying the second argument over all classes.
 */
#define FORALLCLASSESMT( M, T ) FORALLCLASSES2( DOUBLELOOP2__, M, T )

/* Use the two looping macros above in a nested manner to produce a default method
 * for each pair combination of classes.
 */
#define DEFAULTOP__( Ta, Tb ) virtual int op( RefCountPtr< const Ta > x1, RefCountPtr< const Tb > x2 ) const { return 0; }
#define DEFAULTOP_( Ta ) FORALLCLASSESMT( DEFAULTOP__, Ta )
#define DEFAULTOP FORALLCLASSESM( DEFAULTOP_ )

/* Define a macro for hiding the default method for a set of classes:
 */
#define HIDEDEFAULT_( Ta, Tb )	virtual int op( Ta * v1, Tb * v2 ) const { return 0; }
#define HIDEDEFAULT( Sa, Sb ) DOUBLELOOP2( Sa, Sb, HIDEDEFAULT_ )

/* Define a macro for hiding the default method for a set of classes:
 */
#define CALLIMPL_( Ta, Tb )	virtual int op( RefCountPtr< const Ta > x1, RefCountPtr< const Tb > x2 ) const { return impl( x1.getPtr( ), x1, x2.getPtr( ), x2 ); }
#define CALLIMPL( Sa, Sb ) DOUBLELOOP2( Sa, Sb, CALLIMPL_ )

class BinOp
{
public:
	int call( RefCountPtr< const Value > v1, RefCountPtr< const Value > v2 ) const;
	DEFAULTOP;
};

#define DISPATCH1NULLDECL_ virtual int dispatch1( RefCountPtr< const Value > self, RefCountPtr< const Value > other, const BinOp * op ) const = 0;
#define DISPATCH2NULLDECL_( Ts, To ) virtual int dispatch2( RefCountPtr< const To > other, RefCountPtr< const Value > self, const BinOp * op ) const = 0;

#define DISPATCHNULLDECL DISPATCH1NULLDECL_ FORALLCLASSESMT( DISPATCH2NULLDECL_, )

class Value
{
public:
	DISPATCHNULLDECL;
};

int
BinOp::call( RefCountPtr< const Value > v1, RefCountPtr< const Value > v2 ) const
{
	return v1->dispatch1( v1, v2, this );
}

#define DISPATCH1DECL_ virtual int dispatch1( RefCountPtr< const Value > self, RefCountPtr< const Value > other, const BinOp * op ) const;
#define DISPATCH1IMPL_( Ts ) \
int Ts::dispatch1( RefCountPtr< const Value > self, RefCountPtr< const Value > other, const BinOp * op ) const				\
	{\
		RefCountPtr< const Ts > typedSelf = self.down_cast< const Ts >( );				\
		if( typedSelf == NullPtr< const Ts >( ) )																\
			{\
				cerr << "Downcast in dispatch1 failed" << endl ;\
			}\
		return other->dispatch2( typedSelf, other, op );\
	}\

#define DISPATCH2DECL_( Ts, To ) virtual int dispatch2( RefCountPtr< const To > other, RefCountPtr< const Value > self, const BinOp * op ) const;
#define DISPATCH2IMPL_( Ts, To ) \
int Ts::dispatch2( RefCountPtr< const To > other, RefCountPtr< const Value > self, const BinOp * op ) const\
	{\
		RefCountPtr< const Ts > typedSelf = self.down_cast< const Ts >( );				\
		if( typedSelf == NullPtr< const Ts >( ) )																\
			{\
				cerr << "Downcast in dispatch1 failed" << endl ;\
			}\
		return op->op( other, typedSelf );\
	}

#define DISPATCHDECL DISPATCH1DECL_ FORALLCLASSESMT( DISPATCH2DECL_, )
#define DISPATCHIMPL( Ts ) DISPATCH1IMPL_( Ts ) FORALLCLASSESMT( DISPATCH2IMPL_, Ts )

class A : public Value
{
public:
	DISPATCHDECL;
};

class B : public Value
{
public:
	DISPATCHDECL;
};

class C : public B
{
public:
	DISPATCHDECL;
};

DISPATCHIMPL( A );
DISPATCHIMPL( B );
DISPATCHIMPL( C );

#define DUMMYANDREF( T ) T *, RefCountPtr< T > 

class Plus : public BinOp
{
public:
	CALLIMPL( CLASSTREE_B_1, CLASSTREE_A_2 )
private:
	int impl( DUMMYANDREF( const B ) v1, DUMMYANDREF( const A ) v2 ) const { return 1; }
	int impl( DUMMYANDREF( const C ) v1, DUMMYANDREF( const A ) v2 ) const { return 2; }
};

class Star : public BinOp
{
public:
	CALLIMPL( CLASSTREE_B_1, CLASSTREE_B_2 )
private:
	int impl( DUMMYANDREF( const B ) v1, DUMMYANDREF( const B ) v2 ) const { return 1; }
	int impl( DUMMYANDREF( const A ) v1, DUMMYANDREF( const B ) v2 ) const { return 7; }
};


/*
int
dispatch( Value * v1, Value * v2, const BinOp * op )
{

}
*/

int
main( )
{
	RefCountPtr< A > a;
	RefCountPtr< B > b;
	RefCountPtr< C > c;
	{
		Plus op;
		const char opstr[] = "+";
		cout << "A" << opstr << "A => " << op.call( a, a ) << endl ;
		cout << "A" << opstr << "B => " << op.call( a, b ) << endl ;
		cout << "A" << opstr << "C => " << op.call( a, c ) << endl ;
		cout << "B" << opstr << "A => " << op.call( b, a ) << endl ;
		cout << "B" << opstr << "B => " << op.call( b, b ) << endl ;
		cout << "B" << opstr << "C => " << op.call( b, c ) << endl ;
		cout << "C" << opstr << "A => " << op.call( c, a ) << endl ;
		cout << "C" << opstr << "B => " << op.call( c, b ) << endl ;
		cout << "C" << opstr << "C => " << op.call( c, c ) << endl ;
	}
	cout << endl ;
	{
		Star op;
		const char opstr[] = "*";
		cout << "A" << opstr << "A => " << op.call( a, a ) << endl ;
		cout << "A" << opstr << "B => " << op.call( a, b ) << endl ;
		cout << "A" << opstr << "C => " << op.call( a, c ) << endl ;
		cout << "B" << opstr << "A => " << op.call( b, a ) << endl ;
		cout << "B" << opstr << "B => " << op.call( b, b ) << endl ;
		cout << "B" << opstr << "C => " << op.call( b, c ) << endl ;
		cout << "C" << opstr << "A => " << op.call( c, a ) << endl ;
		cout << "C" << opstr << "B => " << op.call( c, b ) << endl ;
		cout << "C" << opstr << "C => " << op.call( c, c ) << endl ;
	}
	return 0;
}
