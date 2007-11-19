#include "refcount.h"
#include "charptrless.h"

#include <iostream>
#include <map>

using namespace std;

class Environment
{
	map< const char *, int, charPtrLess > bindings;
public:
	void bind( const char * var, int val )
	{
		bindings[ var ] = val;
	}
	int lookup( const char * var )
	{
		return bindings[ var ];
	}
};


class Expression;
class Continuation;

typedef RefCountPtr< Environment > EnvRef;
typedef RefCountPtr< const Continuation > Kernel::ContRef;


class Continuation
{
public:
	static size_t counter;
	static void showCounter( std::ostream & os )
	{
		for( size_t i = counter; i > 0; --i )
			{
				os << "*" ;
			}
		os << endl ;
	}
	Continuation( )
	{
		++counter;
		cerr << "+" ;
		showCounter( cerr );
	}
	virtual ~Continuation( )
	{
		--counter;
		cerr << "-" ;
		showCounter( cerr );
	}
	virtual void operator () ( int val, Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const = 0;
};

size_t Continuation::counter = 0;

class PrintAndExit : public Continuation
{
public:
	virtual ~PrintAndExit( ) { }
	virtual void operator () ( int val, Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		cout << "Exit value: " << val << endl ;
		exit( 0 );
	}
};

class Return : public Continuation
{
	bool * flag;
	int * res;
public:
	Return( bool * _flag, int * _res ) : flag( _flag ), res( _res ) { }
	virtual void operator () ( int val, Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		*flag = true;
		*res = val; 
 }
	virtual ~Return( ) { }
};

class Expression
{
public:
	virtual void eval( Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const = 0;
};

class Variable : public Expression
{
	const char * var;
public:
	Variable( const char * _var ) : var( _var ) { }
	virtual void eval( Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		int val = (*evalEnv)->lookup( var );
		Kernel::ContRef cont = *evalCont;
		cont->operator () ( val, evalExpr, evalEnv, evalCont );
	}
};

class PlusContinuation2 : public Continuation
{
	int t1;
	Kernel::ContRef cont;
public:
	
	PlusContinuation2( int _t1, const Kernel::ContRef & _cont ) : t1( _t1 ), cont( _cont ) { }
	virtual ~PlusContinuation2( ) { }
	virtual void operator () ( int val, Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		(*cont)( t1 + val, evalExpr, evalEnv, evalCont );
	}
};

class PlusContinuation1 : public Continuation
{
public:
	Expression * t2;
	Kernel::ContRef cont;
	EnvRef env;
	
	PlusContinuation1( Expression * _t2, const Kernel::ContRef & _cont, EnvRef _env ) : t2( _t2 ), cont( _cont ), env( _env ) { }
	virtual ~PlusContinuation1( ) { }
	virtual void operator () ( int val, Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		*evalExpr = t2;
		*evalEnv = env;
		*evalCont = Kernel::ContRef( new PlusContinuation2( val, cont ) );
	}
};

class PlusExpression : public Expression
{
public:
	Expression * t1;
	Expression * t2;

	PlusExpression( Expression * _t1, Expression * _t2 ) : t1( _t1 ), t2( _t2 ) { }
	virtual void eval( Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		*evalExpr = t1;
		/* not changing env */
		*evalCont = Kernel::ContRef( new PlusContinuation1( t2, *evalCont, *evalEnv ) );
	}
};


class IfContinuation : public Continuation
{
	Expression * consequence;
	Expression * alternative;
	Kernel::ContRef cont;
	EnvRef env;
public:
	IfContinuation( Expression * _consequence, Expression * _alternative, const Kernel::ContRef & _cont, EnvRef _env ) : consequence( _consequence ), alternative( _alternative ), cont( _cont ), env( _env ) { }
	~IfContinuation( ) { }
	virtual void operator () ( int val, Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		switch( val )
			{
			case 1:
				*evalExpr = consequence;
				break;
			case 0:
				*evalExpr = alternative;
				break;
			default:
				cerr << "Only the integers 0 and 1 are valid values for predicates." << endl ;
				exit( 1 );
			}
		*evalEnv = env;
		*evalCont = cont;
	}
};

class IfExpression : public Expression
{
public:
	Expression * predicate;
	Expression * consequence;
	Expression * alternative;

	IfExpression( Expression * _predicate, Expression * _consequence, Expression * _alternative ) : predicate( _predicate ), consequence( _consequence ), alternative( _alternative ) { }
	virtual void eval( Expression ** evalExpr, EnvRef * evalEnv, Kernel::ContRef * evalCont ) const
	{
		*evalExpr = predicate;
		*evalCont = Kernel::ContRef( new IfContinuation( consequence, alternative, *evalCont, *evalEnv ) );
		/* not changing evalEnv */
	}	
};


int
main( )
{
	EnvRef globalEnv = EnvRef( new Environment );
	globalEnv->bind( "a", 8 );
	globalEnv->bind( "b", 5 );
	globalEnv->bind( "true", 1 );
	globalEnv->bind( "false", 0 );

	Expression * program =
		new PlusExpression( new Variable( "b" ),
												new PlusExpression( new IfExpression( new Variable( "true" ),
																															new Variable( "a" ),
																															new Variable( "b" ) ),
																						new Variable( "true" ) ) );


	Expression * evalExpr = program;
	EnvRef evalEnv = globalEnv;
	
	{
		bool exitFlag = false;
		int res;
		Kernel::ContRef evalCont = Kernel::ContRef( new Return( & exitFlag, & res ) );
		
		while( ! exitFlag )
			{
				evalExpr->eval( & evalExpr, & evalEnv, & evalCont );
			}

		cout << "Program returned " << res << endl ;
	}

	cout << "Live continuations: " << Continuation::counter << endl ;

	return 1;
}
