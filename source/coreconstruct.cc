#include <cmath>

#include "Shapes_Helpers_decls.h"

#include "shapescore.h"
#include "globals.h"
#include "shapesexceptions.h"
#include "consts.h"
#include "simplepdfi.h"
#include "simplepdfo.h"
#include "astfun.h"
#include "tagtypes.h"
#include "multipage.h"
#include "charconverters.h"
#include "pagecontentstates.h"
#include "texlabelmanager.h"
#include "autoonoff.h"
#include "timetypes.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include <ctime>

using namespace Shapes;


namespace Shapes
{
  namespace Lang
  {

    class Core_tag : public Lang::CoreFunction
    {
    public:
      Core_tag( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "label", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "obj", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "transform", Kernel::THE_TRUE_VARIABLE );  // this argument means "transform if applicable"
	formals_->appendEvaluatedCoreFormal( "draw", Kernel::THE_TRUE_VARIABLE );  // this argument means "draw if applicable"
      }
      
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );
	
	typedef const Lang::Symbol KeyType;
	RefCountPtr< KeyType > key = Helpers::down_cast_CoreArgument< KeyType >( title_, args, 0, callLoc );
	bool tryTransform = Helpers::down_cast_CoreArgument< const Lang::Boolean >( title_, args, 2, callLoc )->val_;
	bool tryDraw = Helpers::down_cast_CoreArgument< const Lang::Boolean >( title_, args, 3, callLoc )->val_;
	
	if( tryDraw && ! tryTransform )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 3, "A tagged object which does not transform cannot be drawn." );
	  }
	
	size_t argsi = 1;
	
	if( tryDraw )
	  {
	    try
	      {
		typedef const Lang::Drawable2D ArgType;
	  
		RefCountPtr< ArgType > obj = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
	  
		Kernel::ContRef cont = evalState->cont_;
		cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::TaggedDrawable2D( key, obj ) ),
				 evalState );
		return;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }

	    try
	      {
		typedef const Lang::Drawable3D ArgType;
	  
		RefCountPtr< ArgType > obj = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
	  
		Kernel::ContRef cont = evalState->cont_;
		cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::TaggedDrawable3D( key, obj ) ),
				 evalState );
		return;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }
	  }

	if( tryTransform )
	  {
	    try
	      {
		typedef const Lang::Geometric2D ArgType;
	  
		RefCountPtr< ArgType > obj = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
	  
		Kernel::ContRef cont = evalState->cont_;
		cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::TaggedGeometric2D( key, obj ) ),
				 evalState );
		return;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }

	    try
	      {
		typedef const Lang::Geometric3D ArgType;
	  
		RefCountPtr< ArgType > obj = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( argsi ) );
	  
		Kernel::ContRef cont = evalState->cont_;
		cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::TaggedGeometric3D( key, obj ) ),
				 evalState );
		return;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }
	  }

	{
	  // As a last resort, we tag any value.
	  // We return a Drawable2D.  Use immerse to make it 3D.
	  Kernel::ContRef cont = evalState->cont_;
	  cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::TaggedValue2D( key, args.getValue( argsi ) ) ),
			   evalState );
	}
      }
    };
    
    // This function is in this file just because it i so related to Core_tag.
    class Core_find : public Lang::CoreFunction
    {
    public:
      Core_find( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "container", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "label", Kernel::THE_SLOT_VARIABLE );
      }
      
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );
	
	typedef const Lang::Symbol KeyType;
	RefCountPtr< KeyType > key = Helpers::down_cast_CoreArgument< KeyType >( title_, args, 1, callLoc );
	
	size_t argsi = 0;
	
	try
	  {
	    typedef const Lang::Drawable2D ContainerType;
      
	    RefCountPtr< ContainerType > container = Helpers::try_cast_CoreArgument< ContainerType >( args.getValue( argsi ) );
      
	    if( ! container->findOneTag( evalState, key->getKey( ), Lang::THE_2D_IDENTITY ) )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, 1, "Label not found." );
	      }
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }
  
	try
	  {
	    typedef const Lang::Drawable3D ContainerType;
      
	    RefCountPtr< ContainerType > container = Helpers::try_cast_CoreArgument< ContainerType >( args.getValue( argsi ) );
      
	    if( ! container->findOneTag( evalState, key->getKey( ), Lang::THE_3D_IDENTITY ) )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, 1, "Label not found." );
	      }
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }
  
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, argsi, Helpers::typeSetString( Lang::Drawable2D::staticTypeName( ), Lang::Drawable3D::staticTypeName( ) ) );
      }
    };
    
    // This function is in this file just because it i so related to Core_tag.
    class Core_findall : public Lang::CoreFunction
    {
    public:
      Core_findall( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "container", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "label", Kernel::THE_SLOT_VARIABLE );
      }
      
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );
	
	typedef const Lang::Symbol KeyType;
	RefCountPtr< KeyType > key = Helpers::down_cast_CoreArgument< KeyType >( title_, args, 1, callLoc );
	
	size_t argsi = 0;
	
	try
	  {
	    typedef const Lang::Drawable2D ContainerType;
      
	    RefCountPtr< ContainerType > container = Helpers::try_cast_CoreArgument< ContainerType >( args.getValue( argsi ) );
      
	    std::vector< Kernel::ValueRef > * dst = new std::vector< Kernel::ValueRef >;
	    container->findTags( dst, evalState->dyn_, key->getKey( ), Lang::THE_2D_IDENTITY );

	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::VectorFunction( dst ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }
  
	try
	  {
	    typedef const Lang::Drawable3D ContainerType;
      
	    RefCountPtr< ContainerType > container = Helpers::try_cast_CoreArgument< ContainerType >( args.getValue( argsi ) );
      
	    std::vector< Kernel::ValueRef > * dst = new std::vector< Kernel::ValueRef >;
	    container->findTags( dst, evalState->dyn_, key->getKey( ), Lang::THE_3D_IDENTITY );

	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::VectorFunction( dst ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }
  
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, argsi, Helpers::typeSetString( Lang::Drawable2D::staticTypeName( ), Lang::Drawable3D::staticTypeName( ) ) );
      }
    };

    class Core_phong : public Lang::CoreFunction
    {
    public:
      Core_phong( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );

	typedef const Lang::Float ArgType;
	double exponent = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc )->val_;
	if( exponent < 0 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The Phong exponent should be greater than 0." );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::SpecularReflectionTerm( 1., exponent ) ),
			 evalState );
      }
    };

    class Core_cons : public Lang::CoreFunction
    {
    public:
      Core_cons( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, false ) )
      {
	formals_->appendEvaluatedCoreFormal( "car", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "cdr", Kernel::THE_SLOT_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 2;
	CHECK_ARITY( args, ARITY, title_ );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::ConsPair( args.getHandle( 0 ),
							       args.getHandle( 1 ) ) ),
			 evalState );
      }
    };

    class Core_list : public Lang::CoreFunction
    {
    public:
      Core_list( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	RefCountPtr< const Lang::SingleList > res = Lang::THE_CONS_NULL;
	if( args.size( ) > 0 )
	  {
	    for( size_t i = args.size( ) - 1; ; --i )
	      {
		res = RefCountPtr< const Lang::SingleList >( new Lang::SingleListPair( args.getHandle( i ), res ) );
		if( i == 0 )
		  {
		    break;
		  }
	      }
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( res,
			 evalState );
      }
    };

    class Core_isnull : public Lang::CoreFunction
    {
    public:
      Core_isnull( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );

	RefCountPtr< const Lang::Value > aUntyped = args.getValue( 0 );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::Boolean( dynamic_cast< const Lang::SingleListNull * >( args.getValue( 0 ).getPtr( ) ) != 0 ) ),
			 evalState );
      }
    };

    class Core_range : public Lang::CoreFunction
    {
    public:
      Core_range( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function range >", true ) )
      {
	formals_->appendEvaluatedCoreFormal( "begin", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "end", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "step", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	try
	  {
	    typedef const Lang::Integer ArgType;

	    ArgType::ValueType begin = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) )->val_;
	    ArgType::ValueType end = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc )->val_;
	    RefCountPtr< ArgType > stepPtr = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 2, callLoc, true );
    
	    ArgType::ValueType step = 1;
	    if( stepPtr != NullPtr< ArgType >( ) )
	      {
		step = stepPtr->val_;
	      }

	    std::list< ArgType::ValueType > tmp;

	    RefCountPtr< const Lang::SingleList > res = Lang::THE_CONS_NULL;
	    if( step > 0 )
	      {
		for( ArgType::ValueType x = begin; x <= end; x += step )
		  {
		    tmp.push_back( x );
		  }
	      }
	    else if( step < 0 )
	      {
		for( ArgType::ValueType x = begin; x >= end; x += step )
		  {
		    tmp.push_back( x );
		  }
	      }
	    else
	      {
		throw Exceptions::CoreOutOfRange( title_, args, 2, "Step size must not be zero." );
	      }

	    while( tmp.size( ) != 0 )
	      {
		res = RefCountPtr< const Lang::SingleList >( new Lang::SingleListPair( Helpers::newValHandle( new Lang::Integer( tmp.back( ) ) ),
										       res ) );
		tmp.pop_back( );
	      }
    
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( res,
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }

	try
	  {
	    typedef const Lang::Float ArgType;

	    double begin = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( 0 ) )->val_;
	    double end = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc )->val_;
	    RefCountPtr< ArgType > stepPtr = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 2, callLoc, true );
    
	    double step = 1;
	    if( stepPtr != NullPtr< ArgType >( ) )
	      {
		step = stepPtr->val_;
	      }

	    std::list< double > tmp;

	    RefCountPtr< const Lang::SingleList > res = Lang::THE_CONS_NULL;
	    if( step > 0 )
	      {
		for( double x = begin; x < end; x += step )
		  {
		    tmp.push_back( x );
		  }
	      }
	    else if( step < 0 )
	      {
		for( double x = begin; x > end; x += step )
		  {
		    tmp.push_back( x );
		  }
	      }
	    else
	      {
		throw Exceptions::CoreOutOfRange( title_, args, 2, "Step size must not be zero." );
	      }

	    while( tmp.size( ) != 0 )
	      {
		res = RefCountPtr< const Lang::SingleList >( new Lang::SingleListPair( Helpers::newValHandle( new Lang::Float( tmp.back( ) ) ),
										       res ) );
		tmp.pop_back( );
	      }
        
	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( res,
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }

	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Integer::staticTypeName( ), Lang::Float::staticTypeName( ) ) );
      }
    };

    class Core_affinetransform : public Lang::CoreFunction
    {
    public:
      Core_affinetransform( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 3;
	CHECK_ARITY( args, ARITY, title_ );

	typedef const Lang::FloatPair ArgType0;
	typedef const Lang::FloatPair ArgType1;
	typedef const Lang::Coords2D ArgType2;
  
	RefCountPtr< ArgType0 > argx = Helpers::down_cast_CoreArgument< ArgType0 >( title_, args, 0, callLoc );
	RefCountPtr< ArgType1 > argy = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, 1, callLoc );
	RefCountPtr< ArgType2 > arg1 = Helpers::down_cast_CoreArgument< ArgType2 >( title_, args, 2, callLoc );
    
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Transform2D( argx->x_, argx->y_, argy->x_, argy->y_, arg1->x_.get( ), arg1->y_.get( ) ) ),
			 evalState );
      }
    };

    class Core_affinetransform3D : public Lang::CoreFunction
    {
    public:
      Core_affinetransform3D( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 4;
	CHECK_ARITY( args, ARITY, title_ );

	typedef const Lang::FloatTriple ArgType0;
	typedef const Lang::FloatTriple ArgType1;
	typedef const Lang::FloatTriple ArgType2;
	typedef const Lang::Coords3D ArgType3;
  
	RefCountPtr< ArgType0 > argx = Helpers::down_cast_CoreArgument< ArgType0 >( title_, args, 0, callLoc );
	RefCountPtr< ArgType1 > argy = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, 1, callLoc );
	RefCountPtr< ArgType2 > argz = Helpers::down_cast_CoreArgument< ArgType2 >( title_, args, 2, callLoc );
	RefCountPtr< ArgType3 > arg1 = Helpers::down_cast_CoreArgument< ArgType3 >( title_, args, 3, callLoc );
  
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Transform3D( argx->x_, argx->y_, argx->z_,
								  argy->x_, argy->y_, argy->z_,
								  argz->x_, argz->y_, argz->z_,
								  arg1->x_.get( ), arg1->y_.get( ), arg1->z_.get( ) ) ),
			 evalState );
      }
    };

    class Core_shift : public Lang::CoreFunction
    {
    public:
      Core_shift( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );

	Kernel::ContRef cont = evalState->cont_;

	size_t i = 0;
	try
	  {
	    typedef const Lang::Coords2D ArgType1;
	    RefCountPtr< ArgType1 > arg1 = Helpers::try_cast_CoreArgument< ArgType1 >( args.getValue( i ) );
	    cont->takeValue( Kernel::ValueRef( new Lang::Transform2D( 1, 0, 0, 1, arg1->x_.get( ), arg1->y_.get( ) ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }

	try
	  {
	    typedef const Lang::Coords3D ArgType1;
	    RefCountPtr< ArgType1 > arg1 = Helpers::try_cast_CoreArgument< ArgType1 >( args.getValue( i ) );
	    cont->takeValue( Kernel::ValueRef( new Lang::Transform3D( 1, 0, 0, 0, 1, 0, 0, 0, 1, arg1->x_.get( ), arg1->y_.get( ), arg1->z_.get( ) ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }

	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, i, Helpers::typeSetString( Lang::Coords2D::staticTypeName( ), Lang::Coords3D::staticTypeName( ) ) );
      }
    };

    class Core_rotate : public Lang::CoreFunction
    {
    public:
      Core_rotate( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "angle", Kernel::THE_SLOT_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t i = 0;
	
	typedef const Lang::Float ArgType1;
	RefCountPtr< ArgType1 > arg1 = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, i, callLoc );
  
	double c = cos( arg1->val_ );
	double s = sin( arg1->val_ );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Transform2D( c, s, -s, c, 0, 0 ) ),
			 evalState );
      }
    };

    class Core_rotate3d : public Lang::CoreFunction
    {
    public:
      Core_rotate3d( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "dir", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "angle", Kernel::THE_SLOT_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t i = 0;
	
	typedef const Lang::FloatTriple ArgType1;
	RefCountPtr< ArgType1 > dir = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, i, callLoc );
	if( dir->x_ == 0 && dir->y_ == 0 && dir->z_ == 0 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, i, "The rotation direction is degenerate, that is (0,0,0)." );
	  }
  
	++i;

	typedef const Lang::Float ArgType2;
	RefCountPtr< ArgType2 > angle = Helpers::down_cast_CoreArgument< ArgType2 >( title_, args, i, callLoc );
  
	double r = sqrt( dir->x_ * dir->x_ + dir->y_ * dir->y_ + dir->z_ * dir->z_ );
	double x = dir->x_ / r;
	double y = dir->y_ / r;
	double z = dir->z_ / r;
	double x2 = x * x;
	double y2 = y * y;
	double z2 = z * z;
	double c = cos( angle->val_ );
	double s = sin( angle->val_ );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Transform3D( x2+(y2+z2)*c,  x*y*(1-c)+z*s, x*z*(1-c)-y*s,
								  x*y*(1-c)-z*s, y2+(x2+z2)*c,  y*z*(1-c)+x*s,
								  x*z*(1-c)+y*s, y*z*(1-c)-x*s, z2+(x2+y2)*c,
								  0, 0, 0 ) ),
			 evalState );
      }
    };

    class Core_scale : public Lang::CoreFunction
    {
    public:
      Core_scale( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	Kernel::VariableHandle one( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Float( 1 ) ) ) );

	formals_->appendEvaluatedCoreFormal( "r", one );
	formals_->appendEvaluatedCoreFormal( "x", one );
	formals_->appendEvaluatedCoreFormal( "y", one );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );
  
	typedef const Lang::Float ArgType;
	RefCountPtr< ArgType > argr = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );
	RefCountPtr< ArgType > argx = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc );
	RefCountPtr< ArgType > argy = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 2, callLoc );
  
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Transform2D( argr->val_ * argx->val_, 0,
								  0, argr->val_ * argy->val_,
								  0, 0 ) ),
			 evalState );
      }
    };

    class Core_scale3d : public Lang::CoreFunction
    {
    public:
      Core_scale3d( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	Kernel::VariableHandle one( new Kernel::Variable( RefCountPtr< const Lang::Value >( new Lang::Float( 1 ) ) ) );

	formals_->appendEvaluatedCoreFormal( "r", one );
	formals_->appendEvaluatedCoreFormal( "x", one );
	formals_->appendEvaluatedCoreFormal( "y", one );
	formals_->appendEvaluatedCoreFormal( "z", one );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	typedef const Lang::Float ArgType;
	RefCountPtr< ArgType > argr = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );
	RefCountPtr< ArgType > argx = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc );
	RefCountPtr< ArgType > argy = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 2, callLoc );
	RefCountPtr< ArgType > argz = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 3, callLoc );
  
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::Transform3D( argr->val_ * argx->val_, 0, 0,
								  0, argr->val_ * argy->val_, 0,
								  0, 0, argr->val_ * argz->val_,
								  0, 0, 0 ) ),
			 evalState );
      }
    };

    class Core_inverse : public Lang::CoreFunction
    {
    public:
      Core_inverse( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );

	{
	  typedef const Lang::Transform2D ArgType;
	  ArgType * tf = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( tf != 0 )
	    {
	      double det = tf->xx_ * tf->yy_ - tf->xy_ * tf->yx_;
	      if( fabs( det ) < Computation::SINGULAR_TRANSFORM_LIMIT )
		{
		  throw Exceptions::CoreOutOfRange( title_, args, 0, "Singular transforms cannot be inverted." );
		}
	      double idet = 1 / det;
	      double ixx = idet * tf->yy_;
	      double ixy = - idet * tf->xy_;
	      double iyx = - idet * tf->yx_;
	      double iyy = idet * tf->xx_;
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::Transform2D( ixx, iyx, 
									ixy, iyy,
									-( ixx * tf->xt_ + ixy * tf->yt_ ), -( iyx * tf->xt_ + iyy * tf->yt_ ) ) ),
			       evalState );
	      return;
	    }
	}

	{
	  typedef const Lang::Transform3D ArgType;
	  ArgType * tf = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( tf != 0 )
	    {
	      double det =
		tf->xx_ * ( tf->yy_ * tf->zz_ - tf->yz_ * tf->zy_ )
		- tf->xy_ * ( tf->yx_ * tf->zz_ - tf->yz_ * tf->zx_ )
		+ tf->xz_ * ( tf->yx_ * tf->zy_ - tf->yy_ * tf->zx_ );
	      if( fabs( det ) < Computation::SINGULAR_TRANSFORM_LIMIT )
		{
		  throw Exceptions::CoreOutOfRange( title_, args, 0, "Singular transforms cannot be inverted." );
		}
	      double idet = 1 / det;
	      double ixx = idet * ( tf->yy_ * tf->zz_ - tf->yz_ * tf->zy_ );
	      double ixy = - idet * ( tf->xy_ * tf->zz_ - tf->xz_ * tf->zy_ );
	      double ixz = idet * ( tf->xy_ * tf->yz_ - tf->xz_ * tf->yy_ );
	      double iyx = - idet * ( tf->yx_ * tf->zz_ - tf->yz_ * tf->zx_ );
	      double iyy = idet * ( tf->xx_ * tf->zz_ - tf->xz_ * tf->zx_ );
	      double iyz = - idet * ( tf->xx_ * tf->yz_ - tf->xz_ * tf->yx_ );
	      double izx = idet * ( tf->yx_ * tf->zy_ - tf->yy_ * tf->zx_ );
	      double izy = - idet * ( tf->xx_ * tf->zy_ - tf->xy_ * tf->zx_ );
	      double izz = idet * ( tf->xx_ * tf->yy_ - tf->xy_ * tf->yx_ );
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::Transform3D( ixx, iyx, izx,
									ixy, iyy, izy,
									ixz, iyz, izz,
									-( ixx * tf->xt_ + ixy * tf->yt_ + ixz * tf->zt_ ),
									-( iyx * tf->xt_ + iyy * tf->yt_ + iyz * tf->zt_ ),
									-( izx * tf->xt_ + izy * tf->yt_ + izz * tf->zt_ ) ) ),
			       evalState );
	      return;
	    }
	}

	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Transform2D::staticTypeName( ), Lang::Transform3D::staticTypeName( ) ) );
      }
    };

    class Core_formxo : public Lang::CoreFunction
    {
    public:
      Core_formxo( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	typedef const Lang::Drawable2D ArgType;
	RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

	RefCountPtr< const Lang::ElementaryPath2D > theBBox = arg->bbox( );
	Concrete::Coords2D llcorner( 0, 0 );
	Concrete::Coords2D urcorner( 0, 0 );
	if( ! theBBox->boundingRectangle( & llcorner, & urcorner ) )
	  {
	    std::string strTitle( title_ );
	    throw Exceptions::InternalError( strrefdup( strTitle + ": The object has no bounding box!" ) );
	  }

  
	RefCountPtr< SimplePDF::PDF_Stream_out > form;
	RefCountPtr< SimplePDF::PDF_Object > indirection = Kernel::the_pdfo->indirect( form );

	RefCountPtr< SimplePDF::PDF_Resources > resources;
	(*form)[ "Resources" ] = Kernel::the_pdfo->indirect( resources );

	(*form)[ "Subtype" ] = SimplePDF::PDF_out::newName( "Form" );
	(*form)[ "FormType" ] = SimplePDF::PDF_out::newInt( 1 );
	(*form)[ "BBox" ] = RefCountPtr< SimplePDF::PDF_Vector >( new SimplePDF::PDF_Vector( llcorner.x_.offtype< 1, 0 >( ), llcorner.y_.offtype< 1, 0 >( ),
											     urcorner.x_.offtype< 1, 0 >( ), urcorner.y_.offtype< 1, 0 >( ) ) );

	/* There's a possibility of adding a transformation matrix entry in the dictionary here, but it is not used, not even
	 * for transformed drawables.
	 */
	//  (*markForm)[ "Matrix" ] = RefCountPtr<PDF_Object>( new PDF_Vector( 1, 0, 0, 1, -30, -30 ) );

	Kernel::PageContentStates pdfState( resources );
	arg->shipout( form->data, & pdfState, Lang::Transform2D( 1, 0, 0, 1, 0, 0 ) );

  
	Lang::XObject * res = new Lang::XObject( indirection,
						 theBBox );
	res->setDebugStr( "user form" );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( res ),
			 evalState );
      }
    };

    class Core_transparencygroup : public Lang::CoreFunction
    {
    public:
      Core_transparencygroup( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( "< core function group >", true ) )
      {
	formals_->appendEvaluatedCoreFormal( "content", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "isolated", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "knockout", Kernel::THE_FALSE_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );
  
	typedef const Lang::Group2D ArgType0;
	typedef const Lang::Boolean ArgType1;
	typedef const Lang::Boolean ArgType2;
	RefCountPtr< ArgType0 > content = Helpers::down_cast_CoreArgument< ArgType0 >( title_, args, 0, callLoc );
	RefCountPtr< ArgType1 > isolated = Helpers::down_cast_CoreArgument< ArgType1 >( title_, args, 1, callLoc );
	RefCountPtr< ArgType2 > knockout = Helpers::down_cast_CoreArgument< ArgType2 >( title_, args, 2, callLoc );

	RefCountPtr< const Lang::ColorSpace > blendSpace = evalState->dyn_->getBlendSpace( );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Helpers::newTransparencyGroup( content, isolated->val_, knockout->val_, blendSpace ),
			 evalState );
      }
    };

    class Core_vector : public Lang::CoreFunction
    {
    public:
      Core_vector( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	std::vector< RefCountPtr< const Lang::Value > > * res = new std::vector< RefCountPtr< const Lang::Value > >;
	res->reserve( args.size( ) );

	for( size_t i = 0; i != args.size( ); ++i )
	  {
	    res->push_back( args.getValue( i ) );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::VectorFunction( res ) ),
			 evalState );
      }
    };

    class Core_interpolate : public Lang::CoreFunction
    {
    public:
      Core_interpolate( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "function", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "domain", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "size", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "range", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "encode", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "decode", Kernel::THE_VOID_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	throw Exceptions::NotImplemented( "Core_interpolate::call" );
      }
    };

    class Core_importPDFpages : public Lang::CoreFunction
    {
    public:
      Core_importPDFpages( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 1;
	CHECK_ARITY( args, ARITY, title_ );
  
	typedef const Lang::String ArgType;
	RefCountPtr< ArgType > arg = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );

	RefCountPtr< std::ifstream > iFile( new std::ifstream( arg->val_.getPtr( ) ) );
	if( ! iFile->good( ) )
	  {
	    std::ostringstream msg;
	    msg << "Failed to open file for input: " << arg->val_.getPtr( ) ;
	    throw Exceptions::CoreOutOfRange( title_, args, 0, strrefdup( msg ) );
	  }
	RefCountPtr< SimplePDF::PDF_in > pdfi( new SimplePDF::PDF_in( iFile ) );

	RefCountPtr< const std::vector< RefCountPtr< const Lang::XObject > > > typedRes = RefCountPtr< const std::vector< RefCountPtr< const Lang::XObject > > >( NullPtr< std::vector< RefCountPtr< const Lang::XObject > > >( ) );
	try
	  {
	    typedRes = Kernel::the_pdfo->addPagesAsXObjects( pdfi );
	  }
	catch( const char * ball )
	  {
	    throw Exceptions::InternalError( strrefdup( ( std::string( "An error occurred while importing " ) + arg->val_.getPtr( ) + ": " + ball ).c_str( ) ) );
	  }

	std::vector< RefCountPtr< const Lang::Value > > * untypedRes = new std::vector< RefCountPtr< const Lang::Value > >;
	untypedRes->reserve( typedRes->size( ) );
	typedef typeof *typedRes ListType;
	for( ListType::const_iterator i = typedRes->begin( ); i != typedRes->end( ); ++i )
	  {
	    untypedRes->push_back( *i );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::VectorFunction( untypedRes ) ),
			 evalState );
      }
    };

    class Core_sprintf : public Lang::CoreFunction
    {
    public:
      Core_sprintf( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	if( args.size( ) == 0 )
	  {
	    throw Exceptions::CoreArityMismatch( title_, 1, args.size( ) );
	  }
	typedef typeof args ArgListType;
	size_t i = 0;
	typedef const Lang::String Arg1Type;
	RefCountPtr< Arg1Type > arg1 = Helpers::down_cast_CoreArgument< Arg1Type >( title_, args, i, callLoc );
  
	/* snprintf( 0, 0, ... ) does not seem to work properly on some systems.
	 * Therefore, I resort to the use of a dummy string, and "n = 1".
	 */
	char * res = 0;
	int status;
	char dummy[1];
	switch( args.size( ) )
	  {
	  case 1:
	    {
	      size_t sz = snprintf( dummy, 1, arg1->val_.getPtr( ) );
	      res = new char[ sz + 1 ];
	      status = sprintf( res, arg1->val_.getPtr( ) );
	    }
	    break;
	  case 2:
	    {
	      ++i;
	      {
		typedef const Lang::String Arg2Type;
		Arg2Type * arg2 = dynamic_cast< Arg2Type * >( args.getValue( i ).getPtr( ) );
		if( arg2 != 0 )
		  {
		    size_t sz = snprintf( dummy, 1, arg1->val_.getPtr( ), arg2->val_.getPtr( ) );
		    res = new char[ sz + 1 ];
		    status = sprintf( res, arg1->val_.getPtr( ), arg2->val_.getPtr( ) );    
		    break;
		  }
	      }
	      {
		typedef const Lang::Float Arg2Type;
		Arg2Type * arg2 = dynamic_cast< Arg2Type * >( args.getValue( i ).getPtr( ) );
		if( arg2 != 0 )
		  {
		    size_t sz = snprintf( dummy, 1, arg1->val_.getPtr( ), arg2->val_ );
		    res = new char[ sz + 1 ];
		    status = sprintf( res, arg1->val_.getPtr( ), arg2->val_ );
		    break;
		  }
	      }
	      {
		typedef const Lang::Integer Arg2Type;
		Arg2Type * arg2 = dynamic_cast< Arg2Type * >( args.getValue( i ).getPtr( ) );
		if( arg2 != 0 )
		  {
		    size_t sz = snprintf( dummy, 1, arg1->val_.getPtr( ), arg2->val_ );
		    res = new char[ sz + 1 ];
		    status = sprintf( res, arg1->val_.getPtr( ), arg2->val_ );
		    break;
		  }
	      }
	      {
		typedef const Lang::ChronologicalTime Arg2Type;
		Arg2Type * arg2 = dynamic_cast< Arg2Type * >( args.getValue( i ).getPtr( ) );
		if( arg2 != 0 )
		  {
		    const char * fmt = arg1->val_.getPtr( );
		    const struct tm * tmp = arg2->temporary_localtime( );
		    size_t sz = strlen( fmt ) * 2;
		    res = new char[ sz ];
		    while( strftime( res, sz, fmt, tmp ) == 0 )
		      {
			delete res;
			sz *= 2;
			res = new char[ sz ];
		      }
		    status = 0; // Here, I'd like to check some error condition instead...
		    break;
		  }
	      }
	      throw Exceptions::CoreTypeMismatch( callLoc, title_, args, i, Interaction::SEVERAL_TYPES );
	    }
	    break;
	  default:
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The number of arguments is out of the implemented range." );
	  }

	if( res == 0 )
	  {
	    throw Exceptions::InternalError( "Failed to assign to res in sprintf." );
	  }

	if( status < 0 )
	  {
	    std::ostringstream oss;
	    oss << "Call to library routine returned negative value indicating an error: " << status << ".";
	    throw Exceptions::CoreOutOfRange( title_, args, 0, strrefdup( oss ) );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::String( res ) ),
			 evalState );
      }
    };

    class Core_strftime : public Lang::CoreFunction
    {
    public:
      Core_strftime( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	const size_t ARITY = 0;
	CHECK_ARITY( args, ARITY, title_ );

	time_t t;
	tm * timeInfo;
	t = time( 0 );
	timeInfo = localtime( & t );
	std::ostringstream res;
	res << timeInfo->tm_hour << ":" << timeInfo->tm_min << ":" << timeInfo->tm_sec ;

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::String( strdup( res.str( ).c_str( ) ) ) ),
			 evalState );
      }
    };

    class Core_ambient_light : public Lang::CoreFunction
    {
    public:
      Core_ambient_light( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "color", Kernel::THE_SLOT_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	{
	  typedef const Lang::Gray ArgType;
	  ArgType * col = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( col != 0 )
	    {
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::AmbientLightGray( col->components( ) ) ),
			       evalState );
	      return;
	    }
	}

	{
	  typedef const Lang::RGB ArgType;
	  ArgType * col = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( col != 0 )
	    {
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::AmbientLightRGB( col->components( ) ) ),
			       evalState );
	      return;
	    }
	}

	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Gray::staticTypeName( ), Lang::RGB::staticTypeName( ) ) );
      }
    };

    class Core_specular_light : public Lang::CoreFunction
    {
    public:
      Core_specular_light( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "color", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "radius", Helpers::newValHandle( new Lang::Length( HUGE_VAL ) ) );
	formals_->appendEvaluatedCoreFormal( "shadows", Kernel::THE_FALSE_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	typedef const Lang::Length RadiusType;
	RefCountPtr< RadiusType > radius = Helpers::down_cast_CoreArgument< RadiusType >( title_, args, 1, callLoc );
	typedef const Lang::Boolean ShadowsType;
	RefCountPtr< ShadowsType > shadows = Helpers::down_cast_CoreArgument< ShadowsType >( title_, args, 2, callLoc );

	{
	  typedef const Lang::Gray ArgType;
	  ArgType * col = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( col != 0 )
	    {
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::SpecularLightGray( Concrete::Coords3D( 0, 0, 0 ),
									      col->components( ),
									      radius->get( ),
									      shadows->val_ ) ),
			       evalState );
	      return;
	    }
	}

	{
	  typedef const Lang::RGB ArgType;
	  ArgType * col = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( col != 0 )
	    {
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::SpecularLightRGB( Concrete::Coords3D( 0, 0, 0 ),
									     col->components( ),
									     radius->get( ),
									     shadows->val_ ) ),
			       evalState );
	      return;
	    }
	}

	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Gray::staticTypeName( ), Lang::RGB::staticTypeName( ) ) );
      }
    };

    class Core_distant_light : public Lang::CoreFunction
    {
    public:
      Core_distant_light( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "color", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "shadows", Kernel::THE_FALSE_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	typedef const Lang::Boolean ShadowsType;
	RefCountPtr< ShadowsType > shadows = Helpers::down_cast_CoreArgument< ShadowsType >( title_, args, 1, callLoc );

	{
	  typedef const Lang::Gray ArgType;
	  ArgType * col = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( col != 0 )
	    {
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::DistantLightGray( Concrete::UnitFloatTriple( 0., 0., -1. ),
									     col->components( ),
									     shadows->val_ ) ),
			       evalState );
	      return;
	    }
	}

	{
	  typedef const Lang::RGB ArgType;
	  ArgType * col = dynamic_cast< ArgType * >( args.getValue( 0 ).getPtr( ) );
	  if( col != 0 )
	    {
	      Kernel::ContRef cont = evalState->cont_;
	      cont->takeValue( Kernel::ValueRef( new Lang::DistantLightRGB( Concrete::UnitFloatTriple( 0., 0., -1. ),
									    col->components( ),
									    shadows->val_ ) ),
			       evalState );
	      return;
	    }
	}
  
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, 0, Helpers::typeSetString( Lang::Gray::staticTypeName( ), Lang::RGB::staticTypeName( ) ) );
      }
    };

    class Core_textrenderingmode : public Lang::CoreFunction
    {
    public:
      Core_textrenderingmode( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "fill", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "stroke", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "clip", Kernel::THE_FALSE_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	typedef const Lang::Boolean FlagType;
	RefCountPtr< FlagType > fill =   Helpers::down_cast_CoreArgument< FlagType >( title_, args, 0, callLoc );
	RefCountPtr< FlagType > stroke = Helpers::down_cast_CoreArgument< FlagType >( title_, args, 1, callLoc );
	RefCountPtr< FlagType > clip =   Helpers::down_cast_CoreArgument< FlagType >( title_, args, 2, callLoc );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::TextRenderingMode( fill->val_, stroke->val_, clip->val_ ) ),
			 evalState );
      }
    };

    class Core_manualkern : public Lang::CoreFunction
    {
    public:
      Core_manualkern( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	// Note that res is _not_ yet const.  We use a RefCountPtr to take care of memory deallocation in case some argument has the wrong type and
	// the result is not used.
	RefCountPtr< Lang::KernedText > res( new Lang::KernedText( evalState->dyn_->getTextState( ), evalState->dyn_->getGraphicsState( ) ) );

	for( size_t i = 0; i != args.size( ); ++i )
	  {
	    try
	      {
		typedef const Lang::String ArgType;
		res->pushString( Helpers::try_cast_CoreArgument< ArgType >( args.getValue( i ) ) );
		continue;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }

	    try
	      {
		typedef const Lang::Float ArgType;
		res->pushKerning( Helpers::try_cast_CoreArgument< ArgType >( args.getValue( i ) )->val_ );
		continue;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }

	    try
	      {
		typedef const Lang::KernedText ArgType;
		Helpers::try_cast_CoreArgument< ArgType >( args.getValue( i ) )->push( res.getPtr( ) );
		continue;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }
      
	    throw Exceptions::CoreTypeMismatch( callLoc, title_, args, i, Helpers::typeSetString( Lang::String::staticTypeName( ), Lang::Float::staticTypeName( ), Lang::KernedText::staticTypeName( ) ) );
	  }

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( res,
			 evalState );
      }
    };

    class Core_automatickern : public Lang::CoreFunction
    {
    public:
      Core_automatickern( const char * title ) : CoreFunction( title ) { }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	iconv_t converter = Helpers::requireUTF8ToMacRomanConverter( );
	iconv_t backconverter = Helpers::requireMacRomanToUTF8Converter( );

	RefCountPtr< const FontMetrics::BaseFont > metrics = evalState->dyn_->getTextState( )->font_->metrics( );
	if( metrics->horizontalMetrics_ == NullPtr< FontMetrics::WritingDirectionMetrics >( ) )
	  {
	    throw Exceptions::FontMetricsError( evalState->dyn_->getTextState( )->font_->fontName( ), strrefdup( "No horizontal metrics defined." ) );
	  }

	// Note that res is _not_ yet const.  We use a RefCountPtr to take care of memory deallocation in case some argument has the wrong type and
	// the result is not used.
	RefCountPtr< Lang::KernedText > res( new Lang::KernedText( evalState->dyn_->getTextState( ), evalState->dyn_->getGraphicsState( ) ) );

	std::ostringstream pendingChars;
	unsigned char prevChar = 0;
	double pendingKerning = 0;

	size_t backbufSize = 5;
	char * backbuf = new char[ backbufSize ];
	DeleteOnExit< char > bufDeleter( backbuf );

	for( size_t i = 0; i != args.size( ); ++i )
	  {
	    try
	      {
		typedef const Lang::String ArgType;
		RefCountPtr< ArgType > str = Helpers::try_cast_CoreArgument< ArgType >( args.getValue( i ) );

		const char * inbuf = str->val_.getPtr( );

		size_t bufSize = strlen( inbuf ) + 1;
		char * buf = new char[ bufSize ];
		DeleteOnExit< char > bufDeleter( buf );

		char * outbuf = buf;
		size_t inbytesleft = bufSize - 1;
		size_t outbytesleft = bufSize - 1;
		// For some reason, my iconv header seems unaware of the const modifier...
		size_t count = iconv( converter,
				      & inbuf, & inbytesleft,
				      & outbuf, & outbytesleft );
		if( count == (size_t)(-1) )
		  {
		    if( errno == EILSEQ )
		      {
			throw Exceptions::MiscellaneousRequirement( "It is suspected that one of the UFT-8 characters used in showed text has no MacRoman representation." );
		      }
		    else if( errno == EINVAL )
		      {
			throw Exceptions::MiscellaneousRequirement( "It is suspected that showed text ended with an incomplete multibyte character." );
		      }
		    else if( errno == E2BIG )
		      {
			throw Exceptions::InternalError( "The buffer allocated for UTF-8 to MacRoman conversion was too small." );
		      }
		    else
		      {
			std::ostringstream msg;
			msg << "iconv failed with an unrecognized error code: " << errno ;
			throw Exceptions::InternalError( strrefdup( msg ) );
		      }
		  }
		*outbuf = '\0';
		for( const char * src = buf; *src != '\0'; ++src )
		  {
		    unsigned char currentChar = *reinterpret_cast< const unsigned char * >( src );
		    double currentKerning = pendingKerning - metrics->getHorizontalKernPairXByCode( prevChar, currentChar );
		    prevChar = currentChar;
		    pendingKerning = 0;
		    if( currentKerning != 0 )
		      {
			if( pendingChars.str( ).size( ) > 0 )
			  {
			    res->pushString( RefCountPtr< const Lang::String >( new Lang::String( strrefdup( pendingChars ) ) ) );
			    pendingChars.str( "" );
			  }
			res->pushKerning( currentKerning );
		      }
	      
		    // Copy the current (multibyte) character to the character queue
		    {
		      const char * inbuf = src;
		      char * outbuf = backbuf;
		      size_t inbytesleft = 1;
		      size_t outbytesleft = backbufSize;
		      // For some reason, my iconv header seems unaware of the const modifier...
		      size_t count = iconv( backconverter,
					    & inbuf, & inbytesleft,
					    & outbuf, & outbytesleft );
		      if( count == (size_t)(-1) )
			{
			  if( errno == EILSEQ )
			    {
			      throw Exceptions::ExternalError( "A character converted from UTF-8 could not be converted back to UFT-8." );
			    }
			  else if( errno == EINVAL )
			    {
			      throw Exceptions::ExternalError( "A character converted from UTF-8 was deemed incomplete." );
			    }
			  else if( errno == E2BIG )
			    {
			      throw Exceptions::InternalError( "The buffer allocated for conversion of a single character back to UTF-8 was too small." );
			    }
			  else
			    {
			      std::ostringstream msg;
			      msg << "iconv failed with an unrecognized error code: " << errno ;
			      throw Exceptions::InternalError( strrefdup( msg ) );
			    }
			}
		      *outbuf = '\0';
		      pendingChars << backbuf ;
		    }
		  }
		continue;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }

	    try
	      {
		typedef const Lang::Float ArgType;
		pendingKerning += Helpers::try_cast_CoreArgument< ArgType >( args.getValue( i ) )->val_;
		continue;
	      }
	    catch( const NonLocalExit::NotThisType & ball )
	      {
		// Never mind, see below
	      }
      
	    throw Exceptions::CoreTypeMismatch( callLoc, title_, args, i, Helpers::typeSetString( Lang::String::staticTypeName( ), Lang::Float::staticTypeName( ) ) );
	  }

	if( pendingChars.str( ).size( ) > 0 )
	  {
	    res->pushString( RefCountPtr< const Lang::String >( new Lang::String( strrefdup( pendingChars ) ) ) );
	    pendingChars.str( "" );
	  }
  
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( res,
			 evalState );
      }
    };

    class Core_newrandom : public Lang::CoreFunction
    {
    public:
      Core_newrandom( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "seed", Kernel::THE_SLOT_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "size", Helpers::newValHandle( new Lang::Integer( 32 ) ) );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	typedef const Lang::Integer SizeType;
	Lang::Integer::ValueType sz = Helpers::down_cast_CoreArgument< SizeType >( title_, args, 1, callLoc )->val_;
	if( sz < 8 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 1, "The size must be at least 8." );
	  }
	if( sz > 256 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 1, "The size must at most 256." );
	  }

	size_t argsi = 0;

	try
	  {
	    typedef const Lang::ChronologicalTime SeedType;
      
	    RefCountPtr< SeedType > seed = Helpers::try_cast_CoreArgument< SeedType >( args.getValue( argsi ) );

	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::HotRandomSeed( sz, seed->val( ) ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }
  
	try
	  {
	    typedef const Lang::Integer SeedType;
      
	    RefCountPtr< SeedType > seed = Helpers::try_cast_CoreArgument< SeedType >( args.getValue( argsi ) );

	    Kernel::ContRef cont = evalState->cont_;
	    cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::HotRandomSeed( sz, seed->val_ ) ),
			     evalState );
	    return;
	  }
	catch( const NonLocalExit::NotThisType & ball )
	  {
	    // Never mind, see below
	  }
  
	throw Exceptions::CoreTypeMismatch( callLoc, title_, args, argsi, Helpers::typeSetString( Lang::Integer::staticTypeName( ), Lang::ChronologicalTime::staticTypeName( ) ) );
      }
    };

    class Core_devrandom : public Lang::CoreFunction
    {
    public:
      Core_devrandom( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendCoreStateFormal( "device" );
	formals_->appendEvaluatedCoreFormal( "size", Helpers::newValHandle( new Lang::Integer( 32 ) ) );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	typedef const Lang::Integer SizeType;
	Lang::Integer::ValueType sz = Helpers::down_cast_CoreArgument< SizeType >( title_, args, 0, callLoc )->val_;
	if( sz < 8 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The size must be at least 8." );
	  }
	if( sz > 256 )
	  {
	    throw Exceptions::CoreOutOfRange( title_, args, 0, "The size must at most 256." );
	  }

	typedef Kernel::WarmRandomDevice GeneratorType;
	GeneratorType * gen = Helpers::down_cast_CoreState< GeneratorType >( title_, args, 0, callLoc );

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( RefCountPtr< const Lang::Value >( new Lang::HotRandomSeed( static_cast< size_t >( sz ), gen ) ),
			 evalState );
      }
    };

    class Core_destination : public Lang::CoreFunction
    {
    public:
      Core_destination( const char * title )
	: CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
      {
	formals_->appendEvaluatedCoreFormal( "remote", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "name", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "level", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "text", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "open", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "bold", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "italic", Kernel::THE_FALSE_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "color", Helpers::newValHandle( new Lang::RGB( Concrete::RGB( 0, 0, 0 ) ) ) );
	formals_->appendEvaluatedCoreFormal( "sides", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "target", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "fittobbox", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "zoom", Kernel::THE_VOID_VARIABLE );
	formals_->appendEvaluatedCoreFormal( "transform", Kernel::THE_TRUE_VARIABLE );
      }
      virtual void
      call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
      {
	args.applyDefaults( );

	size_t argsi = 0;
	const size_t remove_i = argsi;
	typedef const Lang::Boolean RemoteType;
	bool remote = Helpers::down_cast_CoreArgument< RemoteType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	typedef const Lang::String NameType;
	RefCountPtr< NameType > nameVal = Helpers::down_cast_CoreArgument< NameType >( title_, args, argsi, callLoc, true );
	RefCountPtr< const char > name = RefCountPtr< const char >( NullPtr< const char >( ) );
	if( nameVal != NullPtr< NameType >( ) )
	  {
	    const SimplePDF::PDF_out::Version STRINGDESTS_VERSION = SimplePDF::PDF_out::PDF_1_2;
	    if( Kernel::the_pdfo->versionGreaterOrEqual( STRINGDESTS_VERSION ) )
	      {
		name = nameVal->val_;
	      }
	    else
	      {
		Kernel::the_pdfo->versionMessage( STRINGDESTS_VERSION, "The naming of a destination was ignored." );
		// Note that this will leave name being null, and hence generate further errors if remote_.
	      }
	  }
	if( nameVal == NullPtr< NameType >( ) )  // Note why this is not an else clause!
	  {
	    if( remote )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, remove_i, "The destination cannot be remote if no name is given." );
	      }
	  }
  
	++argsi;
	const size_t outlineLevel_i = argsi;
	typedef const Lang::Integer OutlineLevelType;
	RefCountPtr< OutlineLevelType > levelVal = Helpers::down_cast_CoreArgument< OutlineLevelType >( title_, args, argsi, callLoc, true );
	int outlineLevel = -1; // This will remain negative only if the level is not present.
	if( levelVal != NullPtr< OutlineLevelType >( ) )
	  {
	    outlineLevel = levelVal->val_;
	    if( outlineLevel < 0 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, argsi, "The outline level must be non-negative." );	  
	      }
	  }

	++argsi;
	typedef const Lang::String OutlineTextType;
	RefCountPtr< OutlineTextType > textVal = Helpers::down_cast_CoreArgument< OutlineTextType >( title_, args, argsi, callLoc, true );
	RefCountPtr< const char > outlineText = RefCountPtr< const char >( NullPtr< const char >( ) );
	if( textVal != NullPtr< OutlineTextType >( ) )
	  {
	    outlineText = textVal->val_;
	  }
	else
	  {
	    if( outlineLevel >= 0 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, outlineLevel_i, "Without an outline text, it is not allowed to make an outline item." );
	      }
	  }

	++argsi;
	typedef const Lang::Boolean OutlineOpenType;
	bool outlineOpen = Helpers::down_cast_CoreArgument< OutlineOpenType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	typedef const Lang::Boolean OutlineBoldType;
	bool outlineBold = Helpers::down_cast_CoreArgument< OutlineBoldType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	typedef const Lang::Boolean OutlineItalicType;
	bool outlineItalic = Helpers::down_cast_CoreArgument< OutlineItalicType >( title_, args, argsi, callLoc )->val_;

	++argsi;
	typedef const Lang::RGB OutlineColorType;
	Concrete::RGB outlineColor = Helpers::down_cast_CoreArgument< OutlineColorType >( title_, args, argsi, callLoc )->components( );

	++argsi;
	const size_t sidesMode_i = argsi;
	typedef const Lang::Symbol SidesModeType;
	RefCountPtr< SidesModeType > sidesVal = Helpers::down_cast_CoreArgument< SidesModeType >( title_, args, argsi, callLoc, true );

	++argsi;
	const size_t target_i = argsi;
	typedef const Lang::Drawable2D TargetType;
	RefCountPtr< TargetType > target = Helpers::down_cast_CoreArgument< TargetType >( title_, args, argsi, callLoc, true );

	Lang::DocumentDestination::Sides sides = Lang::DocumentDestination::PAGE; // Defaults to false, unless a target is given.
	if( target != NullPtr< TargetType >( ) )
	  {
	    sides = Lang::DocumentDestination::TOPLEFT;
	    if( remote )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, target_i, "The target can not be specified for remote destinations." );
	      }
	  }
	static Lang::Symbol SIDES_TopLeft( "topleft" );
	static Lang::Symbol SIDES_Page( "page" );
	static Lang::Symbol SIDES_Top( "top" );
	static Lang::Symbol SIDES_Left( "left" );
	static Lang::Symbol SIDES_Rectangle( "rectangle" );
	if( sidesVal != NullPtr< SidesModeType >( ) )
	  {
	    if( *sidesVal == SIDES_TopLeft )
	      {
		sides = Lang::DocumentDestination::TOPLEFT;
	      }
	    else if( *sidesVal == SIDES_Page )
	      {
		if( target != NullPtr< TargetType >( ) )
		  {
		    throw Exceptions::CoreOutOfRange( title_, args, sidesMode_i, "The sides mode cannot be page when a target object is present." );
		  }
		sides = Lang::DocumentDestination::PAGE;
	      }
	    else if( *sidesVal == SIDES_Top )
	      {
		sides = Lang::DocumentDestination::TOP;
	      }
	    else if( *sidesVal == SIDES_Left )
	      {
		sides = Lang::DocumentDestination::LEFT;
	      }
	    else if( *sidesVal == SIDES_Rectangle )
	      {
		sides = Lang::DocumentDestination::RECTANGLE;
	      }
	    else
	      {
		std::ostringstream oss;
		oss << "Valid sides modes are the symbols { "
		    << SIDES_TopLeft.name( ).getPtr( ) << ", "
		    << SIDES_Page.name( ).getPtr( ) << ", "
		    << SIDES_Top.name( ).getPtr( ) << ", "
		    << SIDES_Left.name( ).getPtr( ) << ", "
		    << SIDES_Rectangle.name( ).getPtr( )
		    << " }." ;
		throw Exceptions::CoreOutOfRange( title_, args, sidesMode_i, strrefdup( oss ) );
	      }
	  }

	++argsi;
	typedef const Lang::Boolean FitToType;
	RefCountPtr< FitToType > fittobboxVal = Helpers::down_cast_CoreArgument< FitToType >( title_, args, argsi, callLoc, true );
	bool fittobbox = false;
	if( fittobboxVal != NullPtr< FitToType >( ) )
	  {
	    if( remote || sides == Lang::DocumentDestination::TOPLEFT || sides == Lang::DocumentDestination::RECTANGLE )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, argsi, "The fit-to-bbox flag cannot be specified in this mode." );
	      }
	    fittobbox = fittobboxVal->val_;
	  }

	++argsi;
	typedef const Lang::Float ZoomType;
	RefCountPtr< ZoomType > zoomVal = Helpers::down_cast_CoreArgument< ZoomType >( title_, args, argsi, callLoc, true );
	double zoom = 0; // This will remain zero only if the zoom argument is not specified.
	if( zoomVal != NullPtr< ZoomType >( ) )
	  {
	    if( remote || sides != Lang::DocumentDestination::TOPLEFT )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, argsi, "The zoom can only be specified when using the top-left sides." );	  	  
	      }
	    zoom = zoomVal->val_;
	    if( zoom <= 0 )
	      {
		throw Exceptions::CoreOutOfRange( title_, args, argsi, "The zoom value must be positive." );
	      }
	  }

	++argsi;
	typedef const Lang::Boolean DoTransformType;
	bool doTransform = Helpers::down_cast_CoreArgument< DoTransformType >( title_, args, argsi, callLoc )->val_;

	Kernel::ContRef cont = evalState->cont_;
	if( remote )
	  {
	    RefCountPtr< const Lang::DocumentDestination >
	      taggedObj( new Lang::DocumentDestination( remote, name, outlineLevel,
							outlineText, outlineOpen, outlineBold, outlineItalic, outlineColor ) );
	    cont->takeValue( RefCountPtr< const Lang::Value >
			     ( new Lang::TaggedValue2D( Kernel::THE_NAVIGATION_SYMBOL, taggedObj ) ),
			     evalState );
	  }
	else
	  {
	    RefCountPtr< const Lang::DocumentDestination >
	      taggedObj( new Lang::DocumentDestination( name, outlineLevel,
							outlineText, outlineOpen, outlineBold, outlineItalic, outlineColor,
							sides, target, fittobbox, zoom ) );
	    if( doTransform )
	      {
		cont->takeValue( RefCountPtr< const Lang::Value >
				 ( new Lang::TaggedGeometric2D( Kernel::THE_NAVIGATION_SYMBOL, taggedObj ) ),
				 evalState );
	      }
	    else
	      {
		cont->takeValue( RefCountPtr< const Lang::Value >
				 ( new Lang::TaggedValue2D( Kernel::THE_NAVIGATION_SYMBOL, taggedObj ) ),
				 evalState );
	      }
	  }
      }
    };
  }
}



void
Kernel::registerCore_construct( Kernel::Environment * env )
{
  env->initDefineCoreFunction( new Lang::Core_cons( "cons" ) );
  env->initDefineCoreFunction( new Lang::Core_list( "list" ) );
  env->initDefineCoreFunction( new Lang::Core_isnull( "isnull" ) );
  env->initDefineCoreFunction( new Lang::Core_range( "range" ) );
  env->initDefineCoreFunction( new Lang::Core_affinetransform( "affinetransform" ) );
  env->initDefineCoreFunction( new Lang::Core_shift( "shift" ) );
  env->initDefineCoreFunction( new Lang::Core_rotate( "rotate" ) );
  env->initDefineCoreFunction( new Lang::Core_rotate3d( "rotate3D" ) );
  env->initDefineCoreFunction( new Lang::Core_scale( "scale" ) );
  env->initDefineCoreFunction( new Lang::Core_scale3d( "scale3D" ) );
  env->initDefineCoreFunction( new Lang::Core_affinetransform3D( "affinetransform3D" ) );
  env->initDefineCoreFunction( new Lang::Core_inverse( "inverse" ) );

  env->initDefineCoreFunction( new Lang::Core_formxo( "formxo" ) );
  env->initDefineCoreFunction( new Lang::Core_transparencygroup( "tgroup" ) );

  env->initDefineCoreFunction( new Lang::Core_tag( "tag" ) );
  env->initDefineCoreFunction( new Lang::Core_find( "find" ) );
  env->initDefineCoreFunction( new Lang::Core_findall( "findall" ) );

  env->initDefineCoreFunction( new Lang::Core_ambient_light( "ambient_light" ) );
  env->initDefineCoreFunction( new Lang::Core_specular_light( "specular_light" ) );
  env->initDefineCoreFunction( new Lang::Core_distant_light( "distant_light" ) );
  env->initDefineCoreFunction( new Lang::Core_phong( "phong" ) );

  env->initDefineCoreFunction( new Lang::Core_vector( "vector" ) );
  env->initDefineCoreFunction( new Lang::Core_importPDFpages( "import" ) );

  env->initDefineCoreFunction( new Lang::Core_sprintf( "sprintf" ) );
  env->initDefineCoreFunction( new Lang::Core_strftime( "strftime" ) );

  env->initDefineCoreFunction( new Lang::Core_newrandom( "newRandom" ) );
  env->initDefineCoreFunction( new Lang::Core_devrandom( "devRandom" ) );

  env->initDefineCoreFunction( new Lang::Core_destination( "destination" ) );

  env->initDefineCoreFunction( new Lang::Core_textrenderingmode( "textmode" ) );
  env->initDefineCoreFunction( new Lang::Core_manualkern( "kerning" ) );
  env->initDefineCoreFunction( new Lang::Core_automatickern( "kern" ) );
}

