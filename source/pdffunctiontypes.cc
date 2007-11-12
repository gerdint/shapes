#include "Shapes_Helpers_decls.h"

#include "pdffunctiontypes.h"
#include "globals.h"
#include "shapescore.h"
#include "ast.h"

using namespace Shapes;


Lang::PDF_Function::PDF_Function( SubType subType, size_t outputDimension, size_t inputDimension, bool rangeIsActive )
  : Lang::Function( new Kernel::EvaluatedFormals( "< PDF-able function >", true ) ),
    subType_( subType ), outputDimension_( outputDimension ), inputDimension_( inputDimension ), rangeIsActive_( rangeIsActive ),
    storage( NullPtr< SimplePDF::PDF_Indirect_out >( ) )
{
  if( outputDimension_ == 0 )
    {
      throw Exceptions::OutOfRange( strrefdup( "The output dimension of a PDF function cannot be 0." ) );
    }
  switch( subType_ )
    {
    case INTERPOLATION:
      title_ = "< Interpolation function >";
      break;
    case EXPONENTIAL:
      title_ = "< Exponential interpolation function >";
      break;
    case STITCHING:
      title_ = "< Stiching function >";
      break;
    case CALCULATOR:
      title_ = "< Calculator function >";
      break;
    default:
      throw Exceptions::InternalError( "PDF_Function::PDF_Function: subType_ out of range in switch." );
    }
}

Lang::PDF_Function::~PDF_Function( )
{ }


void
Lang::PDF_Function::addCommonFields( SimplePDF::PDF_Dictionary * dst ) const
{
  (*dst)[ "FunctionType" ] = Kernel::the_pdfo->newInt( subType_ );
  
  if( domain_.size( ) != inputDimension_ )
    {
      throw Exceptions::InternalError( strrefdup( "The size of domain_ does not match inputDimension_." ) );
    }
  {
    RefCountPtr< SimplePDF::PDF_Vector > tmpVector;
    SimplePDF::PDF_Vector::VecType & dstRef( tmpVector->vec );
    dstRef.reserve( 2 * inputDimension_ );
    typedef typeof domain_ ListType;
    for( ListType::const_iterator i = domain_.begin( ); i != domain_.end( ); ++i )
      {
	dstRef.push_back( SimplePDF::PDF_out::newFloat( i->first ) );
	dstRef.push_back( SimplePDF::PDF_out::newFloat( i->second ) );
      }
    (*dst)[ "Domain" ] = tmpVector;
  }
  
  if( rangeIsActive_ )
    {
      if( range_.size( ) != inputDimension_ )
	{
	  throw Exceptions::InternalError( strrefdup( "The size of range_ does not match outputDimension_." ) );
	}
      {
	RefCountPtr< SimplePDF::PDF_Vector > tmpVector;
	SimplePDF::PDF_Vector::VecType & dstRef( tmpVector->vec );
	dstRef.reserve( 2 * outputDimension_ );
	typedef typeof range_ ListType;
	for( ListType::const_iterator i = range_.begin( ); i != range_.end( ); ++i )
	  {
	    dstRef.push_back( SimplePDF::PDF_out::newFloat( i->first ) );
	    dstRef.push_back( SimplePDF::PDF_out::newFloat( i->second ) );
	  }
	(*dst)[ "Range" ] = tmpVector;
      }
    }
  else
    {
      if( subType_ == INTERPOLATION || subType_ == CALCULATOR )
	{
	  throw Exceptions::InternalError( strrefdup( "Range must be active for this type of function." ) );
	}
    }
}


std::auto_ptr< const std::vector< double > >
Lang::PDF_Function::callImpl( const std::vector< double > & arg ) const
{
  throw Exceptions::NotImplemented( "Emulation of PDF functions (of this kind)." );
}

void
Lang::PDF_Function::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  std::vector< double > argVector;
  argVector.reserve( inputDimension_ );
  if( args.size( ) == 1 )
    {
      const Lang::Value * argPtr = args.getValue( 0 ).getPtr( );
      switch( inputDimension_ )
	{
	case 1:
	  {
	    typedef const Lang::Float ArgType;
	    ArgType * arg = dynamic_cast< ArgType * >( argPtr );
	    if( arg != 0 )
	      {
		argVector.push_back( arg->val_ );
		goto done;
	      }
	  }
	  break;
	case 2:
	  {
	    typedef const Lang::FloatPair ArgType;
	    ArgType * arg = dynamic_cast< ArgType * >( argPtr );
	    if( arg != 0 )
	      {
		argVector.push_back( arg->x_ );
		argVector.push_back( arg->y_ );
		goto done;
	      }
	  }
	  break;
	case 3:
	  {
	    typedef const Lang::FloatTriple ArgType;
	    ArgType * arg = dynamic_cast< ArgType * >( argPtr );
	    if( arg != 0 )
	      {
		argVector.push_back( arg->x_ );
		argVector.push_back( arg->y_ );
		argVector.push_back( arg->z_ );
		goto done;
	      }
	  }
	  break;
	}
      {
	typedef const Lang::VectorFunction ArgType;
	ArgType * arg = dynamic_cast< ArgType * >( argPtr );
	if( arg != 0 )
	  {
	    RefCountPtr< const std::vector< double > > argVec = arg->getNumeric( callLoc );
	    if( argVec->size( ) != inputDimension_ )
	      {
		throw Exceptions::CoreArityMismatch( title_, inputDimension_, argVec->size( ) );
	      }
	    for( size_t i = 0; i < inputDimension_; ++i )
	      {
		argVector.push_back( (*argVec)[ i ] );
	      }
	    goto done;
	  }
      }

      throw Exceptions::CoreTypeMismatch( callLoc, title_, args.getLoc( 0 ), args.getValue( 0 )->getTypeName( ), Helpers::typeSetString( Lang::Float::staticTypeName( ), Lang::FloatPair::staticTypeName( ), Lang::FloatTriple::staticTypeName( ), Lang::VectorFunction::staticTypeName( ) ) );

    }
  else if( args.size( ) == inputDimension_ )
    {
      for( size_t i = 0; i < inputDimension_; ++i )
	{
	  argVector.push_back( Helpers::down_cast_CoreArgument< const Lang::Float >( title_, args, i, callLoc )->val_ );
	}
    }
  else
    {
      throw Exceptions::CoreArityMismatch( title_, inputDimension_, args.size( ) );
    }
 done:

  std::auto_ptr< const std::vector< double > > resVector = this->callImpl( argVector );

  if( resVector->size( ) != outputDimension_ )
    {
      throw Exceptions::InternalError( strrefdup( "The PDF function call emulation returned with the wrong arity." ) );
    }
  RefCountPtr< const Lang::Value > res = NullPtr< const Lang::Value >( );
  switch( resVector->size( ) )
    {
    case 0:
      {
	throw Exceptions::InternalError( strrefdup( "PDF function with output arity 0 detected upon return. " ) );
      }
      break;
    case 1:
      {
	res = RefCountPtr< const Lang::Value >( new Lang::Float( (*resVector)[ 0 ] ) );
      }
      break;
    case 2:
      {
	res = RefCountPtr< const Lang::Value >( new Lang::FloatPair( (*resVector)[ 0 ], (*resVector)[ 1 ] ) );
      }
      break;
    case 3:
      {
	res = RefCountPtr< const Lang::Value >( new Lang::FloatTriple( (*resVector)[ 0 ], (*resVector)[ 1 ], (*resVector)[ 2 ] ) );
      }
      break;
    default:
      {
	std::vector< Kernel::ValueRef > * tmpRes = new typeof *tmpRes;
	tmpRes->reserve( resVector->size( ) );
	typedef typeof *resVector VectorType;
	for( VectorType::const_iterator i = resVector->begin( ); i != resVector->end( ); ++i )
	  {
	    tmpRes->push_back( RefCountPtr< const Lang::Value >( new Lang::Float( *i ) ) );
	  }
	res = RefCountPtr< const Lang::Value >( new Lang::VectorFunction( tmpRes ) );
      }
    }

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( res, evalState );
}

bool
Lang::PDF_Function::matchesDimensions( size_t outputDimension, size_t inputDimension ) const
{
  return
    outputDimension_ == outputDimension &&
    inputDimension_  == inputDimension;
}

RefCountPtr< SimplePDF::PDF_Indirect_out >
Lang::PDF_Function::getFunction( ) const
{
  return storage.unconst_cast< SimplePDF::PDF_Indirect_out >( );
}
 

Lang::PDF_InterpolationFunction::PDF_InterpolationFunction( const std::vector< std::pair< double, double > > & range,
							       const std::vector< std::pair< double, double > > & domain,
							       const std::vector< double > & size,
							       unsigned char bitsPerSample,
							       bool cubic,
							       const std::vector< std::pair< double, double > > & encode,
							       const std::vector< std::pair< double, double > > & decode,
							       const std::vector< double > table )
  : Lang::PDF_Function( Lang::PDF_Function::INTERPOLATION, range.size( ), domain.size( ), true )
{
  throw Exceptions::NotImplemented( "PDF_InterpolationFunction::PDF_InterpolationFunction" );
}

Lang::PDF_InterpolationFunction::~PDF_InterpolationFunction( )
{ }
