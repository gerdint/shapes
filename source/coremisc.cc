#include <cmath>

#include "metapdfcore.h"
#include "globals.h"
#include "metapdfexceptions.h"
#include "consts.h"
#include "simplepdfi.h"
#include "metapdfastfun.h"
#include "continuations.h"
#include "multipage.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <stdio.h>
#include <sys/time.h>

using namespace MetaPDF;


void
Lang::NullFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Lang::THE_VOID,
		   evalState );
}


void
Lang::Core_identity::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  CHECK_ARITY( args, ARITY, title_ );
  
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( args.getHandle( 0 ),
		    evalState );
}


void
Lang::Core_typeof::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  CHECK_ARITY( args, ARITY, title_ );
  
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( args.getValue( 0 )->getClass( ),
		   evalState );
}


void
Lang::Core_error::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 1;
  CHECK_ARITY( args, ARITY, title_ );
  
  throw Exceptions::UserError( Helpers::down_cast_CoreArgument< const Lang::String >( title_, args, 0, callLoc )->val_ );
}


Lang::Core_if::Core_if( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( "< core function if >" ) )
{
  formals_->appendEvaluatedCoreFormal( "predicate", Kernel::THE_SLOT_VARIABLE, true );
  formals_->appendEvaluatedCoreFormal( "consequence", Kernel::THE_SLOT_VARIABLE, false );
  formals_->appendEvaluatedCoreFormal( "alternative", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_VOID ) ), false );
}

void
Lang::Core_if::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  evalState->cont_ = Kernel::ContRef( new Kernel::IfContinuation( args.getHandle( 1 ), args.getHandle( 2 ), evalState->cont_, callLoc ) );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( args.getHandle( 0 ), evalState );
}

namespace MetaPDF
{
  namespace Kernel
  {

  class AndContinuation : public Kernel::Continuation
  {
    RefCountPtr< std::vector< Kernel::VariableHandle > > arguments_;
    std::vector< Kernel::VariableHandle >::const_iterator next_;
    Kernel::ContRef cont_;
  public:
    AndContinuation( const RefCountPtr< std::vector< Kernel::VariableHandle > > & arguments, const std::vector< Kernel::VariableHandle >::const_iterator & next, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
      : Kernel::Continuation( traceLoc ), arguments_( arguments ), next_( next ), cont_( cont )
    { }
    virtual ~AndContinuation( ) { }
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
    {
      if( ! Helpers::down_cast< const Lang::Boolean >( val, traceLoc_ )->val_ )
	{
	  cont_->takeValue( Lang::THE_FALSE,
			   evalState );
	  return;
	}
      
      std::vector< Kernel::VariableHandle >::const_iterator nextNext = next_;
      ++nextNext;
      if( nextNext == arguments_->end( ) )
	{
	  cont_->takeHandle( *next_, evalState );
	  return;	  
	}

      Kernel::ContRef newCont = Kernel::ContRef( new Kernel::AndContinuation( arguments_, nextNext, cont_, traceLoc_ ) );
      evalState->cont_ = newCont;
      newCont->takeHandle( *next_, evalState );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "and" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      typedef typeof next_ I;
      for( I i = next_; i != arguments_->end( ); ++i )
	{
	  const_cast< Kernel::Variable * >( i->getPtr( ) )->gcMark( marked );
	}
      cont_->gcMark( marked );
    }
  };

  class OrContinuation : public Kernel::Continuation
  {
    RefCountPtr< std::vector< Kernel::VariableHandle > > arguments_;
    std::vector< Kernel::VariableHandle >::const_iterator next_;
    Kernel::ContRef cont_;
  public:
    OrContinuation( const RefCountPtr< std::vector< Kernel::VariableHandle > > & arguments, const std::vector< Kernel::VariableHandle >::const_iterator & next, Kernel::ContRef cont, const Ast::SourceLocation & traceLoc )
      : Kernel::Continuation( traceLoc ), arguments_( arguments ), next_( next ), cont_( cont )
    { }
    virtual ~OrContinuation( ) { }
    virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const
    {
      if( Helpers::down_cast< const Lang::Boolean >( val, traceLoc_ )->val_ )
	{
	  cont_->takeValue( Lang::THE_TRUE,
			   evalState );
	  return;
	}
      
      std::vector< Kernel::VariableHandle >::const_iterator nextNext = next_;
      ++nextNext;
      if( nextNext == arguments_->end( ) )
	{
	  cont_->takeHandle( *next_, evalState );
	  return;	  
	}

      Kernel::ContRef newCont = Kernel::ContRef( new Kernel::OrContinuation( arguments_, nextNext, cont_, traceLoc_ ) );
      evalState->cont_ = newCont;
      newCont->takeHandle( *next_, evalState );
    }
    virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const
    {
      trace->push_front( Kernel::Continuation::BackTraceElem( this, "or" ) );
      cont_->backTrace( trace );
    }
    virtual void gcMark( Kernel::GCMarkedSet & marked )
    {
      typedef typeof next_ I;
      for( I i = next_; i != arguments_->end( ); ++i )
	{
	  const_cast< Kernel::Variable * >( i->getPtr( ) )->gcMark( marked );
	}
      cont_->gcMark( marked );
    }
  };

  }
}

Lang::Core_and::Core_and( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, false ) )
{ }

void
Lang::Core_and::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  if( args.size( ) == 0 )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( THE_TRUE, evalState );
      return;
    }

  if( args.size( ) == 1 )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeHandle( args.getHandle( 0 ), evalState );
      return;
    }

  typedef typeof *(args.getVariables( )) ListType;
  ListType::const_iterator next = args.getVariables( )->begin( );
  ++next;
  evalState->cont_ = Kernel::ContRef( new Kernel::AndContinuation( args.getVariables( ), next, evalState->cont_, callLoc ) );
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( args.getHandle( 0 ), evalState );
}

Lang::Core_or::Core_or( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, false ) )
{ }

void
Lang::Core_or::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  if( args.size( ) == 0 )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( Lang::THE_FALSE, evalState );
      return;
    }

  if( args.size( ) == 1 )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeHandle( args.getHandle( 0 ),
			evalState );
      return;
    }

  typedef typeof *(args.getVariables( )) ListType;
  ListType::const_iterator next = args.getVariables( )->begin( );
  ++next;
  evalState->cont_ = Kernel::ContRef( new Kernel::OrContinuation( args.getVariables( ), next, evalState->cont_, callLoc ) );
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( args.getHandle( 0 ), evalState );
}

void
Lang::Core_show::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  for( size_t i = 0; i != args.size( ); ++i )
    {
      std::cerr << args.getValue( i )->getTypeName( ) << ": " ;
      args.getValue( i )->show( std::cerr );
      std::cerr << std::endl ;
    }
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
		    evalState );
}


void
Lang::Core_memoryinfo::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 0;
  CHECK_ARITY( args, ARITY, title_ );
  std::cerr << "Environments:  alive: " << Kernel::Environment::liveCount << "  of total: " << Kernel::Environment::createdCount
	    << "  (" << 100 * static_cast< double >( Kernel::Environment::liveCount ) / static_cast< double >( Kernel::Environment::createdCount ) << "%)" << std::endl ;
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
		    evalState );
}  


void
Lang::Core_rectangle::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  const size_t ARITY = 2;
  CHECK_ARITY( args, ARITY, title_ );
  
  typedef typeof args ListType;

  typedef const Lang::Coords2D ArgType;

  RefCountPtr< ArgType > arg1 = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 0, callLoc );
  RefCountPtr< ArgType > arg2 = Helpers::down_cast_CoreArgument< ArgType >( title_, args, 1, callLoc );

  Lang::ElementaryPath2D * res = new Lang::ElementaryPath2D;

  res->push_back( new Concrete::PathPoint2D( arg1->x_.get( ), arg1->y_.get( ) ) );
  res->push_back( new Concrete::PathPoint2D( arg2->x_.get( ), arg1->y_.get( ) ) );
  res->push_back( new Concrete::PathPoint2D( arg2->x_.get( ), arg2->y_.get( ) ) );
  res->push_back( new Concrete::PathPoint2D( arg1->x_.get( ), arg2->y_.get( ) ) );
  res->close( );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( res ),
		   evalState );
}


Lang::Core_hot::Core_hot( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( "< core function hot >", true ) )
{
  formals_->appendEvaluatedCoreFormal( "init", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "tackon", Kernel::THE_SLOT_VARIABLE );
  formals_->appendEvaluatedCoreFormal( "freeze", Kernel::VariableHandle( new Kernel::Variable( Lang::THE_IDENTITY ) ) );
}

void
Lang::Core_hot::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::HotTriple
				      ( args.getValue( 0 ),
					Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 1, callLoc ),
					Helpers::down_cast_CoreArgument< const Lang::Function >( title_, args, 2, callLoc ) ) ),
		   evalState );
}


Lang::Core_nextpagenumber::Core_nextpagenumber( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendCoreStateFormal( "catalog" );
}

void
Lang::Core_nextpagenumber::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  typedef Kernel::WarmCatalog StateType;
  StateType * state = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::Integer( state->getNextPageNumber( ) ) ),
		   evalState );
}


Lang::Core_nextpagelabel::Core_nextpagelabel( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendCoreStateFormal( "catalog" );
}

void
Lang::Core_nextpagelabel::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  typedef Kernel::WarmCatalog StateType;
  StateType * state = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( Kernel::ValueRef( new Lang::String( state->getNextPageLabel( ) ) ),
		   evalState );
}


Lang::Core_setpagelabel::Core_setpagelabel( const char * title )
  : CoreFunction( title, new Kernel::EvaluatedFormals( title, true ) )
{
  formals_->appendCoreStateFormal( "catalog" );

  formals_->appendEvaluatedCoreFormal( "prefix", Helpers::newValHandle( new Lang::String( strrefdup( "" ) ) ) );
  formals_->appendEvaluatedCoreFormal( "style", Helpers::newValHandle( new Lang::Symbol( "decimal" ) ) );
  formals_->appendEvaluatedCoreFormal( "number", Helpers::newValHandle( new Lang::Integer( 1 ) ) );
}

void
Lang::Core_setpagelabel::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  args.applyDefaults( );

  typedef Kernel::WarmCatalog StateType;
  StateType * state = Helpers::down_cast_CoreState< StateType >( title_, args, 0, callLoc );

  size_t argsi = 0;
  typedef const Lang::String PrefixType;
  RefCountPtr< PrefixType > prefix = Helpers::down_cast_CoreArgument< PrefixType >( title_, args, argsi, callLoc );
  
  ++argsi;
  typedef const Lang::Symbol StyleType;
  RefCountPtr< StyleType > style = Helpers::down_cast_CoreArgument< StyleType >( title_, args, 1, callLoc );

  static Lang::Symbol STYLE_none( "none" );
  static Lang::Symbol STYLE_decimal( "decimal" );
  static Lang::Symbol STYLE_ROMAN( "ROMAN" );
  static Lang::Symbol STYLE_roman( "roman" );
  static Lang::Symbol STYLE_ALPHABET( "ALPHABET" );
  static Lang::Symbol STYLE_alphabet( "alphabet" );

  Kernel::WarmCatalog::PageLabelEntry::Style styleVal;
  if( *style == STYLE_none )
    {
      styleVal = Kernel::WarmCatalog::PageLabelEntry::NONE;
    }
  else if( *style == STYLE_decimal )
    {
      styleVal = Kernel::WarmCatalog::PageLabelEntry::DECIMAL;
    }
  else if( *style == STYLE_ROMAN )
    {
      styleVal = Kernel::WarmCatalog::PageLabelEntry::ROMAN;
    }
  else if( *style == STYLE_roman )
    {
      styleVal = Kernel::WarmCatalog::PageLabelEntry::rOMAN;
    }
  else if( *style == STYLE_ALPHABET )
    {
      styleVal = Kernel::WarmCatalog::PageLabelEntry::ALPHABET;
    }
  else if( *style == STYLE_alphabet )
    {
      styleVal = Kernel::WarmCatalog::PageLabelEntry::aLPHABET;
    }
  else
    {
      std::ostringstream oss;
      oss << "Valid page label styles are the symbols { "
	  << STYLE_none.name( ).getPtr( ) << ", "
	  << STYLE_decimal.name( ).getPtr( ) << ", "
	  << STYLE_ROMAN.name( ).getPtr( ) << ", "
	  << STYLE_roman.name( ).getPtr( ) << ", "
	  << STYLE_alphabet.name( ).getPtr( ) << ", "
	  << STYLE_ALPHABET.name( ).getPtr( )
	  << " }." ;
      throw Exceptions::CoreOutOfRange( title_, args, argsi, strrefdup( oss ) );
    }

  ++argsi;
  typedef const Lang::Integer NumberType;
  RefCountPtr< NumberType > number = Helpers::down_cast_CoreArgument< NumberType >( title_, args, argsi, callLoc );
  if( number->val_ < 1 )
    {
      throw Exceptions::CoreOutOfRange( title_, args, argsi, "PDF only allows strictly positive page numbers." );
    }
  
  state->setLabel( prefix->val_, styleVal, static_cast< size_t >( number->val_ ) );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( Kernel::THE_SLOT_VARIABLE,
		    evalState );
}
