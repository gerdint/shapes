#include "metapdfast.h"
#include "globals.h"

using namespace MetaPDF;
using namespace std;


void
Kernel::Continuation::takeHandle( Kernel::HandleType val, Kernel::EvalState * evalState, bool callingMyself ) const
{
  if( val->isThunk( ) )
    {
      val->force( val, evalState );
    }
  else if( callingMyself )
    {
      throw Exceptions::InternalError( strrefdup( "Continuation is just calling itself..." ) );
    }
  else
    {
      this->takeValue( val->getUntyped( ), evalState, true );
    }
}

void
Kernel::Continuation::takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool callingMyself ) const
{
  if( callingMyself )
    {
      throw Exceptions::InternalError( strrefdup( "Continuation is just calling itself..." ) );
    }
  this->takeHandle( Kernel::HandleType( new Kernel::Variable( val ) ), evalState, true );
}

const Ast::SourceLocation &
Kernel::Continuation::traceLoc( ) const
{
  return traceLoc_;
}

Kernel::Continuation::BackTraceElem::BackTraceElem( const Kernel::Continuation * cont, const char * msg )
  : mem_( NullPtr< const char >( ) ), cont_( cont ), msg_( msg )
{ }

Kernel::Continuation::BackTraceElem::BackTraceElem( const Kernel::Continuation * cont, const RefCountPtr< const char > msg )
  : mem_( msg ), cont_( cont ), msg_( msg.getPtr( ) )
{ }

Kernel::Continuation::BackTraceElem::~BackTraceElem( )
{ }

std::ostream &
Kernel::operator << ( std::ostream & os, const Kernel::Continuation::BackTraceElem & elem )
{
  os << elem.cont_->traceLoc( ) << "  " << elem.msg_ ;
  return os;
}


void
Kernel::Continuation::backTrace( std::ostream & os )
{
  typedef std::list< Kernel::Continuation::BackTraceElem > ListType;
  ListType trace;
  backTrace( & trace );
  ListType::const_iterator i = trace.begin( );
  ListType::const_iterator next = i;
  ++next;
  for( ; i != trace.end( ); ++i, ++next )
    {
      if( next == trace.end( ) ||
	  not i->cont_->traceLoc( ).contains( next->cont_->traceLoc( ) ) )
	{
	  os << "  " << *i << std::endl ;
	}
    }
}


Ast::Node::Node( const Ast::SourceLocation & loc )
  : parent_( 0 ), loc_( loc )
{ }

Ast::Node::~Node( )
{ }

void
Ast::Node::setParent( Ast::Node * parent )
{
  parent_ = parent;
}

Ast::Expression::Expression( const Ast::SourceLocation & loc )
  : Ast::Node( loc ), immediate_( false )
{ }

Ast::Expression::~Expression( )
{ }


const Ast::SourceLocation &
Ast::Node::loc( ) const
{
  return loc_;
}


Ast::BindNode::BindNode( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id )
  : Ast::Node( loc ), idLoc_( idLoc ), id_( id )
{ }

Ast::BindNode::~BindNode( )
{
  if( id_ != 0 )
    {
      delete id_;
    }
}

const char *
Ast::BindNode::id( ) const
{
  return id_;
}

const Ast::SourceLocation &
Ast::BindNode::idLoc( ) const
{
  return idLoc_;
}


Ast::SequencingNode::SequencingNode( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id )
  : Ast::Node( loc ), idLoc_( idLoc ), id_( id )
{ }

Ast::SequencingNode::~SequencingNode( )
{
  delete id_;
}

const char *
Ast::SequencingNode::id( ) const
{
  return id_;
}

const Ast::SourceLocation &
Ast::SequencingNode::idLoc( ) const
{
  return idLoc_;
}



Ast::IdentifierNode::IdentifierNode( )
{ }

Ast::IdentifierNode::~IdentifierNode( )
{ }
