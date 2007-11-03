#include "MetaPDF_Helpers_decls.h"

#include "astclass.h"
#include "metapdfexceptions.h"
#include "consts.h"
#include "globals.h"
#include "astvar.h"
#include "metapdfcore.h"

#include <string>

using namespace MetaPDF;


Ast::MethodIdExpr::MethodIdExpr( const Ast::SourceLocation & loc, Ast::Expression * classPart, const char * name )
  : loc_( loc ), classPart_( classPart ), name_( name )
{ }

Ast::MethodIdExpr::~MethodIdExpr( )
{ }

// Kernel::MethodId
// Ast::MethodIdExpr::identifier( Kernel::Environment::VariableHandle dstgroup, SimplePDF::PDF_out * pdfo, Kernel::GraphicsState * metaState, Kernel::PassedEnv env ) const
// {
//   RefCountPtr< const Lang::Value > untypedClass = classPart->value( dstgroup, pdfo, metaState, env );
//   typedef const Lang::Class ArgType;
//   RefCountPtr< ArgType > typedClass = untypedClass.down_cast< ArgType >( );
//   if( typedClass == NullPtr< ArgType >( ) )
//     {
//       throw Exceptions::TypeMismatch( classPart, untypedClass->getTypeName( ), ArgType::staticTypeName( ) );
//     }
//   return Kernel::MethodId( typedClass, id->identifier( dstgroup, pdfo, metaState, env ) );
// }



Ast::MemberDeclaration::MemberDeclaration( const Ast::SourceLocation & loc, const char * id, Ast::Expression * init, const Ast::MemberMode & mode )
  : loc_( loc ), id_( id ), init_( init ), mode_( mode )
{
  checkModeConsistency( );
}

Ast::MemberDeclaration::~MemberDeclaration( )
{
  delete init_;
}

void
Ast::MemberDeclaration::addModeBits( const Ast::MemberMode & bits )
{
  mode_ |= bits;
  checkModeConsistency( );
}

void
Ast::MemberDeclaration::checkModeConsistency( )
{
  if( ( mode_ & Ast::MEMBER_CONST ) != 0 && ( mode_ & ( Ast::MEMBER_ACCESS_PUBLIC_INSERT | Ast::MEMBER_ACCESS_PROTECTED_INSERT ) ) != 0 )
    {
      throw Exceptions::ParserError( loc_, strrefdup( "The member cannot be declared both const and assignable." ) );
    }
}


Ast::ClassSection::ClassSection( )
{ }

Ast::ClassSection::~ClassSection( )
{ }


Ast::MemberSection::MemberSection( )
{ }

Ast::MemberSection::~MemberSection( )
{
  /* We must not delete the declarations here.  This class is only used to pass these declarations on to a ClassExpression, which will be responsible for deletion.
   */
}

void
Ast::MemberSection::addModeBits( const Ast::MemberMode & bits )
{
  for( iterator i = begin( ); i != end( ); ++i )
    {
      (*i)->addModeBits( bits );
    }
}


Ast::PrepareSection::PrepareSection( const Ast::SourceLocation & loc, std::list< Ast::Node * > * nodes )
  : loc_( loc ), nodes_( nodes )
{ }

Ast::PrepareSection::~PrepareSection( )
{
  /* We must not delete the expressions here.  This class is only used to pass these expressions on to a ClassExpression, which will be responsible for deletion.
   * However, the list shall be deleted.
   */
  delete nodes_;
}


Ast::AbstractSection::AbstractSection( const Ast::SourceLocation & loc, const std::list< RefCountPtr< const char > > * methods )
  : loc_( loc ), methods_( methods )
{ }

Ast::AbstractSection::~AbstractSection( )
{
  delete methods_;
}


Ast::OverridesSection::OverridesSection( Ast::Expression * super, Ast::MemberSection * members )
  : super_( super ), members_( members )
{ }

Ast::OverridesSection::~OverridesSection( )
{
  delete super_;
  delete members_;
}


Ast::ClassFunction::ClassFunction( const Ast::SourceLocation & loc, Ast::Expression * name, const Kernel::Formals * constructorFormals, std::list< const Ast::CallExpr * > * parentsWithInitArgs, Ast::MemberMode classMode, std::list< Ast::ClassSection * > * sections )
  : Lang::Function( new Kernel::EvaluatedFormals( "< class construction >", true ) ), loc_( loc ), name_( name ), constructorFormals_( constructorFormals ), parentsWithInitArgs_( parentsWithInitArgs ), isAbstract_( ( classMode & Ast::CLASS_MODE_ABSTRACT ) != 0 ), isFinal_( ( classMode & Ast::CLASS_MODE_FINAL ) != 0 )
{
  for( std::list< Ast::ClassSection * >::iterator i = sections->begin( ); i != sections->end( ); ++i )
    {
      {
	typedef Ast::MemberSection T;
	T * memberSection = dynamic_cast< T * >( *i );
	if( memberSection != 0 )
	  {
	    for( std::list< Ast::MemberDeclaration * >::iterator j = memberSection->begin( );
		 j != memberSection->end( );
		 ++j )
	      {
		members_.push_back( *j );
	      }
	    delete *i;
	    continue;
	  }
      }
      {
	typedef Ast::PrepareSection T;
	T * prepareSection = dynamic_cast< T * >( *i );
	if( prepareSection != 0 )
	  {
	    for( std::list< Ast::Node * >::iterator j = prepareSection->nodes_->begin( );
		 j != prepareSection->nodes_->end( );
		 ++j )
	      {
		preparations_.push_back( *j );
	      }
	    delete *i;
	    continue;
	  }
      }
      {
	typedef Ast::OverridesSection T;
	T * overridesSection = dynamic_cast< T * >( *i );
	if( overridesSection != 0 )
	  {
	    overrides_.push_back( overridesSection );
	    continue;
	  }
      }
      {
	typedef Ast::AbstractSection T;
	T * abstractSection = dynamic_cast< T * >( *i );
	if( abstractSection != 0 )
	  {
	    for( std::list< RefCountPtr< const char > >::const_iterator j = abstractSection->methods_->begin( );
		 j != abstractSection->methods_->end( );
		 ++j )
	      {
		abstractSet_.insert( strdup( j->getPtr( ) ) );
	      }
	    continue;
	  }
      }
      throw Exceptions::InternalError( strrefdup( "ClassFunction::ClassFunction: the ClassSection was neither of the expected types" ) );
    }
  delete sections;

  {
    typedef std::map< const char *, Ast::MemberDeclaration *, charPtrLess > MapType;
    MapType seen;
    for( std::list< Ast::MemberDeclaration * >::const_iterator i = members_.begin( ); i != members_.end( ); ++i )
      {

	if( abstractSet_.find( (*i)->id_ ) != abstractSet_.end( ) )
	  {
	    throw Exceptions::MemberAlsoAbstract( (*i)->loc_, (*i)->id_, Ast::SourceLocation( ) );
	  }
	MapType::const_iterator j = seen.find( (*i)->id_ );
	if( j != seen.end( ) )
	  {
	    throw Exceptions::MemberAlreadyDeclared( (*i)->loc_, (*i)->id_, j->second->loc_ );
	  }
	seen[ (*i)->id_ ] = *i;
	
	if( ( (*i)->mode_ & Ast::MEMBER_ACCESS_PUBLIC_GET ) != 0 )
	  {
	    if( ( (*i)->mode_ & Ast::MEMBER_METHOD ) == 0 && ! isFinal_ )
	      {
		throw Exceptions::PublicGetSetInNonfinalClass( (*i)->loc_, (*i)->id_ );
	      }
	    publicGetSet_.insert( (*i)->id_ );
	  }
	if( ( (*i)->mode_ & Ast::MEMBER_ACCESS_PROTECTED_GET ) != 0 )
	  {
	    protectedGetSet_.insert( (*i)->id_ );
	  }
	if( ( (*i)->mode_ & Ast::MEMBER_ACCESS_PUBLIC_INSERT ) != 0 )
	  {
	    if( ( (*i)->mode_ & Ast::MEMBER_METHOD ) == 0 && ! isFinal_ )
	      {
		throw Exceptions::PublicGetSetInNonfinalClass( (*i)->loc_, (*i)->id_ );
	      }
	    publicSetSet_.insert( (*i)->id_ );
	  }
	if( ( (*i)->mode_ & Ast::MEMBER_ACCESS_PROTECTED_INSERT ) != 0 )
	  {
	    protectedSetSet_.insert( (*i)->id_ );
	  }
	if( ( (*i)->mode_ & Ast::MEMBER_FINAL ) != 0 )
	  {
	    finalSet_.insert( (*i)->id_ );
	  }
	if( ( (*i)->mode_ & Ast::MEMBER_TRANSFORMING ) != 0 )
	  {
	    if( ( (*i)->mode_ & Ast::MEMBER_METHOD ) == 0 && ! isFinal_ )
	      {
		throw Exceptions::TransformingMemberInNonfinalClass( (*i)->loc_, (*i)->id_ );
	      }
	    transformingSet_.insert( (*i)->id_ );	    
	  }
      }
  }
}

Ast::ClassFunction::~ClassFunction( )
{
  delete name_;

  delete constructorFormals_;
  
  for( std::list< const Ast::CallExpr * >::iterator i = parentsWithInitArgs_->begin( );
       i != parentsWithInitArgs_->end( );
       ++i )
    {
      delete *i;
    }
  delete parentsWithInitArgs_;

  /* preparations_ and members_ delete their elements by themselves
   */
}


void
Ast::ClassFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( name_ );

  constructorFormals_->push_exprs( args );

  typedef std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > > ParentClassList;
  RefCountPtr< ParentClassList > parents = RefCountPtr< ParentClassList >( new ParentClassList );
  for( std::list< const Ast::CallExpr * >::iterator i = parentsWithInitArgs_->begin( );
       i != parentsWithInitArgs_->end( );
       ++i )
    {
      args->orderedExprs_->push_back( (*i)->funExpr_ );
    }

  {
    typedef std::map< RefCountPtr< const Lang::Class >, std::map< const char *, Ast::MemberDeclaration *, charPtrLess > > MapType;
    MapType seen;

    typedef typeof overrides_ ListType;
    for( ListType::const_iterator i = overrides_.begin( ); i != overrides_.end( ); ++i )
      {
	args->orderedExprs_->push_back( (*i)->super_ );
      }
  }
  
}

void
Ast::ClassFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  size_t pos = 0;
  RefCountPtr< const Lang::String > typedName = args.getHandle( pos )->getVal< const Lang::String >( name_->loc( ) );
  ++pos;

  Kernel::EvaluatedFormals * evaluatedFormals = constructorFormals_->newEvaluatedFormals( args, & pos );

  typedef std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > > ParentClassList;
  RefCountPtr< ParentClassList > parents = RefCountPtr< ParentClassList >( new ParentClassList );
  for( std::list< const Ast::CallExpr * >::iterator i = parentsWithInitArgs_->begin( );
       i != parentsWithInitArgs_->end( );
       ++i )
    {
      RefCountPtr< const Lang::Class > typedParent = args.getHandle( pos )->getVal< const Lang::Class >( (*i)->funExpr_->loc( ) );
      ++pos;
      parents->push_back( std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * >( typedParent, (*i)->argList_ ) ); 
    }

  std::map< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > > * overridesMap = new std::map< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > >;
  {
    typedef std::map< RefCountPtr< const Lang::Class >, std::map< const char *, Ast::MemberDeclaration *, charPtrLess > > MapType;
    MapType seen;

    typedef typeof overrides_ ListType;
    for( ListType::const_iterator i = overrides_.begin( ); i != overrides_.end( ); ++i )
      {
	RefCountPtr< const Lang::Class > typedParent = args.getHandle( pos )->getVal< const Lang::Class >( (*i)->super_->loc( ) );
	++pos;
	std::map< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > >::iterator j = overridesMap->find( typedParent );
	if( j == overridesMap->end( ) )
	  {
	    j = overridesMap->insert( j, std::pair< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > >( typedParent, std::list< Ast::MemberDeclaration * >( ) ) );
	  }
	std::map< const char *, Ast::MemberDeclaration *, charPtrLess > & currentClassSeen = seen[ typedParent ];
	for( std::list< Ast::MemberDeclaration * >::const_iterator k = (*i)->members_->begin( ); k != (*i)->members_->end( ); ++k )
	  {
	    std::map< const char *, Ast::MemberDeclaration *, charPtrLess >::const_iterator l = currentClassSeen.find( (*k)->id_ );
	    if( l != currentClassSeen.end( ) )
	      {
		throw Exceptions::MemberAlreadyDeclared( (*k)->loc_, (*k)->id_, l->second->loc_ );
	      }
	    currentClassSeen[ (*k)->id_ ] = *k;

	    j->second.push_back( *k );
	  }
      }
  }

  Lang::UserClass * res = new Lang::UserClass( this,
					       evalState->env_,
					       typedName->val_,
					       evaluatedFormals,
					       parents,
					       RefCountPtr< std::map< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > > >( overridesMap ),
					       isFinal_ );
  RefCountPtr< const Lang::Class > resRef = RefCountPtr< const Lang::Class >( res );
  res->setSelfRef( resRef );
  res->setupAndCheck( isAbstract_ );

  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( resRef,
		   evalState );
}

bool
Ast::ClassFunction::isInPublicGetSet( const char * field ) const
{
  return publicGetSet_.find( field ) != publicGetSet_.end( );
}

bool
Ast::ClassFunction::isInPublicSetSet( const char * field ) const
{
  return publicSetSet_.find( field ) != publicSetSet_.end( );
}

bool
Ast::ClassFunction::isInProtectedGetSet( const char * field ) const
{
  return protectedGetSet_.find( field ) != protectedGetSet_.end( );
}

bool
Ast::ClassFunction::isInProtectedSetSet( const char * field ) const
{
  return protectedSetSet_.find( field ) != protectedSetSet_.end( );
}

bool
Ast::ClassFunction::isInAbstractSet( const char * field ) const
{
  return abstractSet_.find( field ) != abstractSet_.end( );
}

bool
Ast::ClassFunction::isInFinalSet( const char * field ) const
{
  return finalSet_.find( field ) != finalSet_.end( );
}

bool
Ast::ClassFunction::isInTransformingSet( const char * field ) const
{
  return transformingSet_.find( field ) != transformingSet_.end( );
}

Lang::Class::MessageMapType
Ast::ClassFunction::getLocalMessageMap( RefCountPtr< const Lang::Class > _myClass ) const
{
  Lang::Class::MessageMapType res;
  {
    typedef typeof abstractSet_ ListType;
    for( ListType::const_iterator i = abstractSet_.begin( ); i != abstractSet_.end( ); ++i )
      {
	res[ Kernel::MethodId( _myClass, *i ) ];
      }
  }
  {
    typedef typeof members_ ListType;
    for( ListType::const_iterator i = members_.begin( ); i != members_.end( ); ++i )
      {
	if( ( (*i)->mode_ & Ast::MEMBER_METHOD ) == 0 )
	  {
	    continue;
	  }
	std::set< RefCountPtr< const Lang::Class > > & theSet = res[ Kernel::MethodId( _myClass, (*i)->id_ ) ];
	theSet.insert( theSet.begin( ), _myClass );
      }
  }
  return res;
}

bool
Ast::ClassFunction::isRepeatableBase( ) const
{
  return constructorFormals_->argumentOrder_->size( ) == 0;
}

#if 0
void
Ast::ClassFunction::bindInitializationArguments( RefCountPtr< const Lang::Class > theClass, Kernel::PassedEnv initEnv, Kernel::Arguments & args ) const
{
  if( args.size( ) != constructorFormals_->size( ) )
    {
      throw Exceptions::UserArityMismatch( this, constructorFormals_->size( ), args.size( ), Exceptions::UserArityMismatch::VARIABLE );
    }

  std::list< Kernel::ValueRef >::const_iterator val = args.begin( );
  std::list< Kernel::ValueRef >::const_iterator end = args.end( );
  std::list< RefCountPtr< const char > >::const_iterator formal = constructorFormals_->begin( );
  Ast::SourceLocation dummy;
  for( ; val != end; ++val, ++formal )
    {
      initEnv->define( dummy, *formal, *val, true );  /* true means constant; the initialization parameters shall not be confused with object states */
    }
}
#endif

void
Ast::ClassFunction::setupInstance( Kernel::PassedEnv instanceEnv, Kernel::PassedEnv privateEnv, Kernel::EvalState * evalState, Kernel::PassedEnv initEnv ) const
{
  throw Exceptions::NotImplemented( "ClassFunction::setupInstance" );

//   typedef typeof members_ ListType;
//   for( ListType::const_iterator i = members_.begin( ); i != members_.end( ); ++i )
//     {
//       const Ast::MemberDeclaration & decl = **i;

//       Kernel::PassedEnv * defineEnv = & instanceEnv;
//       if( ( decl.mode & Ast::MEMBER_ACCESS_BITS ) == Ast::MEMBER_ACCESS_PRIVATE )
// 	{
// 	  defineEnv = & privateEnv;
// 	}

//       Kernel::PassedEnv & evalEnv = initEnv;
//       if( ( decl.mode & Ast::MEMBER_METHOD ) != 0 )
// 	{
// 	  evalEnv = privateEnv;
// 	}      

//       (*defineEnv)->define( decl.loc, decl.id, decl.init->value( dstgroup, pdfo, metaState, evalEnv ), ( decl.mode & ( Ast::MEMBER_METHOD | Ast::MEMBER_CONST ) ) != 0 );
//     }
}

void
Ast::ClassFunction::prepareInstance( Kernel::EvalState * evalState, Kernel::PassedEnv privateEnv ) const
{
  throw Exceptions::NotImplemented( "ClassFunction::prepareInstance" );
  
  //  Ast::CodeBracket::evalSequence( preparations.begin( ), preparations.end( ), metaState, privateEnv, false );
}

const Ast::SourceLocation &
Ast::ClassFunction::loc( ) const
{
  return loc_;
}


Ast::PublicMethodReferenceFunction::PublicMethodReferenceFunction( const Ast::SourceLocation & loc, Ast::Expression * obj, MethodIdExpr * methodId )
  : Lang::Function( 0 ), loc_( loc ), obj_( obj ), methodClass_( methodId->classPart_ ), methodName_( strdup( methodId->name_ ) )
{
  std::cerr << "Warning: Ast::PublicMethodReferenceFunction::PublicMethodReferenceFunction initializes Lang::Function with 0 pointer to formals." << std::endl ;
  delete methodId;
}

Ast::PublicMethodReferenceFunction::~PublicMethodReferenceFunction( )
{
  delete obj_;
  delete methodClass_;
}

void
Ast::PublicMethodReferenceFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( obj_ );
  args->orderedExprs_->push_back( methodClass_ );
}

void
Ast::PublicMethodReferenceFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  RefCountPtr< const Lang::Value > untypedObj = args.getValue( 0 );
  Kernel::MethodId methodId( args.getHandle( 1 )->getVal< const Lang::Class >( methodClass_->loc( ) ), methodName_ );
  {
    typedef const Lang::Instance ObjType;
    ObjType * typedObj = dynamic_cast< ObjType * >( untypedObj.getPtr( ) );
    if( typedObj != 0 )
      {
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( typedObj->getMethod( methodId ),
			 evalState );
	return;
      }
  }
  {
    typedef const Lang::TransformedInstance ObjType;
    ObjType * typedObj = dynamic_cast< ObjType * >( untypedObj.getPtr( ) );
    if( typedObj != 0 )
      {
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( typedObj->getMethod( methodId ),
			 evalState );
	return;
      }
  }
  throw Exceptions::TypeMismatch( obj_->loc( ), untypedObj->getTypeName( ), Helpers::typeSetString( Lang::Instance::staticTypeName( ), Lang::TransformedInstance::staticTypeName( ) ) );
}



Ast::ProtectedMethodReferenceFunction::ProtectedMethodReferenceFunction( const Ast::SourceLocation & loc, const Ast::SourceLocation & selfLoc, Ast::Expression * parent, Ast::MethodIdExpr * methodId )
  : Lang::Function( 0 ), loc_( loc ), selfLoc_( selfLoc ), parent_( parent ), methodClass_( methodId->classPart_ ), methodName_( strdup( methodId->name_ ) ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{
  std::cerr << "Warning: Ast::PublicMethodReferenceFunction::PublicMethodReferenceFunction initializes Lang::Function with 0 pointer to formals." << std::endl ;
  /* Currently, a runtime check is used to ensure that this expression indeed refers to a super instance. 
   */
  delete methodId;
}

Ast::ProtectedMethodReferenceFunction::~ProtectedMethodReferenceFunction( )
{
  delete parent_;
  delete methodClass_;
  delete methodName_;
  if( *idKey_ != 0 )
    {
      delete *idKey_;
    }
  delete idKey_;     //  This can be done only as long as this is not shared!
}

void
Ast::ProtectedMethodReferenceFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( methodClass_ );
  if( parent_ != 0 )
    {
      args->orderedExprs_->push_back( parent_ );
    }
}

void
Ast::ProtectedMethodReferenceFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  /* argument expressions shall be analyzed from the calling expression.
   * Here, they are only used to locate errors.
   */

  if( *idKey_ == 0 )
    {
      try
	{
	  *idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalVariableKey( selfLoc_, Lang::SELF_ID ) );
	}
      catch( const Exceptions::LookupUnknown & ball )
	{
	  throw Exceptions::MisplacedSuperReference( selfLoc_ );
	}
    }
}

void
Ast::ProtectedMethodReferenceFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  typedef const Lang::Instance SelfType;
  RefCountPtr< const Lang::Value > untypedSelf = evalState->env_->getVarHandle( **idKey_ )->getUntyped( );
  SelfType * typedSelf = dynamic_cast< SelfType * >( untypedSelf.getPtr( ) );
  if( typedSelf == 0 )
    {
      throw Exceptions::InternalError( strrefdup( "ProtectedMethodAccess:  the self variable was not an instance." ) );
    }

  Kernel::MethodId methodId( args.getHandle( 0 )->getVal< const Lang::Class >( methodClass_->loc( ) ), methodName_ );
  
  if( parent_ != 0 )
    {
      RefCountPtr< const Lang::Class > typedParent = args.getHandle( 1 )->getVal< const Lang::Class >( parent_->loc( ) );
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( typedSelf->superReference( typedParent )->getLocalMethod( methodId ),
		       evalState );
      return;
    }

  /* parent == 0 means a reference to a local override */
  Kernel::ContRef cont = evalState->cont_;
  cont->takeValue( typedSelf->getLocalMethod( methodId ),
		   evalState );
}


Ast::ProtectedMemberReferenceFunction::ProtectedMemberReferenceFunction( const Ast::SourceLocation & loc, const Ast::SourceLocation & selfLoc, Ast::Expression * parent, const Ast::SourceLocation & idLoc, const char * id )
  : Lang::Function( 0 ), loc_( loc ), selfLoc_( selfLoc ), parent_( parent ), idLoc_( idLoc ), id_( id ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{
  std::cerr << "Warning: Ast::ProtectedMemberReferenceFunction::ProtectedMemberReferenceFunction initializes Lang::Function with 0 pointer to formals." << std::endl ;
  /* Currently, a runtime check is used to ensure that this expression indeed refers to a super instance. 
   */
}

Ast::ProtectedMemberReferenceFunction::~ProtectedMemberReferenceFunction( )
{
  delete parent_;
  delete id_;
  if( *idKey_ != 0 )
    {
      delete *idKey_;
    }
  delete idKey_;     //  This can be done only as long as this is not shared!
}

void
Ast::ProtectedMemberReferenceFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( parent_ );
}

void
Ast::ProtectedMemberReferenceFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  /* argument expressions shall be analyzed from the calling expression.
   * Here, they are only used to locate errors.
   */

  if( *idKey_ == 0 )
    {
      try
	{
	  *idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalVariableKey( selfLoc_, Lang::SELF_ID ) );
	}
      catch( const Exceptions::LookupUnknown & ball )
	{
	  throw Exceptions::MisplacedSuperReference( selfLoc_ );
	}
    }
}

void
Ast::ProtectedMemberReferenceFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  typedef const Lang::Instance SelfType;
  RefCountPtr< const Lang::Value > untypedSelf = evalState->env_->getVarHandle( **idKey_ )->getUntyped( );
  SelfType * typedSelf = dynamic_cast< SelfType * >( untypedSelf.getPtr( ) );
  if( typedSelf == 0 )
    {
      throw Exceptions::InternalError( strrefdup( "ProtectedMethodAccess:  the self variable was not an instance." ) );
    }

  RefCountPtr< const Lang::Class > typedParent = args.getHandle( 0 )->getVal< const Lang::Class >( parent_->loc( ) );
  Kernel::ContRef cont = evalState->cont_;
  cont->takeHandle( typedSelf->superReference( typedParent )->getLocalField( id_ ),
		    evalState );
}


Ast::ProtectedMemberInsertionFunction::ProtectedMemberInsertionFunction( const Ast::SourceLocation & loc, const Ast::SourceLocation & selfLoc, Ast::Expression * parent, const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * pieceExpr )
  : Lang::Function( 0 ), loc_( loc ), selfLoc_( selfLoc ), parent_( parent ), idLoc_( idLoc ), id_( id ), pieceExpr_( pieceExpr ), idKey_( new Kernel::Environment::LexicalKey * ( 0 ) )
{
  std::cerr << "Warning: Ast::ProtectedMemberInsertionFunction::ProtectedMemberInsertionFunction initializes Lang::Function with 0 pointer to formals." << std::endl ;
  /* Currently, a runtime check is used to ensure that this expression indeed refers to a super instance. 
   */
}

Ast::ProtectedMemberInsertionFunction::~ProtectedMemberInsertionFunction( )
{
  if( parent_ != 0 )
    {
      delete parent_;
    }
  delete id_;
  delete pieceExpr_;
  if( *idKey_ != 0 )
    {
      delete *idKey_;
    }
  delete idKey_;     //  This can be done only as long as this is not shared!
}

void
Ast::ProtectedMemberInsertionFunction::push_exprs( Ast::ArgListExprs * args ) const
{
  args->orderedExprs_->push_back( pieceExpr_ );
  if( parent_ != 0 )
    {
      args->orderedExprs_->push_back( parent_ );
    }
}

void
Ast::ProtectedMemberInsertionFunction::analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env )
{
  /* argument expressions shall be analyzed from the calling expression.
   * Here, they are only used to locate errors.
   */

  if( *idKey_ == 0 )
    {
      try
	{
	  *idKey_ = new Kernel::Environment::LexicalKey( env->findLexicalStateKey( selfLoc_, Lang::SELF_ID ) );
	}
      catch( const Exceptions::LookupUnknown & ball )
	{
	  throw Exceptions::MisplacedSuperReference( selfLoc_ );
	}
    }
}

void
Ast::ProtectedMemberInsertionFunction::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
  typedef const Lang::Instance SelfType;
  RefCountPtr< const Lang::Value > untypedSelf = evalState->env_->getVarHandle( **idKey_ )->getUntyped( );
  SelfType * typedSelf = dynamic_cast< SelfType * >( untypedSelf.getPtr( ) );
  if( typedSelf == 0 )
    {
      throw Exceptions::InternalError( strrefdup( "ProtectedMethodAccess:  the self variable was not an instance." ) );
    }

  RefCountPtr< const Lang::Value > piece = args.getValue( 0 );

  throw Exceptions::NotImplemented( "Tacking on fields." );

//   if( parent_ == 0 )
//     {
//       typedSelf->getField( id_, untypedSelf )->tackOn( evalState, piece, evalState->dyn_, pieceExpr_->loc( ) );
//       return;
//     }

//   RefCountPtr< const Lang::Class > typedParent = args.getHandle( 1 )->getVal< const Lang::Class >( parent_->loc( ) );
//   typedSelf->superReference( typedParent )->getLocalField( id_ )->tackOn( evalState, piece, evalState->dyn_, pieceExpr_->loc( ) );
}
