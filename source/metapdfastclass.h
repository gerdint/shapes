#ifndef metapdfastclass_h
#define metapdfastclass_h

#include "metapdfast.h"
#include "metapdfastfun.h"
#include "consts.h"
#include "ptrowner.h"
#include "classtypes.h"
#include "charptrless.h"

namespace MetaPDF
{
  namespace Ast
  {
    class MemberDeclaration
    {
    public:
      Ast::SourceLocation loc_;
      const char * id_;
      Ast::Expression * init_;
      Ast::MemberMode mode_;
      MemberDeclaration( const Ast::SourceLocation & loc, const char * id, Ast::Expression * init, const Ast::MemberMode & mode );
      ~MemberDeclaration( );
      void addModeBits( const Ast::MemberMode & bits );
      void checkModeConsistency( );
    };
    
    class ClassSection
    {
    public:
      ClassSection( );
      virtual ~ClassSection( );
    };
    
    class MemberSection : public ClassSection, public std::list< Ast::MemberDeclaration * >
    {
    public:
      MemberSection( );
      virtual ~MemberSection( );
      void addModeBits( const Ast::MemberMode & bits );
    };
    
    class PrepareSection : public ClassSection
    {
      Ast::SourceLocation loc_;
    public:
      std::list< Ast::Node * > * nodes_;
      PrepareSection( const Ast::SourceLocation & loc, std::list< Ast::Node * > * nodes );
      virtual ~PrepareSection( );
    };
    
    class AbstractSection : public ClassSection
    {
    public:
      Ast::SourceLocation loc_;
      const std::list< RefCountPtr< const char > > * methods_;
      AbstractSection( const Ast::SourceLocation & loc, const std::list< RefCountPtr< const char > > * methods );
      virtual ~AbstractSection( );
    };
    
    class OverridesSection : public ClassSection
    {
    public:
      Ast::Expression * super_;
      Ast::MemberSection * members_;
      OverridesSection( Ast::Expression * super, Ast::MemberSection * members );
      virtual ~OverridesSection( );
    };
    
    class ClassFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      Ast::Expression * name_;
      const Kernel::Formals * constructorFormals_;
      std::list< const Ast::CallExpr * > * parentsWithInitArgs_;
      PtrOwner_back_Access< std::list< Ast::Node * > > preparations_;
      PtrOwner_back_Access< std::list< Ast::MemberDeclaration * > > members_;
      PtrOwner_back_Access< std::list< Ast::OverridesSection * > > overrides_;
      typedef std::set< const char *, charPtrLess > FieldSetType;
      FieldSetType publicGetSet_;
      FieldSetType publicSetSet_;
      FieldSetType protectedGetSet_;
      FieldSetType protectedSetSet_;
      FieldSetType abstractSet_;
      FieldSetType finalSet_;
      FieldSetType transformingSet_;
      bool isAbstract_;
      bool isFinal_;
    public:
      ClassFunction( const Ast::SourceLocation & loc, Ast::Expression * name, const Kernel::Formals * constructorFormals, std::list< const Ast::CallExpr * > * parentsWithInitArgs, Ast::MemberMode classMode, std::list< Ast::ClassSection * > * sections );
      virtual ~ClassFunction( );
      
      void push_exprs( Ast::ArgListExprs * args ) const;
      
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      
      bool isInPublicGetSet( const char * field ) const;
      bool isInPublicSetSet( const char * field ) const;
      bool isInProtectedGetSet( const char * field ) const;
      bool isInProtectedSetSet( const char * field ) const;
      bool isInAbstractSet( const char * field ) const;
      bool isInFinalSet( const char * field ) const;
      bool isInTransformingSet( const char * field ) const;
      
      Lang::Class::MessageMapType getLocalMessageMap( RefCountPtr< const Lang::Class > _myClass ) const;
      
      bool isRepeatableBase( ) const;
      //    void bindInitializationArguments( RefCountPtr< const Lang::Class > theClass, Kernel::PassedEnv initEnv, Kernel::Arguments & args ) const;
      void setupInstance( Kernel::PassedEnv instanceEnv, Kernel::PassedEnv privateEnv, Kernel::EvalState * evalState, Kernel::PassedEnv initEnv ) const;
      void prepareInstance( Kernel::EvalState * evalState, Kernel::PassedEnv privateEnv ) const;
      
      const Ast::SourceLocation & loc( ) const;
      
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };

    class MethodIdExpr
    {
      Ast::SourceLocation loc_;
    public:
      Ast::Expression * classPart_;
      const char * name_;
      MethodIdExpr( const Ast::SourceLocation & loc, Ast::Expression * classPart, const char * name );
      ~MethodIdExpr( );
    };

    class PublicMethodReferenceFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      Ast::Expression * obj_;
      Ast::Expression * methodClass_;
      const char * methodName_;
    public:
      PublicMethodReferenceFunction( const Ast::SourceLocation & loc, Ast::Expression * obj, MethodIdExpr * methodId );
      ~PublicMethodReferenceFunction( );
      void push_exprs( Ast::ArgListExprs * args ) const;
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };
    
    class ProtectedMethodReferenceFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      Ast::SourceLocation selfLoc_;
      Ast::Expression * parent_;
      Ast::Expression * methodClass_;
      const char * methodName_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      ProtectedMethodReferenceFunction( const Ast::SourceLocation & loc, const Ast::SourceLocation & selfLoc, Ast::Expression * parent, Ast::MethodIdExpr * methodId );
      ~ProtectedMethodReferenceFunction( );
      void push_exprs( Ast::ArgListExprs * args ) const;
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };
    
    class ProtectedMemberReferenceFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      Ast::SourceLocation selfLoc_;
      Ast::Expression * parent_;
      Ast::SourceLocation idLoc_;
      const char * id_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      ProtectedMemberReferenceFunction( const Ast::SourceLocation & loc, const Ast::SourceLocation & selfLoc, Ast::Expression * parent, const Ast::SourceLocation & idloc, const char * id );
      ~ProtectedMemberReferenceFunction( );
      void push_exprs( Ast::ArgListExprs * args ) const;
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };
    
    class ProtectedMemberInsertionFunction : public Lang::Function
    {
      Ast::SourceLocation loc_;
      Ast::SourceLocation selfLoc_;
      Ast::Expression * parent_;
      Ast::SourceLocation idLoc_;
      const char * id_;
      Ast::Expression * pieceExpr_;
      mutable Kernel::Environment::LexicalKey ** idKey_;
    public:
      ProtectedMemberInsertionFunction( const Ast::SourceLocation & loc, const Ast::SourceLocation & selfLoc, Ast::Expression * parent, const Ast::SourceLocation & idloc, const char * id, Ast::Expression * pieceExpr );
      ~ProtectedMemberInsertionFunction( );
      void push_exprs( Ast::ArgListExprs * args ) const;
      virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
      virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
      virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
      virtual bool isTransforming( ) const { return false; }
    };

  }
}


#endif
