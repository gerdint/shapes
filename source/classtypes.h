#ifndef classtypes_h
#define classtypes_h

#include "Shapes_Ast_decls.h"
#include "Shapes_Lang_decls.h"

#include "ptrowner.h"
#include "refcount.h"
#include "pdfstructure.h"
#include "shapesvalue.h"
#include "environment.h"
#include "charptrless.h"
#include "methodid.h"
#include "drawabletypes.h"
#include "hottypes.h"

#include <list>
#include <iostream>
#include <stack>
#include <set>

namespace Shapes
{
	namespace Lang
	{

	class Instance : public Lang::Drawable2D
	{
	protected:
		mutable Kernel::PassedEnv env;
		Kernel::PassedEnv privateEnv;

		Kernel::WarmGroup2D * warm2D;
		Kernel::WarmGroup3D * warm3D;
		Kernel::PassedDyn my_dyn;
	public:
		RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > parents; /* the reference is non-const to allow prepared to be changed */
		/* public scope is the easy way to let a Class add overrides to its instances */
		std::map< RefCountPtr< const Lang::Class >, std::map< const char *, RefCountPtr< const Lang::Function >, charPtrLess > > overrides;
	protected:
		RefCountPtr< const Lang::Class > myClass;
		bool prepared;
		bool protectedAccess;
		mutable std::map< Kernel::MethodId, RefCountPtr< const Lang::Function > > methodTable;
	public:
		Instance( Kernel::PassedEnv _env, Kernel::PassedEnv _privateEnv, RefCountPtr< const Lang::Class > _myClass, bool _protectedAccess, const Kernel::PassedDyn & _my_dyn );
		virtual ~Instance( );
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		void prepare( Kernel::EvalState * evalState );
		virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
		Kernel::VariableHandle getLocalField( const char * fieldID ) const;
		virtual void tackOn( const char * fieldID, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc );
		virtual void tackOnLocal( const char * fieldID, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc );
		RefCountPtr< const Lang::Function > getMethod( Kernel::MethodId fieldID ) const;
		RefCountPtr< const Lang::Function > getLocalMethod( Kernel::MethodId fieldID ) const;
		RefCountPtr< const Lang::Instance > superReference( RefCountPtr< const Lang::Class > parent ) const;

		virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;

		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;

		virtual const RefCountPtr< const Lang::Class > & getClass( ) const;
		static RefCountPtr< const char > staticTypeName( );

		virtual void show( std::ostream & os ) const;
		DISPATCHDECL;
	};

	class TransformedInstance : public Lang::Drawable2D
	{
		Lang::Transform2D tf;
		RefCountPtr< const Lang::Instance > obj;
	public:
		TransformedInstance( const Lang::Transform2D & _tf, const RefCountPtr< const Lang::Instance > & _obj );
		~TransformedInstance( );
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
		RefCountPtr< const Lang::Function > getMethod( Kernel::MethodId fieldID ) const;

		virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;

		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;

		TYPEINFODECL;
		virtual void show( std::ostream & os ) const;
	};

	class Class : public Lang::Value
	{
	public:
		typedef std::map< Kernel::MethodId, std::set< RefCountPtr< const Lang::Class > > > MessageMapType;
	private:
		RefCountPtr< const char > prettyName;
	protected:
		mutable RefCountPtr< const Lang::Class > selfRef;
		bool isFinal;
		MessageMapType messageMap;
		std::set< Kernel::MethodId > abstractSet;
	public:
		Class( RefCountPtr< const char > _prettyName );
		virtual ~Class( );
		void setSelfRef( RefCountPtr< const Lang::Class > _selfRef ) const;

		virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;

		RefCountPtr< const char > getPrettyName( ) const;

		virtual bool isInPublicGetSet( const char * field ) const;
		virtual bool isInPublicSetSet( const char * field ) const;
		virtual bool isInProtectedGetSet( const char * field ) const;
		virtual bool isInProtectedSetSet( const char * field ) const;
		virtual bool isInTransformingSet( const char * field ) const;

		virtual Kernel::ValueRef method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const = 0;
		virtual bool method_isa( RefCountPtr< const Lang::Class > T ) const = 0;
		//		virtual Kernel::ValueRef method_implements( ) const = 0;

		virtual void findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const = 0;
		virtual void assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const = 0;
		virtual void superNew( RefCountPtr< Lang::Instance > instanceSelf,
													 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
													 Kernel::Arguments & args,
													 Kernel::EvalState * evalState ) const = 0;
		virtual void findParents( std::set< RefCountPtr< const Lang::Class > > * _allParents, std::set< RefCountPtr< const Lang::Class > > * _multiParents ) const;
		virtual bool isRepeatableBase( ) const = 0;
		virtual void prepareInstance( Kernel::EvalState * evalState, Kernel::PassedEnv privateEnv ) const;

		bool getFinal( ) const;
		const MessageMapType & getMessageMap( ) const;
		const RefCountPtr< const Lang::Class > & getMethodDefinitionClass( const Kernel::MethodId & method ) const;

		virtual RefCountPtr< const Lang::Function > getMutator( const char * mutatorID ) const;

		void showAbstractSet( std::ostream & os ) const;

		TYPEINFODECL;
		virtual void show( std::ostream & os ) const;
		DISPATCHDECL;
	};

	class Object : public Lang::Class
	{
		Kernel::PassedEnv dummyEnv;
	public:
		Object( );
		virtual ~Object( );
		virtual void gcMark( Kernel::GCMarkedSet & marked );

		virtual Kernel::ValueRef method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
		virtual bool method_isa( RefCountPtr< const Lang::Class > T ) const;

		virtual void findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const;
		virtual void assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const;
		virtual void superNew( RefCountPtr< Lang::Instance > instanceSelf,
													 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
													 Kernel::Arguments & emptyArglist,
													 Kernel::EvalState * evalState ) const;
		virtual bool isRepeatableBase( ) const;
	};

	class MetaClass : public Lang::Class
	{
	public:
		MetaClass( );
		virtual ~MetaClass( );

		virtual Kernel::ValueRef method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
		virtual bool method_isa( RefCountPtr< const Lang::Class > T ) const;

		virtual void findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const;
		virtual void assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const;
		virtual void superNew( RefCountPtr< Lang::Instance > instanceSelf,
													 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
													 Kernel::Arguments & emptyArglist,
													 Kernel::EvalState * evalState ) const;
		virtual void findParents( std::set< RefCountPtr< const Lang::Class > > * _allParents, std::set< RefCountPtr< const Lang::Class > > * _multiParents ) const;
		virtual bool isRepeatableBase( ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class SystemFinalClass : public Lang::Class
	{
		std::map< const char *, RefCountPtr< const Lang::Function >, charPtrLess > mutators_;
		typedef void ( * RegisterMutatorFunction )( SystemFinalClass * );
		RegisterMutatorFunction registerMutatorFunction_;
	public:
		SystemFinalClass( RefCountPtr< const char > _prettyName );
		SystemFinalClass( RefCountPtr< const char > _prettyName, RegisterMutatorFunction registerMutatorFunction );
		virtual ~SystemFinalClass( );
		void initMutators( );
		void registerMutator( Lang::CoreFunction * fun );

		virtual Kernel::ValueRef method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
		virtual bool method_isa( RefCountPtr< const Lang::Class > T ) const;

		virtual void findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const;
		virtual void assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const;
		virtual void superNew( RefCountPtr< Lang::Instance > instanceSelf,
													 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
													 Kernel::Arguments & emptyArglist,
													 Kernel::EvalState * evalState ) const;
		virtual bool isRepeatableBase( ) const;
		virtual RefCountPtr< const Lang::Function > getMutator( const char * mutatorID ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class SystemVirtualInterface : public Lang::Class
	{
	public:
		SystemVirtualInterface( RefCountPtr< const char > _prettyName );
		virtual ~SystemVirtualInterface( );

		void addVirtual( const char * id );

		virtual Kernel::ValueRef method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
		virtual bool method_isa( RefCountPtr< const Lang::Class > T ) const;

		virtual void findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const;
		virtual void assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const;
		virtual void superNew( RefCountPtr< Lang::Instance > instanceSelf,
													 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
													 Kernel::Arguments & emptyArglist,
													 Kernel::EvalState * evalState ) const;
		virtual bool isRepeatableBase( ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class UserClass : public Lang::Class
	{
		const Shapes::Ast::ClassFunction * classExpr;
		Kernel::EvaluatedFormals * formals;
		Kernel::PassedEnv env;
		RefCountPtr< std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > > > parents;
		std::set< RefCountPtr< const Lang::Class > > allParents;
		std::set< RefCountPtr< const Lang::Class > > multiParents;
		std::set< RefCountPtr< const Lang::Class > > immediateParents;
		RefCountPtr< std::map< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > > > overrides;
	public:
		UserClass( const Shapes::Ast::ClassFunction * _classExpr, Kernel::PassedEnv _env, RefCountPtr< const char > _prettyName, Kernel::EvaluatedFormals * _formals, RefCountPtr< std::list< std::pair< RefCountPtr< const Lang::Class >, const Ast::ArgListExprs * > > > _parents, RefCountPtr< std::map< RefCountPtr< const Lang::Class >, std::list< Ast::MemberDeclaration * > > > _overrides, bool _isFinal );
		virtual ~UserClass( );

		virtual void gcMark( Kernel::GCMarkedSet & marked );

		void setupAndCheck( bool declaredAbstract );

		virtual bool isInPublicGetSet( const char * field ) const;
		virtual bool isInPublicSetSet( const char * field ) const;
		virtual bool isInProtectedGetSet( const char * field ) const;
		virtual bool isInProtectedSetSet( const char * field ) const;
		virtual bool isInTransformingSet( const char * field ) const;

		virtual Kernel::ValueRef method_new( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
		virtual bool method_isa( RefCountPtr< const Lang::Class > T ) const;

		virtual void findMultiplyInheritedClasses( std::set< RefCountPtr< Lang::Class > > * visited, std::set< RefCountPtr< Lang::Class > > * found ) const;
		virtual void assertMethodOverridable( const char * id, const RefCountPtr< const Lang::Class > & caller ) const;
		virtual void superNew( RefCountPtr< Lang::Instance > instanceSelf,
													 RefCountPtr< std::map< RefCountPtr< const Lang::Class >, RefCountPtr< Lang::Instance > > > createdObjects,
													 Kernel::Arguments & args,
													 Kernel::EvalState * evalState ) const;
		virtual void findParents( std::set< RefCountPtr< const Lang::Class > > * _allParents, std::set< RefCountPtr< const Lang::Class > > * _multiParents ) const;
		virtual bool isRepeatableBase( ) const;
		virtual void prepareInstance( Kernel::EvalState * evalState, Kernel::PassedEnv privateEnv ) const;

		void addOverrides( Kernel::EvalState * evalState, RefCountPtr< Lang::Instance > instance, Kernel::PassedEnv privateEnv ) const;
	};

	class ClassMethodBase : public Lang::Function
	{
	protected:
		RefCountPtr< const Lang::Class > self;
	public:
		ClassMethodBase( RefCountPtr< const Lang::Class > _self );
		virtual ~ClassMethodBase( );
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		virtual bool isTransforming( ) const;
	};

	class ClassMethodNew : public Lang::ClassMethodBase
	{
	public:
		ClassMethodNew( RefCountPtr< const Lang::Class > _self );
		virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
	};

	class ClassMethodIsa : public Lang::ClassMethodBase
	{
	public:
		ClassMethodIsa( RefCountPtr< const Lang::Class > _self );
		virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
	};

	}
}

#endif
