#ifndef containertypes_h
#define containertypes_h

#include <list>
#include <iostream>
#include <stack>
#include <set>

#include "ptrowner.h"
#include "refcount.h"
#include "pdfstructure.h"
#include "shapesvalue.h"
#include "environment.h"
#include "charptrless.h"

namespace Shapes
{
	namespace Lang
	{

	class SingleList : public Lang::NoOperatorOverloadValue
	{
	public:
		SingleList( );
		virtual ~SingleList( );
		virtual bool isNull( ) const = 0;
		virtual void foldl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const = 0;
		virtual void foldr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const = 0;
		virtual void foldsl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const = 0;
		virtual void foldsr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const = 0;
		virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
		TYPEINFODECL;
	};

	class SingleListPair : public Lang::SingleList
	{
	public:
		/* The data is provided public becase it is used in function application
		 */
		Kernel::VariableHandle car_;
		RefCountPtr< const Lang::SingleList > cdr_;

		SingleListPair( const Kernel::VariableHandle & car, const RefCountPtr< const Lang::SingleList > & cdr );
		virtual ~SingleListPair( );
		virtual bool isNull( ) const;
		virtual void foldl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const;
		virtual void foldr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const;
		virtual void foldsl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const;
		virtual void foldsr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class SingleListNull : public Lang::SingleList
	{
	public:
		SingleListNull( );
		virtual ~SingleListNull( );
		virtual bool isNull( ) const;
		virtual void foldl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const;
		virtual void foldr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, const Ast::SourceLocation & callLoc ) const;
		virtual void foldsl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const;
		virtual void foldsr( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Function > & op, const Kernel::VariableHandle & nullResult, Kernel::StateHandle state, const Ast::SourceLocation & callLoc ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
	};


	class SingleListMethodBase : public Lang::Function
	{
	protected:
		RefCountPtr< const Lang::SingleList > self_;
	public:
		SingleListMethodBase( RefCountPtr< const Lang::SingleList > self, Kernel::EvaluatedFormals * formals );
		virtual ~SingleListMethodBase( );
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		virtual bool isTransforming( ) const;
	};

	class SingleListMethodFoldL : public Lang::SingleListMethodBase
	{
	public:
		SingleListMethodFoldL( RefCountPtr< const Lang::SingleList > _self );
		virtual ~SingleListMethodFoldL( );
		virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
	};

	class SingleListMethodFoldR : public Lang::SingleListMethodBase
	{
	public:
		SingleListMethodFoldR( RefCountPtr< const Lang::SingleList > _self );
		virtual ~SingleListMethodFoldR( );
		virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
	};

	class SingleListMethodFoldSL : public Lang::SingleListMethodBase
	{
	public:
		SingleListMethodFoldSL( RefCountPtr< const Lang::SingleList > _self );
		virtual ~SingleListMethodFoldSL( );
		virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
	};

	class SingleListMethodFoldSR : public Lang::SingleListMethodBase
	{
	public:
		SingleListMethodFoldSR( RefCountPtr< const Lang::SingleList > _self );
		virtual ~SingleListMethodFoldSR( );
		virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
	};

		class Structure : public Lang::NoOperatorOverloadValue
		{
			bool argListOwner_;
		public:
			/* The data is provided public becase it is used in function application
			 */
			const Ast::ArgListExprs * argList_;
			RefCountPtr< const Lang::SingleList > values_;
			Structure( const Ast::ArgListExprs * argList, const RefCountPtr< const Lang::SingleList > & values, bool argListOwner = false );
			virtual ~Structure( );
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			Kernel::VariableHandle getPosition( size_t pos, const RefCountPtr< const Lang::Value > & selfRef ) const;
			RefCountPtr< const Lang::Structure > getSink( size_t consumedArguments ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

	class ConsPair : public Lang::NoOperatorOverloadValue
	{
		Kernel::VariableHandle car_;
		Kernel::VariableHandle cdr_;
	public:
		ConsPair( const Kernel::VariableHandle & car, const Kernel::VariableHandle & cdr );
		virtual ~ConsPair( );
		virtual void show( std::ostream & os ) const;
		virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		TYPEINFODECL;
	};

	}
}

#endif
