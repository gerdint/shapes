/* This file is part of Shapes.
 *
 * Shapes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * Shapes is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2008 Henrik Tidefelt
 */

#ifndef astvar_h
#define astvar_h

#include "Shapes_Kernel_decls.h"

#include "ast.h"

namespace Shapes
{
	namespace Ast
	{

		class CodeBracket : public Expression
		{
			std::list< Ast::Node * > * nodes_;
			std::map< const char *, size_t, charPtrLess > * argumentOrder_;
			std::map< const char *, size_t, charPtrLess > * dynamicMap_;
			std::map< const char *, size_t, charPtrLess > * stateOrder_;
		public:
			CodeBracket( const Ast::SourceLocation & loc, std::list< Ast::Node * > * nodes );
			virtual ~CodeBracket( );

			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;

			void evalAt( const RefCountPtr< const Kernel::CodeBracketContInfo > & info, const std::list< Ast::Node * >::const_iterator & i, Kernel::EvalState * evalState ) const;
		};

	}

	namespace Kernel
	{
		class CodeBracketContInfo
		{
		public:
			const Ast::CodeBracket * bracketExpr_;
			Kernel::PassedEnv env_;
			Kernel::PassedDyn dyn_;
			Kernel::ContRef cont_;

			CodeBracketContInfo( const Ast::CodeBracket * bracketExpr, const Kernel::EvalState & evalState );
			~CodeBracketContInfo( );

			void gcMark( Kernel::GCMarkedSet & marked );
		};

		class CodeBracketContinuation : public Kernel::Continuation
		{
			RefCountPtr< const Kernel::CodeBracketContInfo > info_;
			std::list< Ast::Node * >::const_iterator pos_;
		public:
			CodeBracketContinuation( const Ast::SourceLocation & _traceLoc, const RefCountPtr< const Kernel::CodeBracketContInfo > & _info, const std::list< Ast::Node * >::const_iterator & _pos );
			virtual ~CodeBracketContinuation( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};


		class DynamicBindingContinuation : public Kernel::Continuation
		{
			Kernel::PassedEnv env_;
			Kernel::Environment::LexicalKey key_;
			Ast::SourceLocation idLoc_;
			Kernel::ContRef cont_;
		public:
			DynamicBindingContinuation( const Ast::SourceLocation & traceLoc, const Kernel::PassedEnv & env, const Kernel::Environment::LexicalKey & key, const Ast::SourceLocation & idLoc, const Kernel::ContRef & cont );
			virtual ~DynamicBindingContinuation( );
			virtual void takeHandle( Kernel::VariableHandle val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class WithDynamicContinuation : public Kernel::Continuation
		{
			Ast::Expression * expr_;
			Kernel::PassedEnv env_;
			Kernel::PassedDyn dyn_;
			Kernel::ContRef cont_;
		public:
			WithDynamicContinuation( const Ast::SourceLocation & traceLoc, Ast::Expression * expr, const Kernel::EvalState & evalState );
			virtual ~WithDynamicContinuation( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class DynamicVariableDeclContinuation : public Kernel::Continuation
		{
			const Ast::DynamicVariableDecl * declExpr_;
			Kernel::PassedEnv env_;
			Kernel::PassedDyn dyn_;
			Kernel::ContRef cont_;
		public:
			DynamicVariableDeclContinuation( const Ast::SourceLocation & traceLoc, const Ast::DynamicVariableDecl * declExpr, Kernel::EvalState & evalState );
			virtual ~DynamicVariableDeclContinuation( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class AssertStructureContinuation : public Kernel::Continuation
		{
			Kernel::ContRef cont_;
		public:
			AssertStructureContinuation( const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
			~AssertStructureContinuation( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

	} // End of namespace Kernel.

	namespace Ast
	{
		class LexiographicVariable : public Expression 
		{
			const char * id_;
			mutable Kernel::Environment::LexicalKey ** idKey_;
		public:
			LexiographicVariable( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey );
			virtual ~LexiographicVariable( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class EvalOutsideExpr : public Expression 
		{
			Ast::Expression * expr_;
		public:
			EvalOutsideExpr( const Ast::SourceLocation & loc, Ast::Expression * expr );
			virtual ~EvalOutsideExpr( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class DefineVariable : public BindNode
		{
			Ast::Expression * expr_;
			mutable size_t ** idPos_;
		public:
			DefineVariable( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos );
			virtual ~DefineVariable( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class StructSplitReference : public Expression
		{
			Ast::SourceLocation structLoc_;
			size_t ** structPos_;
			Ast::SourceLocation fieldLoc_;
			const char * fieldId_;
			size_t fieldPos_;
			Ast::Expression * defaultExpr_;
		public:
			StructSplitReference( Ast::SourceLocation fieldLoc, const char * fieldId, Ast::Expression * defaultExpr );
			StructSplitReference( Ast::SourceLocation fieldLoc, size_t fieldPos, Ast::Expression * defaultExpr );
			virtual ~StructSplitReference( );
			void setStruct( Ast::SourceLocation structLoc, size_t ** structPos );
			bool isOrdered( ) const { return fieldId_ == 0; }
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class StructSplitSink : public Expression
		{
			Ast::SourceLocation structLoc_;
			size_t ** structPos_;
			size_t consumedArguments_;
		public:
			StructSplitSink( );
			virtual ~StructSplitSink( );
			void setStruct( Ast::SourceLocation structLoc, size_t ** structPos, size_t consumedArguments );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class AssertNoSinkNeeded : public Expression
		{
			size_t orderedCount_;
			Ast::SourceLocation structLoc_;
			size_t ** structPos_;
		public:
			AssertNoSinkNeeded( const Ast::SourceLocation & loc, size_t orderedCount, Ast::SourceLocation structLoc, size_t ** structPos );
			virtual ~AssertNoSinkNeeded( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class SplitDefineVariables
		{
			static size_t splitVarCount;
			static PtrOwner_back_Access< std::list< const char * > > mem;
			const char * splitVarId_;
		public:
			std::list< std::pair< Ast::DefineVariable *, Ast::StructSplitReference * > > exprs_;
			Ast::DefineVariable * sinkDefine_;
			Ast::StructSplitSink * sinkExpr_;
			bool seenNamed_;
			bool seenDefault_;
			SplitDefineVariables( );
			const char * newSplitVarId( ) const;
		};

		class StateReference : public Node
		{
		public:
			StateReference( const Ast::SourceLocation & loc );
			virtual ~StateReference( );
			virtual void eval( Kernel::EvalState * evalState ) const;	// illegal to call
			virtual Kernel::StateHandle getHandle( Kernel::PassedEnv env, Kernel::PassedDyn dyn ) const = 0;
		};

		class LexiographicState : public StateReference
		{
			const char * id_;
			mutable Kernel::Environment::LexicalKey ** idKey_;
		public:
			LexiographicState( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey );
			virtual ~LexiographicState( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			Kernel::StateHandle getHandle( Kernel::PassedEnv env, Kernel::PassedDyn dyn ) const;
		};

		class DynamicState : public Ast::StateReference
		{
			const char * id_;
			mutable Kernel::Environment::LexicalKey ** idKey_;
		public:
			DynamicState( const Ast::SourceLocation & loc, const char * id );
			virtual ~DynamicState( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			Kernel::StateHandle getHandle( Kernel::PassedEnv env, Kernel::PassedDyn dyn ) const;
		};

		class IntroduceState : public BindNode
		{
			Ast::Expression * expr_;
			mutable size_t ** idPos_;
		public:
			IntroduceState( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, size_t ** idPos );
			virtual ~IntroduceState( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class Insertion : public Node
		{
			Ast::StateReference * stateRef_;
			Ast::Expression * expr_;
		public:
			Insertion( Ast::StateReference * stateRef, Ast::Expression * expr );
			virtual ~Insertion( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class Freeze : public Expression
		{
			const char * id_;
			mutable size_t ** idPos_;
		public:
			Freeze( const Ast::SourceLocation & idLoc, const char * id, size_t ** idPos );
			virtual ~Freeze( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class Peek : public Expression
		{
			Ast::StateReference * stateRef_;
		public:
			Peek( const Ast::SourceLocation & idLoc, Ast::StateReference * stateRef );
			virtual ~Peek( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class EvalSymbolFunction : public Lang::Function
		{
			Ast::SourceLocation loc_;
			Ast::Expression * expr_;
			const Ast::AnalysisEnvironment * analysisEnv_;
		public:
			EvalSymbolFunction( const Ast::SourceLocation & loc, Ast::Expression * expr );
			virtual ~EvalSymbolFunction( );
			void push_exprs( Ast::ArgListExprs * args ) const;
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			virtual bool isTransforming( ) const { return false; }
		};

		class MemberReferenceFunction : public Lang::Function
		{
			Ast::SourceLocation loc_;
			Ast::Expression * variable_;
			const char * fieldID_;
		public:
			MemberReferenceFunction( const Ast::SourceLocation & loc, Ast::Expression * variable, const char * fieldID );
			virtual ~MemberReferenceFunction( );
			void push_exprs( Ast::ArgListExprs * args ) const;
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			virtual bool isTransforming( ) const { return false; }
		};

		class MutatorReference : public Ast::Expression
		{
			Ast::SourceLocation mutatorLoc_;
			Ast::StateReference * state_;
			const char * mutatorID_;
		public:
			MutatorReference( const Ast::SourceLocation & mutatorLoc, Ast::StateReference * state, const char * mutatorID );
			virtual ~MutatorReference( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class SpecialLength : public Expression
		{
			Ast::SourceLocation loc_;
			double val_;
			int sort_;
		public:
			SpecialLength( const Ast::SourceLocation & loc, double val, int sort );
			virtual ~SpecialLength( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class DynamicVariable : public Ast::Expression
		{
			Ast::SourceLocation loc_;
			const char * id_;
			mutable Kernel::Environment::LexicalKey ** idKey_;
		public:
			DynamicVariable( const Ast::SourceLocation & loc, const char * id );
			virtual ~DynamicVariable( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class DynamicBindingExpression : public Ast::Expression
		{
			Ast::SourceLocation idLoc_;
			const char * id_;
			Ast::Expression * expr_;
			mutable Kernel::Environment::LexicalKey ** idKey_;
		public:
			DynamicBindingExpression( const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * expr, Kernel::Environment::LexicalKey ** idKey );
			virtual ~DynamicBindingExpression( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class DynamicStateBindingExpression : public Ast::Expression
		{
			Ast::SourceLocation loc_;
			Ast::SourceLocation dstLoc_;
			const char * dstId_;
			mutable Kernel::Environment::LexicalKey ** dstIdKey_;
			Ast::StateReference * src_;
		public:
			DynamicStateBindingExpression( const Ast::SourceLocation & loc, const Ast::SourceLocation & dstLoc, const char * dstId, Ast::StateReference * src );
			virtual ~DynamicStateBindingExpression( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class WithDynamicExpr : public Ast::Expression
		{
			Ast::Expression * bindings_;
			Ast::Expression * expr_;
		public:
			WithDynamicExpr( const Ast::SourceLocation & loc, Ast::Expression * bindings, Ast::Expression * expr );
			virtual ~WithDynamicExpr( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class DynamicVariableDecl : public Ast::BindNode
		{
			mutable size_t ** idPos_;
			Ast::Expression * impl_;
		public:
			DynamicVariableDecl( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Ast::Expression * filterExpr, Ast::Expression * defaultExpr );
			virtual ~DynamicVariableDecl( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class DynamicVariableDeclFunction : public Lang::Function
		{
			const char * id_;
			Ast::Expression * filterExpr_;
			Ast::Expression * defaultExpr_;
			size_t ** idPos_;
		public:
			DynamicVariableDeclFunction( const char * id, Ast::Expression * filterExpr, Ast::Expression * defaultExpr, size_t ** idPos );
			virtual ~DynamicVariableDeclFunction( );
			void push_exprs( Ast::ArgListExprs * args ) const;
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			virtual bool isTransforming( ) const { return false; }
		};

		class DynamicStateDecl : public Ast::BindNode
		{
			const char * defaultStateID_;
			mutable size_t ** idPos_;
			Ast::StateReference * defaultState_;
		public:
			DynamicStateDecl( const Ast::SourceLocation & loc, const Ast::SourceLocation & idLoc, const char * id, Ast::StateReference * defaultState, size_t ** idPos );
			virtual ~DynamicStateDecl( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class DynamicExpression : public Ast::Expression
		{
			Ast::Expression * expr_;
		public:
			DynamicExpression( const Ast::SourceLocation & loc, Ast::Expression * expr );
			virtual ~DynamicExpression( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class LexiographicType : public Expression 
		{
			const char * id_;
			mutable Kernel::Environment::LexicalKey ** idKey_;
		public:
			LexiographicType( const Ast::SourceLocation & loc, const char * id, Kernel::Environment::LexicalKey ** idKey );
			virtual ~LexiographicType( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};


	}
}


#endif
