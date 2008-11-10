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

#ifndef astexpr_h
#define astexpr_h

#include "ast.h"

namespace Shapes
{
	namespace Ast
	{

		class UnaryExpr : public Expression
		{
		public:
			UnaryExpr( const Ast::SourceLocation & loc );
			virtual ~UnaryExpr( );
			virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg ) const = 0;
			DEFAULTUNARYOPDECL;
		};

		class UnaryPrefixExpr : public UnaryExpr
		{
		protected:
			Ast::SourceLocation opLoc_;
			Ast::Expression * expr_;
		public:
			UnaryPrefixExpr( const Ast::SourceLocation & loc, const Ast::SourceLocation & opLoc, Ast::Expression * expr );
			UnaryPrefixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr );
			virtual ~UnaryPrefixExpr( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
			virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg ) const;
		};

		class UnaryPostfixExpr : public UnaryExpr
		{
		protected:
			Ast::SourceLocation opLoc_;
			Ast::Expression * expr_;
		public:
			UnaryPostfixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr );
			virtual ~UnaryPostfixExpr( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
			virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg ) const;
		};

		class BinaryInfixExpr : public Expression
		{
		protected:
			Ast::SourceLocation opLoc_;
			Ast::Expression * expr1_;
			Ast::Expression * expr2_;
		public:
			BinaryInfixExpr( const Ast::SourceLocation & opLoc, Ast::Expression * expr1,	Ast::Expression * expr2 );
			virtual ~BinaryInfixExpr( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
			virtual RefCountPtr< const Lang::Value > throwNotApplicable( const Lang::Value * arg1, const Lang::Value * arg2 ) const;
			const Ast::Expression * get_expr2( ) const { return expr2_; } /* to be used by the continuation */
			DEFAULTBINARYOPDECL;
		};

	}

	namespace Kernel
	{

		class UnaryCont_1 : public Kernel::Continuation
		{
			const Ast::UnaryExpr * op_;
			Kernel::PassedDyn dyn_;
			Kernel::ContRef cont_;
		public:
			UnaryCont_1( const Ast::UnaryExpr * op, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
			virtual ~UnaryCont_1( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class BinaryInfixCont_2 : public Kernel::Continuation
		{
			const Ast::BinaryInfixExpr * op_;
			Kernel::ValueRef val1_;
			Kernel::PassedDyn dyn_;
			Kernel::ContRef cont_;
		public:
			BinaryInfixCont_2( const Ast::BinaryInfixExpr * op, const Kernel::ValueRef & val1, const Kernel::PassedDyn & dyn, const Kernel::ContRef & cont, const Ast::SourceLocation & traceLoc );
			virtual ~BinaryInfixCont_2( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class BinaryInfixCont_1 : public Kernel::Continuation
		{
			const Ast::BinaryInfixExpr * op_;
			Kernel::PassedEnv env_;
			Kernel::PassedDyn dyn_;
			Kernel::ContRef cont_;
		public:
			BinaryInfixCont_1( const Ast::BinaryInfixExpr * op, Kernel::EvalState & evalState, const Ast::SourceLocation & traceLoc );
			virtual ~BinaryInfixCont_1( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

	}
}


#endif
