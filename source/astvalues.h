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

#ifndef astvalues_h
#define astvalues_h

#include "ast.h"

namespace Shapes
{
	namespace Ast
	{

		class Constant : public Expression
		{
			Kernel::VariableHandle val_;
		public:
			Constant( const Ast::SourceLocation & loc, const Kernel::VariableHandle & val );
			Constant( const Ast::SourceLocation & loc, RefCountPtr< const Lang::Value > val );
			Constant( const Ast::SourceLocation & loc, const Lang::Value * val );
			virtual ~Constant( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class PolarHandle2DExpr : public Expression
		{
			Ast::Expression * rExpr_;
			Ast::Expression * aExpr_;
		public:
			PolarHandle2DExpr( const Ast::SourceLocation & loc, Ast::Expression * rExpr, Ast::Expression * aExpr );
			virtual ~PolarHandle2DExpr( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		class PolarHandle2DExprFree_a : public Expression
		{
			Ast::Expression * rExpr_;
		public:
			PolarHandle2DExprFree_a( const Ast::SourceLocation & loc, Ast::Expression * rExpr );
			virtual ~PolarHandle2DExprFree_a( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

		/* This class really doesn't fit in any file, but here it is!
		 */
		class EmptyExpression : public Expression
		{
		public:
			EmptyExpression( const Ast::SourceLocation & loc );
			virtual ~EmptyExpression( );
			virtual void analyze( Ast::Node * parent, const Ast::AnalysisEnvironment * env );
			virtual void eval( Kernel::EvalState * evalState ) const;
		};

	}

	namespace Kernel
	{

		class PolarHandle2DCont : public Kernel::Continuation
		{
			RefCountPtr< Kernel::PolarHandlePromise > rPromise_;
			Kernel::ContRef cont_;
		public:
			PolarHandle2DCont( const Ast::SourceLocation & _traceLoc, const RefCountPtr< Kernel::PolarHandlePromise > & rPromise, const Kernel::ContRef & cont );
			virtual ~PolarHandle2DCont( );
			virtual void takeValue( const RefCountPtr< const Lang::Value > & val, Kernel::EvalState * evalState, bool dummy ) const;
			virtual void backTrace( std::list< Kernel::Continuation::BackTraceElem > * trace ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

	}

}

#endif
