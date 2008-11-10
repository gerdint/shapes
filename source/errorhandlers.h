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

#ifndef errorhandlers_h
#define errorhandlers_h

#include "functiontypes.h"
#include "shapesexceptions.h"

namespace Shapes
{
	namespace Lang
	{

	class ErrorHandler : public Lang::Function
	{
	public:
		ErrorHandler( );
		virtual ~ErrorHandler( );
		virtual bool isTransforming( ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
	};

	template< class T >
		class ExceptionWrapper : public Lang::ErrorHandler
	{
	protected:
		const char * title;
		const char * msg;
	public:
		ExceptionWrapper( const char * _title, const char * _msg );
		virtual ~ExceptionWrapper( );
		virtual void call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const;
	};

	}
}


template< class T >
Shapes::Lang::ExceptionWrapper< T >::ExceptionWrapper( const char * _title, const char * _msg )
	: title( _title ), msg( _msg )
{ }

template< class T >
Shapes::Lang::ExceptionWrapper< T >::~ExceptionWrapper( )
{ }

template< class T >
void
Shapes::Lang::ExceptionWrapper< T >::call( Kernel::EvalState * evalState, Kernel::Arguments & args, const Ast::SourceLocation & callLoc ) const
{
	throw T( msg );
}

#endif
