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

#ifndef timetypes_h
#define timetypes_h

#include "refcount.h"
#include "shapesvalue.h"

#include <ctime>

namespace Shapes
{

	namespace Lang
	{

		class ChronologicalTime : public Lang::NoOperatorOverloadValue
		{
			time_t t_;
		public:
			ChronologicalTime( time_t t );
			virtual ~ChronologicalTime( );
			const struct tm * temporary_localtime( ) const;
			const time_t & val( ) const { return t_; }
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			TYPEINFODECL;
		};

		/*
		class ChronologicalDuration : public Lang::Value
		{
			time_t diff_;
		public:
			ChronologicalDuration( time_t diff );
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			TYPEINFODECL;
			virtual Kernel::QuickTypeID getTypeID( ) const { return Kernel::TYPEID_NoOperatorOverloadValue; };
		};
		*/
	}
}

#endif
