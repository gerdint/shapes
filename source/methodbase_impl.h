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


namespace Shapes
{

	template< class T >
	Lang::MethodBase< T >::MethodBase( RefCountPtr< const T > self, const char * title, bool transforming, bool forceAll )
		: Kernel::MethodBaseBase( title, forceAll ),
			self_( self ), transforming_( transforming )
	{ }

	template< class T >
	Lang::MethodBase< T >::MethodBase( RefCountPtr< const T > self, const char * title, bool transforming )
		: Kernel::MethodBaseBase( title ),
			self_( self ), transforming_( transforming )
	{ }

	template< class T >
	Lang::MethodBase< T >::~MethodBase( )
	{ }

	template< class T >
	void
	Lang::MethodBase< T >::gcMark( Kernel::GCMarkedSet & marked )
	{
		const_cast< T * >( self_.getPtr( ) )->gcMark( marked );
	}

	template< class T >
	bool
	Lang::MethodBase< T >::isTransforming( ) const
	{
		return transforming_;
	}

}
