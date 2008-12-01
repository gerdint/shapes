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

#include "pagecontentstates.h"

using namespace Shapes;


Kernel::PageContentStates::PageContentStates( RefCountPtr< SimplePDF::PDF_Resources > & resources, bool setDefaults )
	: graphics_( true ), text_( true ), resources_( resources )
{
	if( ! setDefaults )
		{
			throw Exceptions::InternalError( strrefdup( "setDefaults must be true in PageContentStates::PageContentStates." ) );
		}

	text_.font_ = NullPtr< const Lang::Font >( );
	text_.size_ = Concrete::Length( std::numeric_limits< double >::signaling_NaN( ) );
	text_.setLeading( Concrete::Length( 0 ) );
}

// Use default constructors for the members.
Kernel::PageContentStates::PageContentStates( RefCountPtr< SimplePDF::PDF_Resources > & resources )
	:	resources_( resources )
{ }

Kernel::PageContentStates::~PageContentStates( )
{ }

