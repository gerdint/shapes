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

#ifndef shadingtypes_h
#define shadingtypes_h

#include "drawabletypes.h"


namespace Shapes
{
	namespace Lang
	{

		class Type4Shading : public Lang::PaintedPolygon2D
		{
			RefCountPtr< SimplePDF::PDF_Object > resource_;
			RefCountPtr< const Lang::ElementaryPath2D > mybbox_;
		public:
			Type4Shading( RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox );
			virtual ~Type4Shading( );
			virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
			virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( Lang::Drawable2D::BoxType boxType ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

	}
}

#endif
