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

#include "classtreemacros.h"
#include "shapesvalue.h"
#include "shapestypes.h"

using namespace Shapes;

#define GETTYPEID_MAKER( T ) \
Kernel::QuickTypeID \
Lang::T::getTypeID( ) const \
{\
	return Kernel::TYPEID_ ## T; \
}

SINGLELOOP1( CLASSTREE1_ROOT, GETTYPEID_MAKER )

