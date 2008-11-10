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

#ifndef charconverters_h
#define charconverters_h


#include "refcount.h"		 // Why?!	(You get strange compiler errors if you dont include refcount.h here.)

#include "FontMetrics_decls.h"

#include <iconv.h>

namespace Shapes
{
	namespace Helpers
	{

		extern const char * theUCS4EncodingName;

		iconv_t requireUTF8ToMacRomanConverter( bool cleanup = false );
		iconv_t requireMacRomanToUTF8Converter( bool cleanup = false );

		iconv_t requireUTF8ToASCIIConverter( bool cleanup = false );

		iconv_t requireUTF8ToUCS4Converter( bool cleanup = false );
		iconv_t requireUTF16BEToUCS4Converter( bool cleanup = false );

		iconv_t requireUTF8ToWinANSIConverter( bool cleanup = false );

		const FontMetrics::GlyphList & requireGlyphList( bool cleanup = false );
		const FontMetrics::CharacterEncoding & requireMacRomanEncoding( bool cleanup = false );
	}
}


#endif
