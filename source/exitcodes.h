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

#ifndef exitcodes_h
#define exitcodes_h


namespace Shapes
{
	namespace Interaction
	{
		enum ExitCode{
			EXIT_OK = 0,
			EXIT_GENERIC_ERROR = 1, EXIT_USER_ERROR = 2, EXIT_TEX_ERROR = 3, EXIT_EXTERNAL_ERROR = 4, EXIT_INVOCATION_ERROR = 5, EXIT_TOLERANCE_ERROR = 6,
			EXIT_INTERNAL_ERROR = 10, EXIT_NOT_IMPLEMENTED = 11,
			EXIT_FILE_ERROR = 20, EXIT_INPUT_FILE_ERROR = 21, EXIT_OUTPUT_FILE_ERROR = 22, EXIT_FILE_PERMISSION_ERROR = 23, EXIT_NO_DIRECTORY_ERROR = 24,
		};
	}
}


#endif
