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

#ifndef upsamplers_h
#define upsamplers_h

#include "bezier.h"
#include "elementarycoords.h"

#include <vector>

namespace Shapes
{
	namespace Computation
	{
		class Upsampler2D
		{
		public:
			Upsampler2D( ){ }
			virtual ~Upsampler2D( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const = 0;
		};

		class Upsampler3D
		{
		public:
			Upsampler3D( ){ }
			virtual ~Upsampler3D( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords3D > & controls ) const = 0;
		};

		class UpsampleInflections : public Upsampler2D
		{
		public:
			UpsampleInflections( ){ }
			virtual ~UpsampleInflections( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const;
		};

		class UpsampleBends : public Upsampler2D
		{
			double maxAngle_;
		public:
			UpsampleBends( double maxAngle )
				: maxAngle_( maxAngle )
			{ }
			virtual ~UpsampleBends( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const;
		};

		class UpsampleEvery2D : public Upsampler2D
		{
			Concrete::Length period_;
		public:
			UpsampleEvery2D( Concrete::Length period )
				: period_( period )
			{ }
			virtual ~UpsampleEvery2D( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const;
		};

		class UpsampleEvery3D : public Upsampler3D
		{
			Concrete::Length period_;
		public:
			UpsampleEvery3D( Concrete::Length period )
				: period_( period )
			{ }
			virtual ~UpsampleEvery3D( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords3D > & controls ) const;
		};

		class UpsampleDifferentiably2D : public Upsampler2D
		{
		public:
			UpsampleDifferentiably2D( ){ }
			virtual ~UpsampleDifferentiably2D( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords2D > & controls ) const;
		};

		class UpsampleDifferentiably3D : public Upsampler3D
		{
		public:
			UpsampleDifferentiably3D( ){ }
			virtual ~UpsampleDifferentiably3D( ){ }
			virtual void operator () ( std::vector< double > * dst, const Bezier::ControlPoints< Concrete::Coords3D > & controls ) const;
		};

	}
}


#endif
