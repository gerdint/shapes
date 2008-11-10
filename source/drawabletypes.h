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

#ifndef drawabletypes_h
#define drawabletypes_h

#include "Shapes_Lang_decls.h"
#include "Shapes_Ast_decls.h"
#include "Shapes_Computation_decls.h"
#include "Shapes_Kernel_decls.h"

#include "ptrowner.h"
#include "refcount.h"
#include "pdfstructure.h"
#include "shapesvalue.h"
#include "environment.h"
#include "charptrless.h"
#include "functiontypes.h"
#include "elementarycoords.h"
#include "concretecolors.h"
#include "bezier.h"

#include <list>
#include <iostream>
#include <stack>
#include <set>

namespace Shapes
{
	namespace Lang
	{

		class FacetNormalGray : public NoOperatorOverloadValue
		{
			Concrete::Coords3D position_;
			RefCountPtr< const Lang::SpecularReflection > reflections_;
			Concrete::UnitFloatTriple reflectionUnitNormal_;
			Concrete::Gray lightMultiply_;
			RefCountPtr< const Lang::SpecularReflection > autoScattering_;
			Concrete::Gray autoIntensity_;
		public:
			FacetNormalGray( const Concrete::Coords3D & position,
											 const RefCountPtr< const Lang::SpecularReflection > & reflections,
											 const Concrete::UnitFloatTriple & reflectionUnitNormal,
											 const Concrete::Gray lightMultiply,
											 const RefCountPtr< const Lang::SpecularReflection > & autoScattering,
											 const Concrete::Gray autoIntensity );
			virtual ~FacetNormalGray( );
			virtual void gcMark( Kernel::GCMarkedSet & marked );

			Concrete::Gray compute( const Concrete::Coords3D & point, const Concrete::UnitFloatTriple normal, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
			Concrete::Gray compute( const Concrete::Coords3D & point, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
			const Concrete::Coords3D & position( ) const { return position_; }
			const Concrete::UnitFloatTriple & normal( ) const { return reflectionUnitNormal_; }
			Concrete::Gray getDebugColor( ) const;
			RefCountPtr< const Lang::FacetNormalGray > transformed( const Lang::Transform3D & tf ) const;
			TYPEINFODECL;
		};

		class FacetNormalRGB : public NoOperatorOverloadValue
		{
			Concrete::Coords3D position_;
			RefCountPtr< const Lang::SpecularReflection > reflections_;
			Concrete::UnitFloatTriple reflectionUnitNormal_;
			Concrete::RGB lightMultiply_;
			RefCountPtr< const Lang::SpecularReflection > autoScattering_;
			Concrete::RGB autoIntensity_;
		public:
			FacetNormalRGB( const Concrete::Coords3D & position,
											const RefCountPtr< const Lang::SpecularReflection > & reflections,
											const Concrete::UnitFloatTriple & reflectionUnitNormal,
											const Concrete::RGB & lightMultiply,
											const RefCountPtr< const Lang::SpecularReflection > & autoScattering,
											const Concrete::RGB & autoIntensity );
			virtual ~FacetNormalRGB( );
			virtual void gcMark( Kernel::GCMarkedSet & marked );

			Concrete::RGB compute( const Concrete::Coords3D & point, const Concrete::UnitFloatTriple normal, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
			Concrete::RGB compute( const Concrete::Coords3D & point, const Lang::Transform3D & tf, const Concrete::Length eyez, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
			const Concrete::Coords3D & position( ) const { return position_; }
			const Concrete::UnitFloatTriple & normal( ) const { return reflectionUnitNormal_; }
			Concrete::RGB getDebugColor( ) const;
			RefCountPtr< const Lang::FacetNormalRGB > transformed( const Lang::Transform3D & tf ) const;
			TYPEINFODECL;
		};

	}

	namespace Computation
	{
		class ZBufTriangle
		{
		public:
			class ZMap
			{
				Concrete::UnitFloatTriple normal_;

				// The first group of members are used to compute the depth
				double k_x_;
				double k_y_;
				double k_z_;
				Concrete::Length m_;
				Physical< -1, 0 > eyezInv_;

				// To break ties, one shall consult this member:
				Concrete::Length tiebreaker_;

				// The next group of members are used to find plane intersections
				Concrete::Length eyez_;
			public:
				ZMap( const Concrete::UnitFloatTriple & normal,
							Concrete::Length m,
							Concrete::Length tiebreaker,
							Concrete::Length eyez );

				Concrete::Length operator () ( const Concrete::Coords2D & p ) const;

				void writeToMatrices( double a[3], Concrete::Length * b ) const;
				Concrete::Length eyez( ) const { return eyez_; }
				const Concrete::UnitFloatTriple & getNormal( ) const { return normal_; }
				Concrete::Length getM( ) const { return m_; }
				Concrete::Length getTiebreaker( ) const { return tiebreaker_; }
			};

			const Computation::PaintedPolygon3D * painter_;
			std::vector< Concrete::Coords2D > points_;
			RefCountPtr< const ZMap > zMap_;

			std::list< RefCountPtr< const Lang::LightSource > > shadowLights_;

			ZBufTriangle( const Computation::PaintedPolygon3D * painter, const RefCountPtr< const Computation::ZBufTriangle::ZMap > & zMap, const Concrete::Coords2D & p1, const Concrete::Coords2D & p2, const Concrete::Coords2D & p3 );
			Concrete::Length zAt( const Concrete::Coords2D & p ) const;
			bool isOnTopOfAt( const Computation::ZBufTriangle & other, const Concrete::Coords2D & p ) const;
			bool overlaps( const ZBufTriangle & other ) const;
			bool overlaps( const ZBufTriangle & other, Concrete::Coords2D * commonPoint, Concrete::Length tol ) const; // tol shall be positive, and gives the smallest acceptable distance from commonPoint to the intersection boundary.
			bool intersection( const Computation::ZBufTriangle & other, Computation::SplicingLine * line ) const;
			bool overlapsAlong( const ZBufTriangle & other, const Computation::SplicingLine & line, Concrete::Length tol ) const; // tol shall be positive, and gives the smallest acceptable distance from commonPoint to the intersection boundary.
			bool contains( const Concrete::Coords2D & p ) const;
			bool contains( const Concrete::Coords2D & p, Concrete::Length tol ) const;
			static void splice( const ZBufTriangle & tOld, const ZBufTriangle & tNew, std::list< Computation::ZBufTriangle > * oldDisjointTriangles, std::list< Computation::ZBufTriangle > * oldOccludedTriangles, std::list< Computation::ZBufTriangle > * newDisjointTriangles, std::list< Computation::ZBufTriangle > * newOccludedTriangles, std::list< Computation::ZBufTriangle > * triangleQueue );
			std::list< Computation::ZBufTriangle >::iterator spliceAlong( const Computation::SplicingLine & line, std::list< Computation::ZBufTriangle > * dst ) const;
			RefCountPtr< const Lang::ElementaryPath2D > toPath( ) const;
			RefCountPtr< const Lang::Drawable2D > debugFrame( ) const;
			void pushLines( std::vector< Computation::SplicingLine > * dst ) const;
			void pushIntersection( std::vector< Computation::SplicingLine > * dst, const Computation::ZBufTriangle & other ) const;

			void addTriangleConstraints( Concrete::Coords2D llCorner, double * a, double * b ) const; // To be used by overlaps.

			Concrete::Area area( ) const;

		private:
			static bool oneWayOverlap( const std::vector< Concrete::Coords2D > & poly1, const std::vector< Concrete::Coords2D > & poly2 );
			void pushIfUnique( std::vector< Computation::SplicingLine > * dst, const Concrete::Coords3D p0, const Concrete::Coords3D p1, bool isTriangleSide = true ) const;
			bool intersectionLinePoints( const Computation::ZBufTriangle & other, Concrete::Coords3D * p0, Concrete::Coords3D * p1 ) const;
			friend std::ostream & operator << ( std::ostream & os, const Computation::ZBufTriangle & self );
		};

		class ZBufLine
		{
		public:
			class ZMap
			{
				Concrete::Coords3D p0_;
				Concrete::UnitFloatTriple d_;

				// The first group of members are used to compute the depth
				double k_x_;
				double k_y_;
				double k_z_;
				//				Physical< -1, 0 > eyezInv_;

				// The next group of members are used to find plane intersections
				Concrete::Length eyez_;
			public:
				ZMap( Concrete::Coords3D p0, const Concrete::UnitFloatTriple & d,
							Concrete::Length eyez );

				// It is assumed that this funciton is only called with points that actually are on the line.
				Concrete::Length operator () ( const Concrete::Coords2D & p ) const;
				Concrete::Coords3D intersection( const Computation::ZBufTriangle::ZMap & plane ) const;

				Concrete::Length eyez( ) const { return eyez_; }
			};

			const Computation::StrokedLine3D * painter_;
			RefCountPtr< const Bezier::PolyCoeffs< Concrete::Coords2D > > bezierView_;

			Concrete::Coords2D p0_;
			Concrete::Coords2D p1_;
			Concrete::Coords2D d_; // this is just p1_ - p0_
			RefCountPtr< const ZMap > zMap_;

			ZBufLine( const Computation::StrokedLine3D * painter, const RefCountPtr< const Computation::ZBufLine::ZMap > & zMap, const RefCountPtr< const Bezier::PolyCoeffs< Concrete::Coords2D > > & bezierView, const Concrete::Coords2D & p0, const Concrete::Coords2D & p1 );
			Concrete::Length zAt( const Concrete::Coords2D & p ) const;
			bool overlaps( const ZBufTriangle & other ) const;
			bool overlaps( const ZBufLine & other ) const;
			void splice( const ZBufTriangle & triangle, std::list< const Computation::ZBufLine * > * myLines ) const;
			static void splice( const ZBufLine * line1, const ZBufLine * line2 , std::list< const Computation::ZBufLine * > * line1Container, std::list< const Computation::ZBufLine * > * line2Container );
			RefCountPtr< const Lang::PaintedPath2D > stroke2D( ) const;

		protected:
			double intersectionTime( Concrete::Coords2D p0, Concrete::Coords2D p1 ) const;
		};

		class FacetLatticeVertex
		{
		public:
			Concrete::Coords3D p3D_;
			Concrete::Coords2D p2D_;
			size_t i_;

			FacetLatticeVertex( const Concrete::Coords3D & p3D, const Concrete::Length eyez, const size_t i );
			~FacetLatticeVertex( ){ }

			Concrete::Length distanceTo2D( const Computation::FacetLatticeVertex & other ) const
			{
				return p2D_.distanceTo( other.p2D_ );
			}
		};

		class FacetLatticeEdge
		{
			mutable const FacetLatticeEdge * child1_;
			mutable const FacetLatticeEdge * child2_;
		public:
			const Computation::FacetLatticeVertex * p0_;
			const Computation::FacetLatticeVertex * p1_;

			FacetLatticeEdge( const Computation::FacetLatticeVertex * p0, const Computation::FacetLatticeVertex * p1 )
				: child1_( 0 ), child2_( 0 ), p0_( p0 ), p1_( p1 )
			{ }

			void split( const Concrete::Length eyez,
									PtrOwner_back_Access< std::list< const Computation::FacetLatticeEdge * > > * edgeMem,
									PtrOwner_back_Access< std::list< const Computation::FacetLatticeVertex * > > * vertexMem,
									const Computation::FacetLatticeEdge ** child1, const Computation::FacetLatticeEdge ** child2 ) const;

			const FacetLatticeVertex * getOther( const FacetLatticeEdge * e ) const;
			bool sharePoint( const FacetLatticeEdge * e ) const;
			Concrete::Length length2D( ) const
			{
				return p0_->p2D_.distanceTo( p1_->p2D_ );
			}

			const FacetLatticeVertex * midpoint( ) const
			{
				return child1_->p1_;
			}
		};

		class FacetLatticeTriangle
		{
		public:
			const Computation::FacetLatticeEdge * e0_;
			const Computation::FacetLatticeEdge * e1_;
			const Computation::FacetLatticeEdge * e2_;
		private:
			const Computation::FacetLatticeVertex * v0_;
			const Computation::FacetLatticeVertex * v1_;
			const Computation::FacetLatticeVertex * v2_;
		public:

			FacetLatticeTriangle( const Computation::FacetLatticeEdge * e0, const Computation::FacetLatticeEdge * e1, const Computation::FacetLatticeEdge * e2 )
				: e0_( e0 ), e1_( e1 ), e2_( e2 ),
					v0_( e0_->p0_ ), v1_( e0_->p1_ ), v2_( e1_->getOther( e0_ ) )
			{ };
			~FacetLatticeTriangle( )
			{ };

			const FacetLatticeEdge * getOther( const Computation::FacetLatticeEdge * ea, const Computation::FacetLatticeEdge * eb ) const;
			bool fitsResolution( const Concrete::Length resolution, const Concrete::Length eyez,
													 PtrOwner_back_Access< std::list< const Computation::FacetLatticeEdge * > > * edgeMem,
													 PtrOwner_back_Access< std::list< const Computation::FacetLatticeVertex * > > * vertexMem,
													 const Computation::FacetLatticeTriangle ** child1, const Computation::FacetLatticeTriangle ** child2 ) const;
			RefCountPtr< const Lang::Drawable2D > paint( const RefCountPtr< const Computation::FacetInterpolatorGray > & interpolator, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Length eyez ) const;
			RefCountPtr< const Lang::Drawable2D > paint( const RefCountPtr< const Computation::FacetInterpolatorRGB > & interpolator, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Length eyez ) const;
			void getVertexes( const Computation::FacetLatticeVertex ** va, const Computation::FacetLatticeVertex ** vb, const Computation::FacetLatticeVertex ** vc ) const;
			unsigned char extendLattice( const Computation::FacetLatticeVertex ** va, const Computation::FacetLatticeVertex ** vb, const Computation::FacetLatticeVertex ** vc ) const;
			void display2D( std::ostream & os ) const;
			void display3D( std::ostream & os ) const;
		};

		class StrokedLine3D
		{
		protected:
			Concrete::Coords3D p0_;
			Concrete::Coords3D p1_;
			RefCountPtr< const Kernel::GraphicsState > metaState_;
		public:
			StrokedLine3D( const Concrete::Coords3D & p0, const Concrete::Coords3D & p1, const RefCountPtr< const Kernel::GraphicsState > metaState );
			virtual ~StrokedLine3D( );
			virtual void push_zBufLine( const Lang::Transform3D & tf, const Concrete::Length eyez, std::list< const Computation::ZBufLine * > * lineQueue ) const;
			const RefCountPtr< const Kernel::GraphicsState > & getMetaState( ) const { return metaState_; }
		};

		class StrokedSplineSegment3D : public StrokedLine3D
		{
			Concrete::Coords3D p0front_;
			Concrete::Coords3D p1rear_;
		public:
			StrokedSplineSegment3D( const Concrete::Coords3D & p0, const Concrete::Coords3D & p0front, const Concrete::Coords3D & p1rear, const Concrete::Coords3D & p1, const RefCountPtr< const Kernel::GraphicsState > metaState );
			~StrokedSplineSegment3D( );
			virtual void push_zBufLine( const Lang::Transform3D & tf, const Concrete::Length eyez, std::list< const Computation::ZBufLine * > * lineQueue ) const;
		};

		class PaintedPolygon3D
		{
		protected:
			std::list< Concrete::Coords3D > points_;
			// The following two members define the plane in which all points are required to be
			bool singleSided_; // If true, normal_ is the outward normal of the surface of a solid object.	I false, this area has two opposite outward normals.
			Concrete::UnitFloatTriple normal_;
			Concrete::Length m_;
			Concrete::Length tiebreaker_;
		public:
			PaintedPolygon3D( bool singleSided, const Concrete::UnitFloatTriple & normal, Concrete::Length m, Concrete::Length tiebreaker );
			virtual ~PaintedPolygon3D( );
			void pushPoint( const Concrete::Coords3D & p );
			void push_zBufTriangles( const Lang::Transform3D & tf, const Concrete::Length eyez, std::list< Computation::ZBufTriangle > * triangleQueue, bool respectSingleSided = false ) const;

			virtual RefCountPtr< const Lang::PaintedPolygon2D > polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const = 0;
			virtual RefCountPtr< const Lang::Color > getColor( ) const = 0;

			void makeLattice( PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice,
												PtrOwner_back_Access< std::list< const Computation::FacetLatticeEdge * > > * edgeMem,
												PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > * vertexMem,
												const Concrete::Length viewResolution,
												const Concrete::Length eyez, const Lang::Transform3D & tf ) const;

			virtual void gcMark( Kernel::GCMarkedSet & marked ) = 0;
		protected:
			Concrete::Coords3D computeMean( ) const;
		private:
			PaintedPolygon3D( const Computation::PaintedPolygon3D & orig );
		};

	class FacetInterpolatorGray
	{
	public:
		FacetInterpolatorGray( ){ };
		virtual ~FacetInterpolatorGray( );
		virtual RefCountPtr< const Lang::Gray > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const = 0;
		virtual RefCountPtr< const Lang::Gray > getDebugColor( ) const = 0;
		virtual RefCountPtr< const Computation::FacetInterpolatorGray > transformed( const Lang::Transform3D & tf ) const = 0;
		virtual void gcMark( Kernel::GCMarkedSet & marked ) = 0;
	};

		class FacetInterpolatorGray1 : public FacetInterpolatorGray
	{
		RefCountPtr< const Lang::FacetNormalGray > n1_;
	public:
		FacetInterpolatorGray1( const RefCountPtr< const Lang::FacetNormalGray > & n1 )
			: n1_( n1 )
			{ }
		virtual ~FacetInterpolatorGray1( );
		virtual RefCountPtr< const Lang::Gray > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const;
		virtual RefCountPtr< const Lang::Gray > getDebugColor( ) const;
		virtual RefCountPtr< const Computation::FacetInterpolatorGray > transformed( const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class FacetInterpolatorGray2 : public FacetInterpolatorGray
	{
		RefCountPtr< const Lang::FacetNormalGray > n1_;
		RefCountPtr< const Lang::FacetNormalGray > n2_;

		// We define members such that the weight of n2_ at point p is
		//	 ( < t_, p > - m_ ) / d_
		// that is, at least as long as this number is in the range [ 0, 1 ].
		Concrete::Length d_;
		Concrete::UnitFloatTriple t_;
		Concrete::Length m_;

		// We also define members that describe how the normal at n1_ is rotated to obtain the normal at n2_
		double angle_;
		Concrete::UnitFloatTriple rotationDirection_;
	public:
		FacetInterpolatorGray2( const RefCountPtr< const Lang::FacetNormalGray > & n1,
														const RefCountPtr< const Lang::FacetNormalGray > & n2 );
		virtual ~FacetInterpolatorGray2( );
		virtual RefCountPtr< const Lang::Gray > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const;
		virtual RefCountPtr< const Lang::Gray > getDebugColor( ) const;
		virtual RefCountPtr< const Computation::FacetInterpolatorGray > transformed( const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class FacetInterpolatorGray3 : public FacetInterpolatorGray
	{
		RefCountPtr< const Lang::FacetNormalGray > n1_;
		RefCountPtr< const Lang::FacetNormalGray > n2_;
		RefCountPtr< const Lang::FacetNormalGray > n3_;

		// The interpolation here is much less sophisticated compared to the interpolation of two normals.
		// We compute positive, unnormalized weights wuch that each weight is one at the point it belongs to, and vanishes
		// at the line through the other points.	Then we normalize.	This should be subject to change in later versions!
		Concrete::Coords3D p1_;
		Concrete::Coords3D p2_;
		Concrete::Coords3D p3_;
		Concrete::UnitFloatTriple d1_;
		Concrete::UnitFloatTriple d2_;
		Concrete::UnitFloatTriple d3_;
		Concrete::Length l1_;
		Concrete::Length l2_;
		Concrete::Length l3_;
	public:
		FacetInterpolatorGray3( const RefCountPtr< const Lang::FacetNormalGray > & n1,
														const RefCountPtr< const Lang::FacetNormalGray > & n2,
														const RefCountPtr< const Lang::FacetNormalGray > & n3 );
		virtual ~FacetInterpolatorGray3( );
		virtual RefCountPtr< const Lang::Gray > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const;
		virtual RefCountPtr< const Lang::Gray > getDebugColor( ) const;
		virtual RefCountPtr< const Computation::FacetInterpolatorGray > transformed( const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class FacetInterpolatorRGB
	{
	public:
		FacetInterpolatorRGB( ){ };
		virtual ~FacetInterpolatorRGB( );
		virtual RefCountPtr< const Lang::RGB > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const = 0;
		virtual RefCountPtr< const Lang::RGB > getDebugColor( ) const = 0;
		virtual RefCountPtr< const Computation::FacetInterpolatorRGB > transformed( const Lang::Transform3D & tf ) const = 0;
		virtual void gcMark( Kernel::GCMarkedSet & marked ) = 0;
	};

		class FacetInterpolatorRGB1 : public FacetInterpolatorRGB
	{
		RefCountPtr< const Lang::FacetNormalRGB > n1_;
	public:
		FacetInterpolatorRGB1( const RefCountPtr< const Lang::FacetNormalRGB > & n1 )
			: n1_( n1 )
			{ }
		virtual ~FacetInterpolatorRGB1( );
		virtual RefCountPtr< const Lang::RGB > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const;
		virtual RefCountPtr< const Lang::RGB > getDebugColor( ) const;
		virtual RefCountPtr< const Computation::FacetInterpolatorRGB > transformed( const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class FacetInterpolatorRGB2 : public FacetInterpolatorRGB
	{
		RefCountPtr< const Lang::FacetNormalRGB > n1_;
		RefCountPtr< const Lang::FacetNormalRGB > n2_;

		// We define members such that the weight of n2_ at point p is
		//	 ( < t_, p > - m_ ) / d_
		// that is, at least as long as this number is in the range [ 0, 1 ].
		Concrete::Length d_;
		Concrete::UnitFloatTriple t_;
		Concrete::Length m_;

		// We also define members that describe how the normal at n1_ is rotated to obtain the normal at n2_
		double angle_;
		Concrete::UnitFloatTriple rotationDirection_;
	public:
		FacetInterpolatorRGB2( const RefCountPtr< const Lang::FacetNormalRGB > & n1,
													 const RefCountPtr< const Lang::FacetNormalRGB > & n2 );
		virtual ~FacetInterpolatorRGB2( );
		virtual RefCountPtr< const Lang::RGB > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const;
		virtual RefCountPtr< const Lang::RGB > getDebugColor( ) const;
		virtual RefCountPtr< const Computation::FacetInterpolatorRGB > transformed( const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class FacetInterpolatorRGB3 : public FacetInterpolatorRGB
	{
		RefCountPtr< const Lang::FacetNormalRGB > n1_;
		RefCountPtr< const Lang::FacetNormalRGB > n2_;
		RefCountPtr< const Lang::FacetNormalRGB > n3_;

		// The interpolation here is much less sophisticated compared to the interpolation of two normals.
		// We compute positive, unnormalized weights wuch that each weight is one at the point it belongs to, and vanishes
		// at the line through the other points.	Then we normalize.	This should be subject to change in later versions!
		Concrete::Coords3D p1_;
		Concrete::Coords3D p2_;
		Concrete::Coords3D p3_;
		Concrete::UnitFloatTriple d1_;
		Concrete::UnitFloatTriple d2_;
		Concrete::UnitFloatTriple d3_;
		Concrete::Length l1_;
		Concrete::Length l2_;
		Concrete::Length l3_;
	public:
		FacetInterpolatorRGB3( const RefCountPtr< const Lang::FacetNormalRGB > & n1,
													 const RefCountPtr< const Lang::FacetNormalRGB > & n2,
													 const RefCountPtr< const Lang::FacetNormalRGB > & n3 );
		virtual ~FacetInterpolatorRGB3( );
		virtual RefCountPtr< const Lang::RGB > compute( const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, const Concrete::Coords3D & point, const Concrete::Length eyez ) const;
		virtual RefCountPtr< const Lang::RGB > getDebugColor( ) const;
		virtual RefCountPtr< const Computation::FacetInterpolatorRGB > transformed( const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class FilledPolygon3D : public PaintedPolygon3D
	{
		RefCountPtr< const Kernel::GraphicsState > metaState_;
	public:
		FilledPolygon3D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
										 const Concrete::UnitFloatTriple & normal, Concrete::Length m,
										 Concrete::Length tiebreaker );
		virtual ~FilledPolygon3D( );
		virtual RefCountPtr< const Lang::PaintedPolygon2D > polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
		virtual RefCountPtr< const Lang::Color > getColor( ) const;		 // shall only be used for debugging!
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class NullPolygon3D : public PaintedPolygon3D
	{
	public:
		NullPolygon3D( );
		virtual ~NullPolygon3D( );
		virtual RefCountPtr< const Lang::PaintedPolygon2D > polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
		virtual RefCountPtr< const Lang::Color > getColor( ) const;		 // shall only be used for debugging!
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class SingleSidedPolygon3DGray : public PaintedPolygon3D
	{
		RefCountPtr< const Computation::FacetInterpolatorGray > interpolator_;
		Concrete::Length viewResolution_;
		Computation::FacetShadeOrder shadeOrder_;	// legal values: { 0, 1, 2 }
	public:
		SingleSidedPolygon3DGray( const RefCountPtr< const Computation::FacetInterpolatorGray > & interpolator,
															bool singleSided, // Confusingly, this type is sometimes used also for double-sided polygons.
															const Concrete::UnitFloatTriple & polygonUnitNormal,
															Concrete::Length m,
															Concrete::Length tiebreaker,
															Concrete::Length viewResolution,
															Computation::FacetShadeOrder shadeOrder );
		virtual ~SingleSidedPolygon3DGray( );
		virtual RefCountPtr< const Lang::PaintedPolygon2D > polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
		virtual RefCountPtr< const Lang::Color > getColor( ) const;		 // shall only be used for debugging!
		virtual void gcMark( Kernel::GCMarkedSet & marked );

	private:
		RefCountPtr< const Lang::PaintedPolygon2D > simple_polygon_to2D( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
		RefCountPtr< const Lang::PaintedPolygon2D > render0( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const;
		RefCountPtr< const Lang::PaintedPolygon2D > render1( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const;
		RefCountPtr< const Lang::PaintedPolygon2D > render2( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice, const PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > & vertexMem ) const;

	protected:
		static void writePacked( std::ostream & os,
														 const size_t BITS_PER_COORDINATE, const size_t BITS_PER_COMPONENT, const size_t BITS_PER_FLAG,
														 const double x, const double y, const Concrete::Gray & color, const unsigned char flag );
		static void writePackedValue( std::ostream & os, char * rest, size_t * restAvail,
																	unsigned int val, size_t bits );
	};

	class SingleSidedPolygon3DRGB : public PaintedPolygon3D
	{
		RefCountPtr< const Computation::FacetInterpolatorRGB > interpolator_;
		Concrete::Length viewResolution_;
		Computation::FacetShadeOrder shadeOrder_;	// legal values: { 0, 1, 2 }
	public:
		SingleSidedPolygon3DRGB( const RefCountPtr< const Computation::FacetInterpolatorRGB > & interpolator,
														 bool singleSided, // Confusingly, this type is sometimes used also for double-sided polygons.
														 const Concrete::UnitFloatTriple & polygonUnitNormal,
														 Concrete::Length m,
														 Concrete::Length tiebreaker,
														 Concrete::Length viewResolution,
														 Computation::FacetShadeOrder shadeOrder );
		virtual ~SingleSidedPolygon3DRGB( );
		virtual RefCountPtr< const Lang::PaintedPolygon2D > polygon_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
		virtual RefCountPtr< const Lang::Color > getColor( ) const;		 // shall only be used for debugging!
		virtual void gcMark( Kernel::GCMarkedSet & marked );

	private:
		RefCountPtr< const Lang::PaintedPolygon2D > simple_polygon_to2D( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights ) const;
		RefCountPtr< const Lang::PaintedPolygon2D > render0( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const;
		RefCountPtr< const Lang::PaintedPolygon2D > render1( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice ) const;
		RefCountPtr< const Lang::PaintedPolygon2D > render2( const Concrete::Length eyez, const Lang::Transform3D & tf, const std::list< RefCountPtr< const Lang::LightSource > > & lights, PtrOwner_back_Access< std::list< const Computation::FacetLatticeTriangle * > > * lattice, const PtrOwner_back_Access< std::list< const FacetLatticeVertex * > > & vertexMem ) const;

	protected:
		static void writePacked( std::ostream & os,
														 const size_t BITS_PER_COORDINATE, const size_t BITS_PER_COMPONENT, const size_t BITS_PER_FLAG,
														 const double x, const double y, const Concrete::RGB & color, const unsigned char flag );
		static void writePackedValue( std::ostream & os, char * rest, size_t * restAvail,
																	unsigned int val, size_t bits );
	};

	class UndirectedEdge
	{
		size_t low_;
		size_t high_;
	public:
	UndirectedEdge( size_t i0, size_t i1 )
		: low_( i0 ), high_( i1 )
		{
			if( low_ > high_ )
				{
					low_ = i1;
					high_ = i0;
				}
		}
		size_t low( ) const { return low_; }
		size_t high( ) const { return high_; }
		static UndirectedEdge begin( )
		{
			return UndirectedEdge( 0, 0 );
		}
		static UndirectedEdge end( )
		{
			return UndirectedEdge( INT_MAX, INT_MAX );
		}
		void increment( size_t size )
		{
			++high_;
			if( high_ >= size )
				{
					++low_;
					if( low_ >= size )
						{
							low_ = INT_MAX;
							high_ = INT_MAX;
							return;
						}
					high_ = low_;
				}
		}
		bool operator == ( const Computation::UndirectedEdge & other ) const
		{
			return low_ == other.low_ && high_ == other.high_;
		}
		bool operator != ( const Computation::UndirectedEdge & other ) const
		{
			return low_ != other.low_ || high_ != other.high_;
		}
	};
	bool operator < ( const Computation::UndirectedEdge & p1, const Computation::UndirectedEdge & p2 );

	template< class T >
		class UndirectedEdgeMatrix
		{
			size_t size_;
			std::vector< std::vector< T > > mem_;
		public:
			UndirectedEdgeMatrix( size_t size );
			UndirectedEdgeMatrix( size_t size, const T & elem );
			~UndirectedEdgeMatrix( );
			T & operator [] ( const UndirectedEdge & idx );
			const T & operator [] ( const UndirectedEdge & idx ) const;
		};

	}

	namespace Lang
	{

	class Drawable2D : public Lang::Geometric2D
	{
	public:
		Drawable2D( );
		virtual ~Drawable2D( );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const = 0;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const = 0;
		RefCountPtr< const Lang::Transformed2D > typed_transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Drawable2D > & self ) const;
		virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;
		virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		TYPEINFODECL;

		DISPATCHDECL;
	};

	class Group2D : public Lang::Drawable2D
	{
	public:
		Group2D( );
		virtual ~Group2D( );
		TYPEINFODECL;
		virtual bool isNull( ) const = 0;
		virtual RefCountPtr< const Lang::Group2D > removeShallow( Lang::Symbol::KeyType key ) const = 0;
	};

	class GroupPair2D : public Lang::Group2D
	{
		RefCountPtr< const Kernel::GraphicsState > metaState_;
		RefCountPtr< const Lang::Drawable2D > car_;
		RefCountPtr< const Lang::Group2D > cdr_;
	public:
		GroupPair2D( RefCountPtr< const Lang::Drawable2D > car, RefCountPtr< const Lang::Group2D > cdr, const RefCountPtr< const Kernel::GraphicsState > & metaState );
		virtual ~GroupPair2D( );
		virtual bool isNull( ) const;
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::Group2D > removeShallow( Lang::Symbol::KeyType key ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class GroupNull2D : public Lang::Group2D
	{
	public:
		GroupNull2D( );
		virtual ~GroupNull2D( );
		virtual bool isNull( ) const;
		virtual RefCountPtr< const Lang::Group2D > removeShallow( Lang::Symbol::KeyType key ) const;
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
	};

	class XObject : public Lang::Drawable2D
	{
		RefCountPtr< const Kernel::GraphicsState > metaState_;
		RefCountPtr< SimplePDF::PDF_Object > resource_;
		RefCountPtr< const Lang::ElementaryPath2D > mybbox_;
		std::string debugStr_;
	public:
		XObject( const RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox );
		XObject( const RefCountPtr< SimplePDF::PDF_Object > & resource, RefCountPtr< const Lang::ElementaryPath2D > mybbox, const RefCountPtr< const Kernel::GraphicsState > & metaState );
		virtual ~XObject( );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		RefCountPtr< const Lang::XObject > cloneWithState( const RefCountPtr< const Kernel::GraphicsState > & _metaState ) const;
		void setDebugStr( const std::string & _debugStr );
		const std::string & getDebugStr( ) const;
		virtual void show( std::ostream & os ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );

		RefCountPtr< SimplePDF::PDF_Object > getResource( ) const;
	};

	class TransparencyGroup : public Lang::XObject
	{
		RefCountPtr< const Lang::ColorSpace > colorSpace_;
		RefCountPtr< SimplePDF::PDF_Indirect_out > indirection_;
	public:
		TransparencyGroup( const RefCountPtr< SimplePDF::PDF_Indirect_out > & indirection, RefCountPtr< const Lang::ElementaryPath2D > _mybbox, const RefCountPtr< const Lang::ColorSpace > & colorSpace );
		virtual ~TransparencyGroup( );
		RefCountPtr< const Lang::ColorSpace > colorSpace( ) const;
		RefCountPtr< SimplePDF::PDF_Indirect_out > getPDF_Object( ) const;
	};

	class PaintedPath2D : public Lang::Drawable2D
	{
	protected:
		RefCountPtr< const Kernel::GraphicsState > metaState_;
		std::list< RefCountPtr< const Lang::ElementaryPath2D > > path_;
		const char * paintCmd_;
	public:
		PaintedPath2D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
									 const char * paintCmd );
		PaintedPath2D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
									 RefCountPtr< const Lang::ElementaryPath2D > path,
									 const char * paintCmd );
		PaintedPath2D( const RefCountPtr< const Kernel::GraphicsState > & metaState,
									 RefCountPtr< const Lang::MultiPath2D > paths,
									 const char * paintCmd );
		virtual ~PaintedPath2D( );
		TYPEINFODECL;
		void addSubPath( RefCountPtr< const Lang::ElementaryPath2D > subpath );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		bool isContainedIn( const Lang::Clipped2D * clipping ) const;
	};

	class PaintedPolygon2D : public Lang::PaintedPath2D
	{
	public:
		PaintedPolygon2D( RefCountPtr< const Kernel::GraphicsState > metaState, RefCountPtr< const Lang::ElementaryPath2D > path );
		virtual ~PaintedPolygon2D( );
		RefCountPtr< const Lang::Drawable2D > clip( std::list< Computation::ZBufTriangle > * regions, const RefCountPtr< const Lang::PaintedPolygon2D > selfRef ) const;
	};

	class BBoxed2D : public Lang::PaintedPolygon2D
	{
		RefCountPtr< const Lang::ElementaryPath2D > mybbox_;
		RefCountPtr< const Lang::Drawable2D > element_;
	public:
		BBoxed2D( RefCountPtr< const Lang::Drawable2D > element, RefCountPtr< const Lang::ElementaryPath2D > mybbox );
		virtual ~BBoxed2D( );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class Transformed2D : public Lang::Drawable2D
	{
		Lang::Transform2D mytf_;
		RefCountPtr< const Lang::Drawable2D > element_;
	public:
		Transformed2D( RefCountPtr< const Lang::Drawable2D > element, const Lang::Transform2D & mytf );
		virtual ~Transformed2D( );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class Clipped2D : public Lang::Drawable2D
	{
		std::list< RefCountPtr< const Lang::ElementaryPath2D > > clipList_;
		RefCountPtr< const Lang::Drawable2D > element_;
		const char * clipCommand_;
	public:
		Clipped2D( const RefCountPtr< const Lang::Drawable2D > & element, const char * clipCommand );
		virtual ~Clipped2D( );
		void addSubPath( const RefCountPtr< const Lang::ElementaryPath2D > & subpath );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		RefCountPtr< const Lang::Drawable2D > debugPolys( ) const;
		bool isSingleConvexPoly( Concrete::Length tol ) const;
		bool convexPolyContains( const Concrete::Coords2D & p, Concrete::Length tol ) const;
	};

	class SoftMasked2D : public Lang::Drawable2D
	{
		RefCountPtr< const Lang::Drawable2D > element_;
		RefCountPtr< const Lang::SoftMask > mask_;
	public:
		SoftMasked2D( const RefCountPtr< const Lang::Drawable2D > & element, const RefCountPtr< const Lang::SoftMask > & mask );
		virtual ~SoftMasked2D( );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class Drawable3D : public Lang::Geometric3D
	{
	public:
		Drawable3D( );
		virtual ~Drawable3D( );
		RefCountPtr< const Lang::Transformed3D > typed_transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const;
		virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const;
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const = 0;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const = 0;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		TYPEINFODECL;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		DISPATCHDECL;
	};

	class Group3D : public Lang::Drawable3D
	{
	public:
		Group3D( );
		virtual ~Group3D( );
		TYPEINFODECL;
		virtual bool isNull( ) const = 0;
		virtual RefCountPtr< const Lang::Group3D > removeShallow( Lang::Symbol::KeyType key ) const = 0;
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual RefCountPtr< const Lang::Group2D > group_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf ) const = 0;
	};

	class GroupPair3D : public Lang::Group3D
	{
		RefCountPtr< const Kernel::GraphicsState > metaState_;
		RefCountPtr< const Lang::Drawable3D > car_;
		RefCountPtr< const Lang::Group3D > cdr_;
	public:
		GroupPair3D( const RefCountPtr< const Lang::Drawable3D > & car, const RefCountPtr< const Lang::Group3D > & cdr, const RefCountPtr< const Kernel::GraphicsState > & metaState );
		virtual ~GroupPair3D( );
		virtual bool isNull( ) const;
		virtual RefCountPtr< const Lang::Group2D > group_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual RefCountPtr< const Lang::Group3D > removeShallow( Lang::Symbol::KeyType key ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class GroupNull3D : public Lang::Group3D
	{
	public:
		GroupNull3D( );
		virtual ~GroupNull3D( );
		virtual bool isNull( ) const;
		virtual RefCountPtr< const Lang::Group2D > group_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual RefCountPtr< const Lang::Group3D > removeShallow( Lang::Symbol::KeyType key ) const;
	};

	class Drawable2Din3D : public Lang::Drawable3D
	{
		RefCountPtr< const Lang::Drawable2D > element_;
	public:
		Drawable2Din3D( RefCountPtr< const Lang::Drawable2D > element );
		virtual ~Drawable2Din3D( );
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class Facing2Din3D : public Lang::Drawable3D
	{
		RefCountPtr< const Lang::Drawable2D > element_;
		bool scale_;
		bool distort_;
	public:
		Facing2Din3D( RefCountPtr< const Lang::Drawable2D > element, bool scale, bool distort );
		virtual ~Facing2Din3D( );
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class FacingFunction3D : public Lang::Drawable3D
	{
		Kernel::PassedDyn dyn_;
		RefCountPtr< const Lang::Function > generator_;
	public:
		FacingFunction3D( Kernel::PassedDyn dyn, RefCountPtr< const Lang::Function > generator );
		virtual ~FacingFunction3D( );
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class PaintedPath3D : public Lang::Drawable3D
	{
		RefCountPtr< const Kernel::GraphicsState > metaState_;
		std::list< RefCountPtr< const Lang::ElementaryPath3D > > path_;
		const char * paintCmd_;
	protected:
		Concrete::Length tiebreaker_;	// This is used to break ties in the z-buffer.
	public:
		PaintedPath3D( RefCountPtr< const Kernel::GraphicsState > metaState,
									 const char * paintCmd,
									 Concrete::Length tiebreaker = Concrete::ZERO_LENGTH );
		PaintedPath3D( RefCountPtr< const Kernel::GraphicsState > metaState,
									 RefCountPtr< const Lang::ElementaryPath3D > path,
									 const char * paintCmd,
									 Concrete::Length tiebreaker = Concrete::ZERO_LENGTH );
		PaintedPath3D( RefCountPtr< const Kernel::GraphicsState > metaState,
									 RefCountPtr< const Lang::MultiPath3D > paths,
									 const char * paintCmd,
									 Concrete::Length tiebreaker = Concrete::ZERO_LENGTH );
		virtual ~PaintedPath3D( );
		TYPEINFODECL;
		void addSubPath( RefCountPtr< const Lang::ElementaryPath3D > subpath );
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class SingleSided3DGray : public Lang::Drawable3D
	{
		// This is a historical note worth to remember.
		// It doesn't make sense to store Computation::PaintedPolygon3D in a Lang:: object, since a Computation::PaintedPolygon3D cannot be transformed
		// but must be created at its final position, while a Lang:: object can be transformed.
		// In a class like this, one can only store stuff that enables us to create the Computation:: object later when the Transform3D is known.

		RefCountPtr< const Lang::ElementaryPath3D > points_;
		RefCountPtr< const Computation::FacetInterpolatorGray > interpolator_;
		bool singleSided_;
		Concrete::UnitFloatTriple polygonUnitNormal_;
		Concrete::Length m_;
		Concrete::Length tiebreaker_;	// This is used to break ties in the z-buffer.
		Concrete::Length viewResolution_;
		Computation::FacetShadeOrder shadeOrder_;
	public:
		SingleSided3DGray( const RefCountPtr< const Lang::ElementaryPath3D > & points,
											 const RefCountPtr< const Computation::FacetInterpolatorGray > & interpolator,
											 bool singleSided,	// Confusing, but true; this type is curretly sometimes used also for double-sided objects.
											 const Concrete::UnitFloatTriple & polygonUnitNormal,
											 Concrete::Length m,
											 Concrete::Length tiebreaker,
											 Concrete::Length viewResolution,
											 Computation::FacetShadeOrder shadeOrder );
		virtual ~SingleSided3DGray( );
		TYPEINFODECL;
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class SingleSided3DRGB : public Lang::Drawable3D
	{
		// This is a historical note worth to remember.
		// It doesn't make sense to store Computation::PaintedPolygon3D in a Lang:: object, since a Computation::PaintedPolygon3D cannot be transformed
		// but must be created at its final position, while a Lang:: object can be transformed.
		// In a class like this, one can only store stuff that enables us to create the Computation:: object later when the Transform3D is known.

		RefCountPtr< const Lang::ElementaryPath3D > points_;
		RefCountPtr< const Computation::FacetInterpolatorRGB > interpolator_;
		bool singleSided_;
		Concrete::UnitFloatTriple polygonUnitNormal_;
		Concrete::Length m_;
		Concrete::Length tiebreaker_;	// This is used to break ties in the z-buffer.
		Concrete::Length viewResolution_;
		Computation::FacetShadeOrder shadeOrder_;
	public:
		SingleSided3DRGB( const RefCountPtr< const Lang::ElementaryPath3D > & points,
											 const RefCountPtr< const Computation::FacetInterpolatorRGB > & interpolator,
											 bool singleSided,	// Confusing, but true; this type is curretly sometimes used also for double-sided objects.
											 const Concrete::UnitFloatTriple & polygonUnitNormal,
											 Concrete::Length m,
											 Concrete::Length tiebreaker,
											 Concrete::Length viewResolution,
											 Computation::FacetShadeOrder shadeOrder );
		virtual ~SingleSided3DRGB( );
		TYPEINFODECL;
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class ZBuf : public Lang::Drawable3D
	{
		const RefCountPtr< const Kernel::GraphicsState > metaState_;
		const RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > pile_;
		const RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > linePile_;
		const RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > lightPile_;
	public:
		ZBuf( const RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > & pile, const RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > & linePile, const RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > & lightPile, const RefCountPtr< const Kernel::GraphicsState > & metaState );
		virtual ~ZBuf( );
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
		typedef std::list< size_t > PolyIndices;
		class IndexRepresentation;
	private:
		static void mergeTriangles( std::list< Computation::ZBufTriangle > * triangles );
		static void recombineTriangles( std::list< Computation::ZBufTriangle > * mergedTriangles );

		// The return value is false if the edge was not added due to a conflict.
		static bool addEdge( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< IndexRepresentation * > > > * edgeMarix, const Computation::UndirectedEdge & edge, IndexRepresentation * indRep, std::list< Computation::UndirectedEdge > * jobQueue, const std::vector< Concrete::Coords2D > & cornerMem );

		static void removeEdge( Computation::UndirectedEdgeMatrix< std::pair< bool, std::list< IndexRepresentation * > > > * edgeMarix, const Computation::UndirectedEdge & edge, IndexRepresentation * indRep );
		static bool areAligned( const Concrete::Coords2D p0, const Concrete::Coords2D p1, const Concrete::Coords2D p2 );
		static bool areAlignedAndOrdered( const Concrete::Coords2D p0, const Concrete::Coords2D p1, const Concrete::Coords2D p2 );
		static void addEdges( PolyIndices * poly, Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > * edgeMatrix );
		static void addEdge( Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > * edgeMatrix, size_t ia, size_t ib, PolyIndices * indRep );
		static bool extendPoly( PolyIndices * poly, const Computation::UndirectedEdge & edge, size_t iNew, Computation::UndirectedEdgeMatrix< std::list< PolyIndices * > > * edgeMatrix, const std::vector< Concrete::Coords2D > & cornerMem, bool convex );
	public:
		static void trianglesToPolys( std::list< Computation::ZBufTriangle > * triangles, Lang::Clipped2D * dst );
	};

		// The ZSorter class is very similar to ZBuf, but uses another algorithm to display itself
	class ZSorter : public Lang::Drawable3D
	{
		const RefCountPtr< const Kernel::GraphicsState > metaState_;
		const RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > pile_;
		const RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > linePile_;
		const RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > lightPile_;
	public:
		ZSorter( const RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > & pile, const RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > & linePile, const RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > & lightPile, const RefCountPtr< const Kernel::GraphicsState > & metaState );
		virtual ~ZSorter( );
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class Transformed3D : public Lang::Drawable3D
	{
		Lang::Transform3D mytf_;
		RefCountPtr< const Lang::Drawable3D > element_;
	public:
		Transformed3D( RefCountPtr< const Lang::Drawable3D > element, const Lang::Transform3D & mytf );
		virtual ~Transformed3D( );
		virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
		virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	class Text : public Lang::Drawable2D
	{
		RefCountPtr< const Kernel::GraphicsState > metaState_;
		RefCountPtr< const Kernel::TextState > textState_;
		RefCountPtr< const std::list< RefCountPtr< const Lang::TextOperation > > > elements_;
		RefCountPtr< const Lang::ElementaryPath2D > mybbox_;
	public:
		Text( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Kernel::TextState > & textState, const RefCountPtr< const std::list< RefCountPtr< const Lang::TextOperation > > > & elements, const RefCountPtr< const Lang::ElementaryPath2D > & mybbox );
		virtual ~Text( );
		virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
		virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
		virtual void gcMark( Kernel::GCMarkedSet & marked );
	};

	}

	namespace Helpers
	{

	RefCountPtr< const Lang::Group2D >
		newGroup2D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 );
	RefCountPtr< const Lang::Group2D >
		newGroup2D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable2D > & obj3, const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 );

	RefCountPtr< const Lang::Group3D >
		newGroup3D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable3D > & obj2, const RefCountPtr< const Lang::Drawable3D > & obj1 );
	RefCountPtr< const Lang::Group3D >
		newGroup3D( const RefCountPtr< const Kernel::GraphicsState > & metaState, const RefCountPtr< const Lang::Drawable3D > & obj3, const RefCountPtr< const Lang::Drawable3D > & obj2, const RefCountPtr< const Lang::Drawable3D > & obj1 );


	RefCountPtr< const Lang::TransparencyGroup >
		newTransparencyGroup( const RefCountPtr< const Lang::Group2D > & content, bool isolated, bool knockout, const RefCountPtr< const Lang::ColorSpace > & blendSpace );

	RefCountPtr< const Lang::TransparencyGroup >
		newSolidTransparencyGroup( const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 );
	RefCountPtr< const Lang::TransparencyGroup >
		newSolidTransparencyGroup( const RefCountPtr< const Lang::Drawable2D > & obj3, const RefCountPtr< const Lang::Drawable2D > & obj2, const RefCountPtr< const Lang::Drawable2D > & obj1 );

	}

}


template< class T >
::Shapes::Computation::UndirectedEdgeMatrix< T >::UndirectedEdgeMatrix( size_t size )
: size_( size )
{
	mem_.reserve( size_ );
	for( size_t row = 0; row < size_; ++row )
		{
			mem_.push_back( std::vector< T >( ) );
			mem_.back( ).resize( size_ - row );
		}
}

template< class T >
::Shapes::Computation::UndirectedEdgeMatrix< T >::UndirectedEdgeMatrix( size_t size, const T & elem )
: size_( size )
{
	mem_.reserve( size_ );
	for( size_t row = 0; row < size_; ++row )
		{
			mem_.push_back( std::vector< T >( ) );
			mem_.back( ).resize( size_ - row, elem );
		}
}

template< class T >
::Shapes::Computation::UndirectedEdgeMatrix< T >::~UndirectedEdgeMatrix( )
{ }

template< class T >
T &
::Shapes::Computation::UndirectedEdgeMatrix< T >::operator [] ( const UndirectedEdge & idx )
{
	size_t low = idx.low( );
	return mem_[ low ][ idx.high( ) - low ];
}

template< class T >
const T &
::Shapes::Computation::UndirectedEdgeMatrix< T >::operator [] ( const UndirectedEdge & idx ) const
{
	size_t low = idx.low( );
	return mem_[ low ][ idx.high( ) - low ];
}

#endif
