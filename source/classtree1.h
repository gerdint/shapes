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

#define CLASSTREE1_ROOT( Ma, S, Mb )								\
	CLASSTREE1_NoOperatorOverloadValue( Ma, S, Mb )																\
	CLASSTREE1_NoOperatorOverloadGeometric2D( Ma, S, Mb )																\
	CLASSTREE1_Drawable2D( Ma, S, Mb )																\
	CLASSTREE1_Drawable3D( Ma, S, Mb )																\
	CLASSTREE1_Symbol( Ma, S, Mb )																\
	CLASSTREE1_Float( Ma, S, Mb )																\
	CLASSTREE1_Integer( Ma, S, Mb )																\
	CLASSTREE1_Length( Ma, S, Mb )																\
	CLASSTREE1_Boolean( Ma, S, Mb )																\
	CLASSTREE1_String( Ma, S, Mb )																\
	CLASSTREE1_FloatPair( Ma, S, Mb )																\
	CLASSTREE1_FloatTriple( Ma, S, Mb )																\
	CLASSTREE1_Coords2D( Ma, S, Mb )																\
	CLASSTREE1_Coords3D( Ma, S, Mb )																\
	CLASSTREE1_PolarHandleBase( Ma, S, Mb )																\
	CLASSTREE1_PathPoint2D( Ma, S, Mb )																\
	CLASSTREE1_PathPoint3D( Ma, S, Mb )																\
	CLASSTREE1_Path2D( Ma, S, Mb )																\
	CLASSTREE1_Path3D( Ma, S, Mb )																\
	CLASSTREE1_MultiPath2D( Ma, S, Mb )																\
	CLASSTREE1_MultiPath3D( Ma, S, Mb )																\
	CLASSTREE1_PathSlider2D( Ma, S, Mb )																\
	CLASSTREE1_PathSlider3D( Ma, S, Mb )																\
	CLASSTREE1_Color( Ma, S, Mb )																\
	CLASSTREE1_Dash( Ma, S, Mb )																\
	CLASSTREE1_Transform2D( Ma, S, Mb )																\
	CLASSTREE1_Transform3D( Ma, S, Mb )																\
	CLASSTREE1_Function( Ma, S, Mb )																\
	CLASSTREE1_Instance( Ma, S, Mb )												\
	CLASSTREE1_Class( Ma, S, Mb )															 \
	CLASSTREE1_LightSource( Ma, S, Mb )																\
	CLASSTREE1_LightGroup( Ma, S, Mb )																\
	CLASSTREE1_SpecularReflection( Ma, S, Mb )																\
	CLASSTREE1_DynamicBindings( Ma, S, Mb )
#define CLASSTREE1_NoOperatorOverloadValue( Ma, S, Mb ) Ma( S, Mb, NoOperatorOverloadValue )
#define CLASSTREE1_NoOperatorOverloadGeometric2D( Ma, S, Mb ) Ma( S, Mb, NoOperatorOverloadGeometric2D )
#define CLASSTREE1_Drawable2D( Ma, S, Mb ) Ma( S, Mb, Drawable2D )
#define CLASSTREE1_Drawable3D( Ma, S, Mb ) Ma( S, Mb, Drawable3D )
#define CLASSTREE1_Symbol( Ma, S, Mb ) Ma( S, Mb, Symbol )
#define CLASSTREE1_Float( Ma, S, Mb ) Ma( S, Mb, Float )
#define CLASSTREE1_Integer( Ma, S, Mb ) Ma( S, Mb, Integer )
#define CLASSTREE1_Length( Ma, S, Mb ) Ma( S, Mb, Length )
#define CLASSTREE1_Boolean( Ma, S, Mb ) Ma( S, Mb, Boolean )
#define CLASSTREE1_String( Ma, S, Mb ) Ma( S, Mb, String )
#define CLASSTREE1_FloatPair( Ma, S, Mb ) Ma( S, Mb, FloatPair )
#define CLASSTREE1_FloatTriple( Ma, S, Mb ) Ma( S, Mb, FloatTriple )
#define CLASSTREE1_Coords2D( Ma, S, Mb ) Ma( S, Mb, Coords2D ) \
			 CLASSTREE1_CornerCoords2D( Ma, S, Mb )
#define CLASSTREE1_Coords3D( Ma, S, Mb ) Ma( S, Mb, Coords3D )
#define CLASSTREE1_CornerCoords2D( Ma, S, Mb ) Ma( S, Mb, CornerCoords2D )
#define CLASSTREE1_PolarHandleBase( Ma, S, Mb ) Ma( S, Mb, PolarHandleBase )
#define CLASSTREE1_PolarHandle2D( Ma, S, Mb ) Ma( S, Mb, PolarHandle2D )
#define CLASSTREE1_PolarHandle2DFree_a( Ma, S, Mb ) Ma( S, Mb, PolarHandle2DFree_a )
#define CLASSTREE1_PolarHandle2DFree_r( Ma, S, Mb ) Ma( S, Mb, PolarHandle2DFree_r )
#define CLASSTREE1_PolarHandle2DFree_ra( Ma, S, Mb ) Ma( S, Mb, PolarHandle2DFree_ra )
#define CLASSTREE1_PathPoint2D( Ma, S, Mb ) Ma( S, Mb, PathPoint2D )
#define CLASSTREE1_PathPoint3D( Ma, S, Mb ) Ma( S, Mb, PathPoint3D )
#define CLASSTREE1_Path2D( Ma, S, Mb ) Ma( S, Mb, Path2D )
#define CLASSTREE1_Path3D( Ma, S, Mb ) Ma( S, Mb, Path3D )
#define CLASSTREE1_MultiPath2D( Ma, S, Mb ) Ma( S, Mb, MultiPath2D )
#define CLASSTREE1_MultiPath3D( Ma, S, Mb ) Ma( S, Mb, MultiPath3D )
#define CLASSTREE1_PathSlider2D( Ma, S, Mb ) Ma( S, Mb, PathSlider2D )
#define CLASSTREE1_PathSlider3D( Ma, S, Mb ) Ma( S, Mb, PathSlider3D )
#define CLASSTREE1_Color( Ma, S, Mb ) Ma( S, Mb, Color )				\
			 CLASSTREE1_Gray( Ma, S, Mb )																\
			 CLASSTREE1_RGB( Ma, S, Mb )																\
			 CLASSTREE1_CMYK( Ma, S, Mb )
#define CLASSTREE1_Gray( Ma, S, Mb ) Ma( S, Mb, Gray )
#define CLASSTREE1_RGB( Ma, S, Mb ) Ma( S, Mb, RGB )
#define CLASSTREE1_CMYK( Ma, S, Mb ) Ma( S, Mb, CMYK )
#define CLASSTREE1_Dash( Ma, S, Mb ) Ma( S, Mb, Dash )
#define CLASSTREE1_Transform2D( Ma, S, Mb ) Ma( S, Mb, Transform2D )
#define CLASSTREE1_Transform3D( Ma, S, Mb ) Ma( S, Mb, Transform3D )
#define CLASSTREE1_Function( Ma, S, Mb ) Ma( S, Mb, Function )
#define CLASSTREE1_Instance( Ma, S, Mb ) Ma( S, Mb, Instance )
#define CLASSTREE1_Class( Ma, S, Mb ) Ma( S, Mb, Class )
#define CLASSTREE1_LightSource( Ma, S, Mb ) Ma( S, Mb, LightSource )
#define CLASSTREE1_LightGroup( Ma, S, Mb ) Ma( S, Mb, LightGroup )
#define CLASSTREE1_SpecularReflection( Ma, S, Mb ) Ma( S, Mb, SpecularReflection )
#define CLASSTREE1_DynamicBindings( Ma, S, Mb ) Ma( S, Mb, DynamicBindings )
#define CLASSTREE1_SingleDynamicBinding( Ma, S, Mb ) Ma( S, Mb, SingleDynamicBinding )
#define CLASSTREE1_DynamicBindingsPair( Ma, S, Mb ) Ma( S, Mb, DynamicBindingsPair )
