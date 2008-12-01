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

#define CLASSTREE2_ROOT( Ma, S, Mb )								\
	CLASSTREE2_NoOperatorOverloadValue( Ma, S, Mb )																\
	CLASSTREE2_NoOperatorOverloadGeometric2D( Ma, S, Mb )																\
	CLASSTREE2_Drawable2D( Ma, S, Mb )																\
	CLASSTREE2_Drawable3D( Ma, S, Mb )																\
	CLASSTREE2_Symbol( Ma, S, Mb )																\
	CLASSTREE2_Float( Ma, S, Mb )																\
	CLASSTREE2_Integer( Ma, S, Mb )																\
	CLASSTREE2_Length( Ma, S, Mb )																\
	CLASSTREE2_Boolean( Ma, S, Mb )																\
	CLASSTREE2_String( Ma, S, Mb )																\
	CLASSTREE2_FloatPair( Ma, S, Mb )																\
	CLASSTREE2_FloatTriple( Ma, S, Mb )																\
	CLASSTREE2_Coords2D( Ma, S, Mb )																\
	CLASSTREE2_Coords3D( Ma, S, Mb )																\
	CLASSTREE2_PolarHandleBase( Ma, S, Mb )																\
	CLASSTREE2_PathPoint2D( Ma, S, Mb )																\
	CLASSTREE2_PathPoint3D( Ma, S, Mb )																\
	CLASSTREE2_Path2D( Ma, S, Mb )																\
	CLASSTREE2_Path3D( Ma, S, Mb )																\
	CLASSTREE2_MultiPath2D( Ma, S, Mb )																\
	CLASSTREE2_MultiPath3D( Ma, S, Mb )																\
	CLASSTREE2_PathSlider2D( Ma, S, Mb )																\
	CLASSTREE2_PathSlider3D( Ma, S, Mb )																\
	CLASSTREE2_Color( Ma, S, Mb )																\
	CLASSTREE2_Dash( Ma, S, Mb )																\
	CLASSTREE2_Transform2D( Ma, S, Mb )																\
	CLASSTREE2_Transform3D( Ma, S, Mb )																\
	CLASSTREE2_Function( Ma, S, Mb )																\
	CLASSTREE2_Instance( Ma, S, Mb )												\
	CLASSTREE2_Class( Ma, S, Mb )															 \
	CLASSTREE2_LightSource( Ma, S, Mb )																\
	CLASSTREE2_LightGroup( Ma, S, Mb )																\
	CLASSTREE2_SpecularReflection( Ma, S, Mb )																\
	CLASSTREE2_DynamicBindings( Ma, S, Mb )
#define CLASSTREE2_NoOperatorOverloadValue( Ma, S, Mb ) Ma( S, Mb, NoOperatorOverloadValue )
#define CLASSTREE2_NoOperatorOverloadGeometric2D( Ma, S, Mb ) Ma( S, Mb, NoOperatorOverloadGeometric2D )
#define CLASSTREE2_Drawable2D( Ma, S, Mb ) Ma( S, Mb, Drawable2D )
#define CLASSTREE2_Drawable3D( Ma, S, Mb ) Ma( S, Mb, Drawable3D )
#define CLASSTREE2_Symbol( Ma, S, Mb ) Ma( S, Mb, Symbol )
#define CLASSTREE2_Float( Ma, S, Mb ) Ma( S, Mb, Float )
#define CLASSTREE2_Integer( Ma, S, Mb ) Ma( S, Mb, Integer )
#define CLASSTREE2_Length( Ma, S, Mb ) Ma( S, Mb, Length )
#define CLASSTREE2_Boolean( Ma, S, Mb ) Ma( S, Mb, Boolean )
#define CLASSTREE2_String( Ma, S, Mb ) Ma( S, Mb, String )
#define CLASSTREE2_FloatPair( Ma, S, Mb ) Ma( S, Mb, FloatPair )
#define CLASSTREE2_FloatTriple( Ma, S, Mb ) Ma( S, Mb, FloatTriple )
#define CLASSTREE2_Coords2D( Ma, S, Mb ) Ma( S, Mb, Coords2D ) \
			 CLASSTREE2_CornerCoords2D( Ma, S, Mb )
#define CLASSTREE2_Coords3D( Ma, S, Mb ) Ma( S, Mb, Coords3D )
#define CLASSTREE2_CornerCoords2D( Ma, S, Mb ) Ma( S, Mb, CornerCoords2D )
#define CLASSTREE2_PolarHandleBase( Ma, S, Mb ) Ma( S, Mb, PolarHandleBase )
#define CLASSTREE2_PolarHandle2D( Ma, S, Mb ) Ma( S, Mb, PolarHandle2D )
#define CLASSTREE2_PolarHandle2DFree_a( Ma, S, Mb ) Ma( S, Mb, PolarHandle2DFree_a )
#define CLASSTREE2_PolarHandle2DFree_r( Ma, S, Mb ) Ma( S, Mb, PolarHandle2DFree_r )
#define CLASSTREE2_PolarHandle2DFree_ra( Ma, S, Mb ) Ma( S, Mb, PolarHandle2DFree_ra )
#define CLASSTREE2_PathPoint2D( Ma, S, Mb ) Ma( S, Mb, PathPoint2D )
#define CLASSTREE2_PathPoint3D( Ma, S, Mb ) Ma( S, Mb, PathPoint3D )
#define CLASSTREE2_Path2D( Ma, S, Mb ) Ma( S, Mb, Path2D )
#define CLASSTREE2_Path3D( Ma, S, Mb ) Ma( S, Mb, Path3D )
#define CLASSTREE2_MultiPath2D( Ma, S, Mb ) Ma( S, Mb, MultiPath2D )
#define CLASSTREE2_MultiPath3D( Ma, S, Mb ) Ma( S, Mb, MultiPath3D )
#define CLASSTREE2_PathSlider2D( Ma, S, Mb ) Ma( S, Mb, PathSlider2D )
#define CLASSTREE2_PathSlider3D( Ma, S, Mb ) Ma( S, Mb, PathSlider3D )
#define CLASSTREE2_Color( Ma, S, Mb ) Ma( S, Mb, Color )				\
			 CLASSTREE2_Gray( Ma, S, Mb )																\
			 CLASSTREE2_RGB( Ma, S, Mb )																\
			 CLASSTREE2_CMYK( Ma, S, Mb )
#define CLASSTREE2_Gray( Ma, S, Mb ) Ma( S, Mb, Gray )
#define CLASSTREE2_RGB( Ma, S, Mb ) Ma( S, Mb, RGB )
#define CLASSTREE2_CMYK( Ma, S, Mb ) Ma( S, Mb, CMYK )
#define CLASSTREE2_Dash( Ma, S, Mb ) Ma( S, Mb, Dash )
#define CLASSTREE2_Transform2D( Ma, S, Mb ) Ma( S, Mb, Transform2D )
#define CLASSTREE2_Transform3D( Ma, S, Mb ) Ma( S, Mb, Transform3D )
#define CLASSTREE2_Function( Ma, S, Mb ) Ma( S, Mb, Function )
#define CLASSTREE2_Instance( Ma, S, Mb ) Ma( S, Mb, Instance )
#define CLASSTREE2_Class( Ma, S, Mb ) Ma( S, Mb, Class )
#define CLASSTREE2_LightSource( Ma, S, Mb ) Ma( S, Mb, LightSource )
#define CLASSTREE2_LightGroup( Ma, S, Mb ) Ma( S, Mb, LightGroup )
#define CLASSTREE2_SpecularReflection( Ma, S, Mb ) Ma( S, Mb, SpecularReflection )
#define CLASSTREE2_DynamicBindings( Ma, S, Mb ) Ma( S, Mb, DynamicBindings )
#define CLASSTREE2_SingleDynamicBinding( Ma, S, Mb ) Ma( S, Mb, SingleDynamicBinding )
#define CLASSTREE2_DynamicBindingsPair( Ma, S, Mb ) Ma( S, Mb, DynamicBindingsPair )
