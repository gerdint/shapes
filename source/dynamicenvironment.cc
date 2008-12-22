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

#include <cmath>

#include "Shapes_Helpers_decls.h"

#include "dynamicenvironment.h"
#include "hottypes.h"
#include "globals.h"
#include "shapescore.h"
#include "ast.h"
#include "isnan.h"

#include <limits>

using namespace Shapes;


void
Kernel::SpecialUnitVariables::specialUnitService( Concrete::Length * d, double * a0, double * a1 )
{
	if( p0_ == 0 || p1_ == 0 )
		{
			throw Exceptions::InternalError( "The path points were not setup before calling specialUnitService" );
		}
	Concrete::Length x0;
	Concrete::Length y0;
	Concrete::Length x3;
	Concrete::Length y3;
	double ag0; /* global angles that will later be compared with aRef */
	double ag1;

	if( reverseDirection_ )
		{
			x0 = p1_->mid_->x_;
			y0 = p1_->mid_->y_;
			x3 = p0_->mid_->x_;
			y3 = p0_->mid_->y_;
			ag0 = p1_->rearAngle_;
			ag1 = p0_->frontAngle_;
		}
	else
		{
			x0 = p0_->mid_->x_;
			y0 = p0_->mid_->y_;
			x3 = p1_->mid_->x_;
			y3 = p1_->mid_->y_;
			ag0 = p0_->frontAngle_;
			ag1 = p1_->rearAngle_;
		}

	Concrete::Length dx = x3 - x0;
	Concrete::Length dy = y3 - y0;

	*d = hypotPhysical( dx, dy );

	double aRef = atan2( dy.offtype< 1, 0 >( ), dx.offtype< 1, 0 >( ) );	/* Angles will be relative to the angle pointing to the other mid point */

	if( p0_->frontState_ & Concrete::PathPoint2D::FREE_ANGLE )
		{
			throw Exceptions::InternalError( "Found a free angle in specialUnitService" );
		}

	if( p1_->rearState_ & Concrete::PathPoint2D::FREE_ANGLE )
		{
			throw Exceptions::InternalError( "Found a free angle in specialUnitService" );
		}

	double ar0 = aRef - ag0;
	if( ar0 < - M_PI )
		{
			do
				{
					ar0 += 2 * M_PI;
				}
			while( ar0 < - M_PI );
		}
	else
		{
			while( ar0 > M_PI )
				{
					ar0 -= 2 * M_PI;
				}
		}

	double ar1 = ag1 - aRef - M_PI;
	if( ar1 < - M_PI )
		{
			do
				{
					ar1 += 2 * M_PI;
				}
			while( ar1 < - M_PI );
		}
	else
		{
			while( ar1 > M_PI )
				{
					ar1 -= 2 * M_PI;
				}
		}

	if( ar0 < 0 )
		{
			ar0 = - ar0;
			ar1 = - ar1;
		}

	ar1 -= ar0;

	if( ar1 < - M_PI )
		{
			ar1 += 2 * M_PI;
		}
	else if( ar1 > M_PI )
		{
			ar1 -= 2 * M_PI;
		}

	*a0 = ar0;
	*a1 = ar1;
}

Kernel::SystemDynamicVariables::SystemDynamicVariables( )
	: graphicsState_( NullPtr< const Kernel::GraphicsState >( ) ),
		eyez_( std::numeric_limits< double >::signaling_NaN( ) ),
		TeX_bleed_( std::numeric_limits< double >::signaling_NaN( ) ),
		defaultUnit_( NullPtr< const Kernel::PolarHandlePromise >( ) ),
		blendSpace_( NullPtr< const Lang::ColorSpace >( ) )
{ }

Kernel::SystemDynamicVariables::SystemDynamicVariables( const RefCountPtr< const Kernel::GraphicsState > & graphicsState )
	: graphicsState_( graphicsState ),
		facetState_( true ),
		textState_( true ),
		eyez_( 50 * 72 / 2.54 ), /* 50 cm */
		TeX_bleed_( 0.5 ), /* 0.5 bp */
		defaultUnit_( new Kernel::PolarHandleEmptyPromise( ) ),
		blendSpace_( Lang::THE_INHERITED_COLOR_SPACE )
{ }

void
Kernel::SystemDynamicVariables::addFrom( const SystemDynamicVariables & other )
{
	if( graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
		{
			graphicsState_ = other.graphicsState_;
		}
	else if( other.graphicsState_ != NullPtr< const Kernel::GraphicsState >( ) )
		{
			graphicsState_ = RefCountPtr< const Kernel::GraphicsState >( new Kernel::GraphicsState( *graphicsState_, *other.graphicsState_ ) );
		}
	/* In the remaining situation, there is nothing to merge, so we're done. */

	if( facetState_ == NullPtr< const Kernel::FacetState >( ) )
		{
			facetState_ = other.facetState_;
		}
	else if( other.facetState_ != NullPtr< const Kernel::FacetState >( ) )
		{
			facetState_ = RefCountPtr< const Kernel::FacetState >( new Kernel::FacetState( *facetState_, *other.facetState_ ) );
		}
	/* In the remaining situation, there is nothing to merge, so we're done. */

	if( textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			textState_ = other.textState_;
		}
	else if( other.textState_ != NullPtr< const Kernel::TextState >( ) )
		{
			textState_ = RefCountPtr< const Kernel::TextState >( new Kernel::TextState( *textState_, *other.textState_ ) );
		}
	/* In the remaining situation, there is nothing to merge, so we're done. */

	if( IS_NAN( eyez_ ) )
		{
			eyez_ = other.eyez_;
		}

	if( IS_NAN( TeX_bleed_ ) )
		{
			TeX_bleed_ = other.TeX_bleed_;
		}

	if( defaultUnit_ == NullPtr< const Kernel::PolarHandlePromise >( ) )
		{
			defaultUnit_ = other.defaultUnit_;
		}

	if( blendSpace_ == NullPtr< const Lang::ColorSpace >( ) )
		{
			blendSpace_ = other.blendSpace_;
		}
}

Kernel::DynamicEnvironment::DynamicEnvironment( const RefCountPtr< const Kernel::GraphicsState > & graphicsState )
	: parent_( NullPtr< Kernel::DynamicEnvironment >( ) ), sysBindings_( new Kernel::SystemDynamicVariables( graphicsState ) ), specialBindings_( 0 ),
		contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{ }

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const Lang::DynamicBindings & bindings )
	: parent_( NullPtr< Kernel::DynamicEnvironment >( ) ), sysBindings_( 0 ), specialBindings_( 0 ),
		contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{
	bindings.bind( bindings_, & sysBindings_ );
	if( sysBindings_ != 0 &&
			sysBindings_->graphicsState_ != NullPtr< const Kernel::GraphicsState >( ) )
		{
			sysBindings_->graphicsState_ =
				RefCountPtr< const Kernel::GraphicsState >( new Kernel::GraphicsState( *(sysBindings_->graphicsState_),
																																							 *(parent->getGraphicsState( )) ) );
		}
	if( sysBindings_ != 0 &&
			sysBindings_->facetState_ != NullPtr< const Kernel::FacetState >( ) )
		{
			sysBindings_->facetState_ =
				RefCountPtr< const Kernel::FacetState >( new Kernel::FacetState( *(sysBindings_->facetState_),
																																				 *(parent->getFacetState( )) ) );
		}
	if( sysBindings_ != 0 &&
			sysBindings_->textState_ != NullPtr< const Kernel::TextState >( ) )
		{
			sysBindings_->textState_ =
				RefCountPtr< const Kernel::TextState >( new Kernel::TextState( *(sysBindings_->textState_),
																																			 *(parent->getTextState( )) ) );
		}
	parent_ = parent->selectParent( parent, bindings_ );
}

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const RefCountPtr< const Kernel::GraphicsState > & graphicsState )
	: parent_( parent ), sysBindings_( new Kernel::SystemDynamicVariables( ) ), specialBindings_( 0 ),
		contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{
	sysBindings_->graphicsState_ = graphicsState;
}

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, Kernel::SystemDynamicVariables * sysBindings )
	: parent_( parent ), sysBindings_( sysBindings ), specialBindings_( 0 ),
		contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{ }

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, Kernel::SpecialUnitVariables * specialBindings )
	: parent_( parent ), sysBindings_( 0 ), specialBindings_( specialBindings ),
		contId_( 0 ), contVal_( NullPtr< Kernel::Continuation >( ) )
{ }

Kernel::DynamicEnvironment::DynamicEnvironment( RefCountPtr< Kernel::DynamicEnvironment > parent, const char * contId, const Kernel::ContRef & contVal )
	: parent_( parent ), sysBindings_( 0 ), specialBindings_( 0 ),
		contId_( contId ), contVal_( contVal )
{ }

Kernel::DynamicEnvironment::~DynamicEnvironment( )
{
	if( sysBindings_ != 0 )
		{
			delete sysBindings_;
		}
	if( specialBindings_ != 0 )
		{
			delete specialBindings_;
		}
}

void
Kernel::DynamicEnvironment::tackOn( const KeyType & key, Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Ast::SourceLocation & callLoc )
{
	throw Exceptions::NotImplemented( "DynamicEnvironment::tackOn" );
//	 MapType::iterator i = bindings_.find( key );
//	 if( i == bindings_.end( ) )
//		 {
//			 if( isBaseEnvironment( ) )
//				 {
//					 throw Exceptions::InternalError( "Key of dynamic variable was not found in dynamic environment." );
//				 }
//			 return parent_->tackOn( key, evalState, piece, callLoc );
//		 }
//	 return i->second.first->tackOn( evalState, piece, callLoc );
}

void
Kernel::DynamicEnvironment::lookup( const KeyType & key, Kernel::EvalState * evalState ) const
{
	MapType::const_iterator i = bindings_.find( key );
	if( i == bindings_.end( ) )
		{
			if( isBaseEnvironment( ) )
				{
					throw NonLocalExit::DynamicBindingNotFound( );
				}
			return parent_->lookup( key, evalState );
		}

	Kernel::ContRef cont = evalState->cont_;
	cont->takeHandle( i->second.first,
										evalState );
}

Kernel::VariableHandle
Kernel::DynamicEnvironment::getVarHandle( const KeyType & key ) const
{
	MapType::const_iterator i = bindings_.find( key );
	if( i == bindings_.end( ) )
		{
			if( isBaseEnvironment( ) )
				{
					throw NonLocalExit::DynamicBindingNotFound( );
				}
			return parent_->getVarHandle( key );
		}
	return i->second.first;
}

RefCountPtr< Kernel::DynamicEnvironment >
Kernel::DynamicEnvironment::selectParent( RefCountPtr< Kernel::DynamicEnvironment > & self, const MapType & newBindings )
{
	if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) ||
			sysBindings_ != 0 ||
			specialBindings_ != 0 ||
			contId_ != 0 )
		{
			return self;
		}

	MapType::const_iterator hint = newBindings.begin( );
	for( MapType::const_iterator i = bindings_.begin( ); i != bindings_.end( ); ++i )
		{
			hint = newBindings.find( i->first );
			if( hint == newBindings.end( ) )
				{
					return self;
				}
		}

	return parent_->selectParent( parent_, newBindings );
}

void
Kernel::DynamicEnvironment::gcMark( Kernel::GCMarkedSet & marked )
{
	for( MapType::iterator i = bindings_.begin( ); i != bindings_.end( ); ++i )
		{
			i->second.first->gcMark( marked );
		}
}


RefCountPtr< const Kernel::GraphicsState >
Kernel::DynamicEnvironment::getGraphicsState( ) const
{
	if( sysBindings_ == 0 ||
			sysBindings_->graphicsState_ == NullPtr< const Kernel::GraphicsState >( ) )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "The graphics state was needed but not defined." );
				}
			return parent_->getGraphicsState( );
		}

	return sysBindings_->graphicsState_;
}

RefCountPtr< const Kernel::FacetState >
Kernel::DynamicEnvironment::getFacetState( ) const
{
	if( sysBindings_ == 0 ||
			sysBindings_->facetState_ == NullPtr< const Kernel::FacetState >( ) )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "The facet state was needed but not defined." );
				}
			return parent_->getFacetState( );
		}

	return sysBindings_->facetState_;
}

RefCountPtr< const Kernel::TextState >
Kernel::DynamicEnvironment::getTextState( ) const
{
	if( sysBindings_ == 0 ||
			sysBindings_->textState_ == NullPtr< const Kernel::TextState >( ) )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "The text state was needed but not defined." );
				}
			return parent_->getTextState( );
		}

	return sysBindings_->textState_;
}

Concrete::Length
Kernel::DynamicEnvironment::getEyeZ( ) const
{
	if( sysBindings_ == 0 ||
			IS_NAN( sysBindings_->eyez_ ) )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "@eyez should allways be bound." );
				}
			return parent_->getEyeZ( );
		}

	return sysBindings_->eyez_;
}

Concrete::Length
Kernel::DynamicEnvironment::getTeXBleed( ) const
{
	if( sysBindings_ == 0 ||
			IS_NAN( sysBindings_->TeX_bleed_ ) )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "@TeX_bleed should allways be bound." );
				}
			return parent_->getTeXBleed( );
		}

	return sysBindings_->TeX_bleed_;
}

RefCountPtr< const Kernel::PolarHandlePromise >
Kernel::DynamicEnvironment::getDefaultUnit( ) const
{
	if( sysBindings_ == 0 ||
			sysBindings_->defaultUnit_ == NullPtr< const Kernel::PolarHandlePromise >( ) )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "The default unit should allways be defined." );
				}
			return parent_->getDefaultUnit( );
		}

	return sysBindings_->defaultUnit_;
}

Kernel::ContRef
Kernel::DynamicEnvironment::getEscapeContinuation( const char * id, const Ast::SourceLocation & loc ) const
{
	if( contId_ == 0 ||
			strcmp( contId_, id ) != 0 )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::UndefinedEscapeContinuation( id, loc );
				}
			return parent_->getEscapeContinuation( id, loc );
		}

	return contVal_;
}

RefCountPtr< const Lang::ColorSpace >
Kernel::DynamicEnvironment::getBlendSpace( ) const
{
	if( sysBindings_ == 0 ||
			sysBindings_->blendSpace_ == NullPtr< const Lang::ColorSpace >( ) )
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "The blend space should allways be defined." );
				}
			return parent_->getBlendSpace( );
		}

	return sysBindings_->blendSpace_;
}

void
Kernel::DynamicEnvironment::specialUnitService( Concrete::Length * d, double * a0, double * a1 )
{
	if( specialBindings_ != 0 )
		{
			specialBindings_->specialUnitService( d, a0, a1 );
		}
	else
		{
			if( parent_ == NullPtr< Kernel::DynamicEnvironment >( ) )
				{
					throw Exceptions::InternalError( "The special unit dynamic context was needed but not defined." );
				}
			parent_->specialUnitService( d, a0, a1 );
		}
}

bool
Kernel::DynamicEnvironment::isBaseEnvironment( ) const
{
	return parent_ == NullPtr< Kernel::DynamicEnvironment >( );
}


Lang::EyeZBinding::EyeZBinding( const char * id, const Ast::SourceLocation & loc, Concrete::Length val )
	: loc_( loc ), val_( val ), id_( id )
{ }

Lang::EyeZBinding::~EyeZBinding( )
{ }

void
Lang::EyeZBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			(*sysBindings)->eyez_ = val_;
			return;
		}

	if( ! IS_NAN( (*sysBindings)->eyez_ ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	(*sysBindings)->eyez_ = val_;
}

void
Lang::EyeZBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":"
		 << val_ / Interaction::displayUnit << Interaction::displayUnitName ;
}

void
Lang::EyeZBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::EyeZDynamicVariableProperties::EyeZDynamicVariableProperties( const char * _name )
	: Kernel::DynamicVariableProperties( _name )
{ }

Kernel::EyeZDynamicVariableProperties::~EyeZDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::EyeZDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	return Helpers::newValHandle( new Lang::Length( dyn->getEyeZ( ) ) );
}

void
Kernel::EyeZDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	try
		{
			RefCountPtr< const Lang::Length > len = val->tryVal< const Lang::Length >( );
			Kernel::ContRef cont = evalState->cont_;
			cont->takeValue( Kernel::ValueRef( new Lang::EyeZBinding( name_, idLoc, len->get( ) ) ),
											 evalState );
			return;
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			/* never mind */
		}

	try
		{
			RefCountPtr< const Lang::Float > maybeInfinity = val->tryVal< const Lang::Float >( );
			if( maybeInfinity->val_ < HUGE_VAL )
				{
					throw Exceptions::OutOfRange( exprLoc, strrefdup( "The only float value allowed here is infinity." ) );
				}
			Kernel::ContRef cont = evalState->cont_;
			cont->takeValue( Kernel::ValueRef( new Lang::EyeZBinding( name_, idLoc, Concrete::HUGE_LENGTH ) ),
											 evalState );
			return;
		}
	catch( const NonLocalExit::NotThisType & ball )
		{
			/* never mind */
		}

	throw Exceptions::TypeMismatch( exprLoc, val->getUntyped( )->getTypeName( ), Helpers::typeSetString( Lang::Length::staticTypeName( ), Lang::Float::staticTypeName( ) ) );
}


Lang::DefaultUnitBinding::DefaultUnitBinding( const char * id, const Ast::SourceLocation & loc, const RefCountPtr< const Kernel::PolarHandlePromise > & val )
	: loc_( loc ), val_( val ), id_( id )
{ }

Lang::DefaultUnitBinding::~DefaultUnitBinding( )
{ }

void
Lang::DefaultUnitBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			(*sysBindings)->defaultUnit_ = val_;
			return;
		}

	if( (*sysBindings)->defaultUnit_ != NullPtr< const Kernel::PolarHandlePromise >( ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	(*sysBindings)->defaultUnit_ = val_;
}

void
Lang::DefaultUnitBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":<promise>" ;
}

void
Lang::DefaultUnitBinding::gcMark( Kernel::GCMarkedSet & marked )
{
	val_->gcMark( marked );
}


Kernel::DefaultUnitDynamicVariableProperties::DefaultUnitDynamicVariableProperties( const char * _name )
	: Kernel::DynamicVariableProperties( _name )
{ }

Kernel::DefaultUnitDynamicVariableProperties::~DefaultUnitDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::DefaultUnitDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	throw Exceptions::MiscellaneousRequirement( "The default unit cannot be evaluated as a variable." );
}

void
Kernel::DefaultUnitDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	if( ! val->isThunk( ) )
		{
			throw Exceptions::InternalError( "The default unit handle was not a thunk." );
		}

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::DefaultUnitBinding( name_, idLoc, RefCountPtr< const Kernel::PolarHandlePromise >( new Kernel::PolarHandleTruePromise( val->copyThunk( ) ) ) ) ),
									 evalState );
	return;
}


Lang::TeXBleedBinding::TeXBleedBinding( const char * id, const Ast::SourceLocation & loc, Concrete::Length val )
	: loc_( loc ), val_( val ), id_( id )
{ }

Lang::TeXBleedBinding::~TeXBleedBinding( )
{ }

void
Lang::TeXBleedBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			(*sysBindings)->TeX_bleed_ = val_;
			return;
		}

	if( ! IS_NAN( (*sysBindings)->TeX_bleed_ ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	(*sysBindings)->TeX_bleed_ = val_;
}

void
Lang::TeXBleedBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":"
		 << val_ / Interaction::displayUnit << Interaction::displayUnitName ;
}

void
Lang::TeXBleedBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::TeXBleedDynamicVariableProperties::TeXBleedDynamicVariableProperties( const char * _name )
	: Kernel::DynamicVariableProperties( _name )
{ }

Kernel::TeXBleedDynamicVariableProperties::~TeXBleedDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::TeXBleedDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	return Helpers::newValHandle( new Lang::Length( dyn->getTeXBleed( ) ) );
}

void
Kernel::TeXBleedDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Length > len = val->getVal< const Lang::Length >( exprLoc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::TeXBleedBinding( name_, idLoc, len->get( ) ) ),
									 evalState );
}


Lang::BlendSpaceBinding::BlendSpaceBinding( const char * id, const Ast::SourceLocation & loc, const RefCountPtr< const Lang::ColorSpace > & space )
	: loc_( loc ), space_( space ), id_( id )
{ }

Lang::BlendSpaceBinding::~BlendSpaceBinding( )
{ }

void
Lang::BlendSpaceBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			(*sysBindings)->blendSpace_ = space_;
			return;
		}

	if( (*sysBindings)->blendSpace_ != NullPtr< const Lang::ColorSpace >( ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	(*sysBindings)->blendSpace_ = space_;
}

void
Lang::BlendSpaceBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << "" ;
	space_->show( os );
}

void
Lang::BlendSpaceBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::BlendSpaceDynamicVariableProperties::BlendSpaceDynamicVariableProperties( const char * _name )
	: Kernel::DynamicVariableProperties( _name )
{ }

Kernel::BlendSpaceDynamicVariableProperties::~BlendSpaceDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::BlendSpaceDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	return Kernel::VariableHandle( new Kernel::Variable(	dyn->getBlendSpace( ) ) );
}

void
Kernel::BlendSpaceDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::ColorSpace > space = val->getVal< const Lang::ColorSpace >( exprLoc );
	if( ! space->isBlendable( ) )
		{
			throw Exceptions::OutOfRange( exprLoc, strrefdup( "This color space cannot be used in blending." ) );
		}

	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::BlendSpaceBinding( name_, idLoc, space ) ),
									 evalState );
}


Kernel::DynamicEnvironment::KeyType Kernel::DynamicEnvironment::nextKey( 0 );

Kernel::DynamicEnvironment::KeyType
Kernel::DynamicEnvironment::getFreshKey( )
{
	++nextKey;
	return nextKey;
}



Lang::DynamicExpression::DynamicExpression( Kernel::PassedEnv env, Ast::Expression * expr )
	: env_( env ), expr_( expr )
{ }

Lang::DynamicExpression::~DynamicExpression( )
{ }

RefCountPtr< const Lang::Class > Lang::DynamicExpression::TypeID( new Lang::SystemFinalClass( strrefdup( "DynamicExpression" ) ) );
TYPEINFOIMPL( DynamicExpression );

void
Lang::DynamicExpression::eval( Kernel::EvalState * evalState ) const
{
	evalState->env_ = env_;
	evalState->expr_ = expr_;
}

void
Lang::DynamicExpression::gcMark( Kernel::GCMarkedSet & marked )
{
	env_->gcMark( marked );
}


void
Kernel::registerDynamic( Kernel::Environment * env )
{
	env->initDefineDynamic( new Kernel::WidthDynamicVariableProperties( "width" ) );
	env->initDefineDynamic( new Kernel::CapStyleDynamicVariableProperties( "cap" ) );
	env->initDefineDynamic( new Kernel::JoinStyleDynamicVariableProperties( "join" ) );
	env->initDefineDynamic( new Kernel::MiterLimitDynamicVariableProperties( "miterlimit" ) );
	env->initDefineDynamic( new Kernel::StrokingDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_STROKING ) );
	env->initDefineDynamic( new Kernel::NonStrokingDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_NONSTROKING ) );
	env->initDefineDynamic( new Kernel::DashDynamicVariableProperties( "dash" ) );
	env->initDefineDynamic( new Kernel::BlendModeDynamicVariableProperties( "blend" ) );
	env->initDefineDynamic( new Kernel::AlphaDynamicVariableProperties( "nonstrokingalpha", false ) );
	env->initDefineDynamic( new Kernel::AlphaDynamicVariableProperties( "strokingalpha", true ) );

	env->initDefineDynamic( new Kernel::ReflectionsDynamicVariableProperties( "reflections" ) );
	env->initDefineDynamic( new Kernel::AutoIntensityDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_AUTOINTENSITY ) );
	env->initDefineDynamic( new Kernel::AutoScatteringDynamicVariableProperties( "autoscattering" ) );
	env->initDefineDynamic( new Kernel::ViewResolutionDynamicVariableProperties( "facetresolution" ) );
	env->initDefineDynamic( new Kernel::ShadeOrderDynamicVariableProperties( "shadeorder" ) );

	env->initDefineDynamic( new Kernel::CharacterSpacingDynamicVariableProperties( "text_characterspacing" ) );
	env->initDefineDynamic( new Kernel::WordSpacingDynamicVariableProperties( "text_wordspacing" ) );
	env->initDefineDynamic( new Kernel::HorizontalScalingDynamicVariableProperties( "text_horizontalscaling" ) );
	env->initDefineDynamic( new Kernel::LeadingDynamicVariableProperties( "text_leading" ) );
	env->initDefineDynamic( new Kernel::FontDynamicVariableProperties( "text_font" ) );
	env->initDefineDynamic( new Kernel::TextSizeDynamicVariableProperties( "text_size" ) );
	env->initDefineDynamic( new Kernel::TextRenderingModeDynamicVariableProperties( "text_rendering" ) );
	env->initDefineDynamic( new Kernel::TextRiseDynamicVariableProperties( "text_rise" ) );
	env->initDefineDynamic( new Kernel::TextKnockoutDynamicVariableProperties( "text_knockout" ) );

	env->initDefineDynamic( new Kernel::TeXBleedDynamicVariableProperties( "TeX_bleed" ) );

	env->initDefineDynamic( new Kernel::EyeZDynamicVariableProperties( Lang::DYNAMIC_VARIABLE_ID_EYEZ ) );
	env->initDefineDynamic( new Kernel::DefaultUnitDynamicVariableProperties( "defaultunit" ) );
	env->initDefineDynamic( new Kernel::BlendSpaceDynamicVariableProperties( "blendspace" ) );
}

