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

#include "facettypes.h"
#include "dynamicenvironment.h"
#include "lighttypes.h"
#include "ast.h"
#include "isnan.h"
#include "globals.h"

using namespace Shapes;


Lang::ReflectionsBinding::ReflectionsBinding( const char * id, const Ast::SourceLocation & loc, const RefCountPtr< const Lang::SpecularReflection > & reflections )
	: loc_( loc ), reflections_( reflections ), id_( id )
{ }

Lang::ReflectionsBinding::~ReflectionsBinding( )
{ }

void
Lang::ReflectionsBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->reflections_ = reflections_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	if( (*sysBindings)->facetState_ == NullPtr< const Kernel::FacetState >( ) )
		{
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->reflections_ = reflections_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	Kernel::FacetState * newState = new Kernel::FacetState( *((*sysBindings)->facetState_) );

	if( newState->reflections_ != NullPtr< const Lang::SpecularReflection >( ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->reflections_ = reflections_;
	(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
}

void
Lang::ReflectionsBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":" ;
	reflections_->show( os );
}

void
Lang::ReflectionsBinding::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::SpecularReflection * >( reflections_.getPtr( ) )->gcMark( marked );
}



Lang::AutoIntensityBinding::AutoIntensityBinding( const char * id, const Ast::SourceLocation & loc, const RefCountPtr< const Lang::Color > & color )
	: loc_( loc ), color_( color ), id_( id )
{ }

Lang::AutoIntensityBinding::~AutoIntensityBinding( )
{ }

void
Lang::AutoIntensityBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->autoIntensity_ = color_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	if( (*sysBindings)->facetState_ == NullPtr< const Kernel::FacetState >( ) )
		{
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->autoIntensity_ = color_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	Kernel::FacetState * newState = new Kernel::FacetState( *((*sysBindings)->facetState_) );

	if( newState->autoIntensity_ != NullPtr< const Lang::Color >( ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->autoIntensity_ = color_;
	(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
}

void
Lang::AutoIntensityBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":" ;
	color_->show( os );
}

void
Lang::AutoIntensityBinding::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::Color * >( color_.getPtr( ) )->gcMark( marked );
}



Lang::AutoScatteringBinding::AutoScatteringBinding( const char * id, const Ast::SourceLocation & loc, const RefCountPtr< const Lang::SpecularReflection > & reflections )
	: loc_( loc ), reflections_( reflections ), id_( id )
{ }

Lang::AutoScatteringBinding::~AutoScatteringBinding( )
{ }

void
Lang::AutoScatteringBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->autoScattering_ = reflections_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	if( (*sysBindings)->facetState_ == NullPtr< const Kernel::FacetState >( ) )
		{
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->autoScattering_ = reflections_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	Kernel::FacetState * newState = new Kernel::FacetState( *((*sysBindings)->facetState_) );

	if( newState->autoScattering_ != NullPtr< const Lang::SpecularReflection >( ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->autoScattering_ = reflections_;
	(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
}

void
Lang::AutoScatteringBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":" ;
	reflections_->show( os );
}

void
Lang::AutoScatteringBinding::gcMark( Kernel::GCMarkedSet & marked )
{
	const_cast< Lang::SpecularReflection * >( reflections_.getPtr( ) )->gcMark( marked );
}


Lang::ViewResolutionBinding::ViewResolutionBinding( const char * id, const Ast::SourceLocation & loc, const Concrete::Length resolution )
	: loc_( loc ), resolution_( resolution ), id_( id )
{ }

Lang::ViewResolutionBinding::~ViewResolutionBinding( )
{ }

void
Lang::ViewResolutionBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->viewResolution_ = resolution_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	if( (*sysBindings)->facetState_ == NullPtr< const Kernel::FacetState >( ) )
		{
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->viewResolution_ = resolution_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	Kernel::FacetState * newState = new Kernel::FacetState( *((*sysBindings)->facetState_) );

	if( ! IS_NAN( newState->viewResolution_ ) )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->viewResolution_ = resolution_;
	(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
}

void
Lang::ViewResolutionBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":"
		 << resolution_ / Interaction::displayUnit << Interaction::displayUnitName ;
}

void
Lang::ViewResolutionBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }


Lang::ShadeOrderBinding::ShadeOrderBinding( const char * id, const Ast::SourceLocation & loc, const Computation::FacetShadeOrder order )
	: loc_( loc ), order_( order ), id_( id )
{ }

Lang::ShadeOrderBinding::~ShadeOrderBinding( )
{ }

void
Lang::ShadeOrderBinding::bind( MapType & bindings, Kernel::SystemDynamicVariables ** sysBindings ) const
{
	if( *sysBindings == 0 )
		{
			*sysBindings = new Kernel::SystemDynamicVariables( );
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->shadeOrder_ = order_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	if( (*sysBindings)->facetState_ == NullPtr< const Kernel::FacetState >( ) )
		{
			Kernel::FacetState * newState = new Kernel::FacetState( );
			newState->shadeOrder_ = order_;
			(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
			return;
		}

	Kernel::FacetState * newState = new Kernel::FacetState( *((*sysBindings)->facetState_) );

	if( newState->shadeOrder_ != -1 )
		{
			throw Exceptions::MultipleDynamicBind( id_, loc_, Ast::THE_UNKNOWN_LOCATION );
		}

	newState->shadeOrder_ = order_;
	(*sysBindings)->facetState_ = RefCountPtr< const Kernel::FacetState >( newState );
}

void
Lang::ShadeOrderBinding::show( std::ostream & os ) const
{
	os << Interaction::DYNAMIC_VARIABLE_PREFIX << id_ << ":"
		 << "'" << order_ ;
}

void
Lang::ShadeOrderBinding::gcMark( Kernel::GCMarkedSet & marked )
{ }



Kernel::ReflectionsDynamicVariableProperties::ReflectionsDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::ReflectionsDynamicVariableProperties::~ReflectionsDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::ReflectionsDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::FacetState > facetState = dyn->getFacetState( );
	return Kernel::VariableHandle( new Kernel::Variable( facetState->reflections_ ) );
}

void
Kernel::ReflectionsDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::SpecularReflection > reflection = val->getVal< const Lang::SpecularReflection >( exprLoc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::ReflectionsBinding( name_, idLoc, reflection ) ),
									 evalState );
}


Kernel::AutoIntensityDynamicVariableProperties::AutoIntensityDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::AutoIntensityDynamicVariableProperties::~AutoIntensityDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::AutoIntensityDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::FacetState > facetState = dyn->getFacetState( );
	return Kernel::VariableHandle( new Kernel::Variable( facetState->autoIntensity_ ) );
}

void
Kernel::AutoIntensityDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Color > color = val->getVal< const Lang::Color >( exprLoc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::AutoIntensityBinding( name_, idLoc, color ) ),
									 evalState );
}


Kernel::AutoScatteringDynamicVariableProperties::AutoScatteringDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::AutoScatteringDynamicVariableProperties::~AutoScatteringDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::AutoScatteringDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::FacetState > facetState = dyn->getFacetState( );
	return Kernel::VariableHandle( new Kernel::Variable( facetState->autoScattering_ ) );
}

void
Kernel::AutoScatteringDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::SpecularReflection > reflection = val->getVal< const Lang::SpecularReflection >( exprLoc );
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::AutoScatteringBinding( name_, idLoc, reflection ) ),
									 evalState );
}


Kernel::ViewResolutionDynamicVariableProperties::ViewResolutionDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::ViewResolutionDynamicVariableProperties::~ViewResolutionDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::ViewResolutionDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::FacetState > facetState = dyn->getFacetState( );
	return Helpers::newValHandle( new Lang::Length( facetState->viewResolution_ ) );
}

void
Kernel::ViewResolutionDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Length > res = val->getVal< const Lang::Length >( exprLoc );
	if( res->get( ) <= 0 )
		{
			throw Exceptions::OutOfRange( exprLoc, strrefdup( "The length must be non-negative." ) );
		}
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::ViewResolutionBinding( name_, idLoc, res->get( ) ) ),
									 evalState );
}

Kernel::ShadeOrderDynamicVariableProperties::ShadeOrderDynamicVariableProperties( const char * name )
	: Kernel::DynamicVariableProperties( name )
{ }

Kernel::ShadeOrderDynamicVariableProperties::~ShadeOrderDynamicVariableProperties( )
{ }

Kernel::VariableHandle
Kernel::ShadeOrderDynamicVariableProperties::fetch( const Kernel::PassedDyn & dyn ) const
{
	RefCountPtr< const Kernel::FacetState > facetState = dyn->getFacetState( );
	return Helpers::newValHandle( new Lang::Integer( facetState->shadeOrder_ ) );
}

void
Kernel::ShadeOrderDynamicVariableProperties::makeBinding( Kernel::VariableHandle val, const Ast::SourceLocation & idLoc, const Ast::SourceLocation & exprLoc, Kernel::EvalState * evalState ) const
{
	RefCountPtr< const Lang::Integer > order = val->getVal< const Lang::Integer >( exprLoc );
	if( order->val_ < 0 || order->val_ > 2 )
		{
			throw Exceptions::OutOfRange( exprLoc, strrefdup( "The shade order integer must be one of { 0, 1, 2 }." ) );
		}
	Kernel::ContRef cont = evalState->cont_;
	cont->takeValue( Kernel::ValueRef( new Lang::ShadeOrderBinding( name_, idLoc, order->val_ ) ),
									 evalState );
}



Kernel::FacetState::FacetState( )
	: reflections_( NullPtr< const Lang::SpecularReflection >( ) ),
		autoIntensity_( NullPtr< const Lang::Color >( ) ),
		autoScattering_( NullPtr< const Lang::SpecularReflection >( ) ),
		viewResolution_( std::numeric_limits< double >::signaling_NaN( ) ),
		shadeOrder_( -1 )
{ }

Kernel::FacetState::FacetState( const Kernel::FacetState & orig )
	: reflections_( orig.reflections_ ),
		autoIntensity_( orig.autoIntensity_ ),
		autoScattering_( orig.autoScattering_ ),
		viewResolution_( orig.viewResolution_ ),
		shadeOrder_( orig.shadeOrder_ )
{ }

Kernel::FacetState::FacetState( const Kernel::FacetState & newValues, const Kernel::FacetState & oldValues )
	: reflections_( oldValues.reflections_ ),
		autoIntensity_( oldValues.autoIntensity_ ),
		autoScattering_( oldValues.autoScattering_ ),
		viewResolution_( oldValues.viewResolution_ ),
		shadeOrder_( oldValues.shadeOrder_ )
{
	if( newValues.reflections_ != NullPtr< const Lang::SpecularReflection >( ) )
		{
			reflections_ = newValues.reflections_;
		}
	if( newValues.autoIntensity_ != NullPtr< const Lang::Color >( ) )
		{
			autoIntensity_ = newValues.autoIntensity_;
		}
	if( newValues.autoScattering_ != NullPtr< const Lang::SpecularReflection >( ) )
		{
			autoScattering_ = newValues.autoScattering_;
		}
	if( ! IS_NAN( newValues.viewResolution_ ) )
		{
			viewResolution_ = newValues.viewResolution_;
		}
	if( newValues.shadeOrder_ != -1 )
		{
			shadeOrder_ = newValues.shadeOrder_;
		}
}

Kernel::FacetState::FacetState( bool setDefaults )
	: reflections_( RefCountPtr< const Lang::SpecularReflection >( new Lang::SpecularReflectionNull( ) ) ),
		autoIntensity_( RefCountPtr< const Lang::Color >( new Lang::Gray( 0 ) ) ),
		autoScattering_( RefCountPtr< const Lang::SpecularReflection >( new Lang::SpecularReflectionNull( ) ) ),
		viewResolution_( Concrete::HUGE_LENGTH ),
		shadeOrder_( 0 )
{
	if( ! setDefaults )
		{
			throw Exceptions::InternalError( strrefdup( "setDefaults must be true in FacetState::FacetState." ) );
		}
}

Kernel::FacetState::~FacetState( )
{ }
