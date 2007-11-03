#include "tagtypes.h"
#include "globals.h"
#include "ast.h"

using namespace Shapes;


Lang::TaggedValue2D::TaggedValue2D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Value > & val )
  : key_( key->getKey( ) ), val_( val )
{ }

Lang::TaggedValue2D::TaggedValue2D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Value > & val )
  : key_( key ), val_( val )
{ }

Lang::TaggedValue2D::~TaggedValue2D( )
{ }

void
Lang::TaggedValue2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
  // Do nothing!
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::TaggedValue2D::bbox( ) const
{
  return Lang::THE_EMPTYPATH2D;
}

RefCountPtr< const Lang::Geometric3D >
Lang::TaggedValue2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  return RefCountPtr< const Lang::Geometric3D >( new Lang::TaggedValue3D( key_, val_ ) );
}

void
Lang::TaggedValue2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      dst->push_back( val_ );
    }
}

bool
Lang::TaggedValue2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( val_,
		       evalState );
      return true;
    }
  return false;
}

void
Lang::TaggedValue2D::show( std::ostream & os ) const
{
  os << "Value with the tag " << Lang::Symbol( key_ ).name( ) ;
}

Lang::Symbol::KeyType
Lang::TaggedValue2D::key( ) const
{
  return key_;
}

RefCountPtr< const Lang::Value >
Lang::TaggedValue2D::val( ) const
{
  return val_;
}

void
Lang::TaggedValue2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Value * >( val_.getPtr( ) )->gcMark( marked );
}


Lang::TaggedGeometric2D::TaggedGeometric2D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Geometric2D > & val )
  : key_( key->getKey( ) ), val_( val )
{ }

Lang::TaggedGeometric2D::TaggedGeometric2D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Geometric2D > & val )
  : key_( key ), val_( val )
{ }

Lang::TaggedGeometric2D::~TaggedGeometric2D( )
{ }

void
Lang::TaggedGeometric2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
  // Do nothing!
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::TaggedGeometric2D::bbox( ) const
{
  return Lang::THE_EMPTYPATH2D;
}

RefCountPtr< const Lang::Geometric3D >
Lang::TaggedGeometric2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  return RefCountPtr< const Lang::Geometric3D >( new Lang::TaggedGeometric3D( key_, val_->to3D( val_ ) ) );
}

void
Lang::TaggedGeometric2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      dst->push_back( val_->transformed( tf, val_ ) );
    }
}

bool
Lang::TaggedGeometric2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( val_->transformed( tf, val_ ),
		       evalState );
      return true;
    }
  return false;
}

void
Lang::TaggedGeometric2D::show( std::ostream & os ) const
{
  os << "Geometric value with the tag " << Lang::Symbol( key_ ).name( ) ;
}

void
Lang::TaggedGeometric2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Geometric2D * >( val_.getPtr( ) )->gcMark( marked );
}


Lang::TaggedDrawable2D::TaggedDrawable2D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Drawable2D > & val )
  : key_( key->getKey( ) ), val_( val )
{ }

Lang::TaggedDrawable2D::TaggedDrawable2D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Drawable2D > & val )
  : key_( key ), val_( val )
{ }

Lang::TaggedDrawable2D::~TaggedDrawable2D( )
{ }

void
Lang::TaggedDrawable2D::shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const
{
  val_->shipout( os, pdfState, tf );
}

RefCountPtr< const Lang::ElementaryPath2D >
Lang::TaggedDrawable2D::bbox( ) const
{
  return val_->bbox( );
}

RefCountPtr< const Lang::Geometric3D >
Lang::TaggedDrawable2D::to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const
{
  return RefCountPtr< const Lang::Geometric3D >( new Lang::TaggedDrawable3D
						 ( key_,
						   RefCountPtr< const Lang::Drawable3D >( new Lang::Drawable2Din3D( val_ ) ) ) );
}

void
Lang::TaggedDrawable2D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      dst->push_back( val_->transformed( tf, val_ ) );
    }
}

bool
Lang::TaggedDrawable2D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( val_->transformed( tf, val_ ),
		       evalState );
      return true;
    }
  return false;
}

void
Lang::TaggedDrawable2D::show( std::ostream & os ) const
{
  os << "Drawable value with the tag " << Lang::Symbol( key_ ).name( ) ;
}

void
Lang::TaggedDrawable2D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Drawable2D * >( val_.getPtr( ) )->gcMark( marked );
}


Lang::TaggedValue3D::TaggedValue3D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Value > & val )
  : key_( key->getKey( ) ), val_( val )
{ }

Lang::TaggedValue3D::TaggedValue3D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Value > & val )
  : key_( key ), val_( val )
{ }

Lang::TaggedValue3D::~TaggedValue3D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::TaggedValue3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
  return RefCountPtr< const Lang::Drawable2D >( new Lang::TaggedValue2D( key_, val_ ) );
}

void
Lang::TaggedValue3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
  // Note that polygonization eats tags.
}

void
Lang::TaggedValue3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      dst->push_back( val_ );
    }
}

bool
Lang::TaggedValue3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( val_,
		       evalState );
      return true;
    }
  return false;
}

void
Lang::TaggedValue3D::show( std::ostream & os ) const
{
  os << "Value with the tag " << Lang::Symbol( key_ ).name( ) ;
}

void
Lang::TaggedValue3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Value * >( val_.getPtr( ) )->gcMark( marked );
}


Lang::TaggedGeometric3D::TaggedGeometric3D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Geometric3D > & val )
  : key_( key->getKey( ) ), val_( val )
{ }

Lang::TaggedGeometric3D::TaggedGeometric3D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Geometric3D > & val )
  : key_( key ), val_( val )
{ }

Lang::TaggedGeometric3D::~TaggedGeometric3D( )
{ }


RefCountPtr< const Lang::Drawable2D >
Lang::TaggedGeometric3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
  RefCountPtr< const Lang::Geometric3D > tfVal = val_->transformed( tf, val_ );
  return RefCountPtr< const Lang::Drawable2D >( new Lang::TaggedGeometric2D( key_, tfVal->to2D( dyn, tfVal ) ) );
}

void
Lang::TaggedGeometric3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
  // Note that polygonization eats tags.
}

void
Lang::TaggedGeometric3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      dst->push_back( val_->transformed( tf, val_ ) );
    }
}

bool
Lang::TaggedGeometric3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( val_->transformed( tf, val_ ),
		       evalState );
      return true;
    }
  return false;
}

void
Lang::TaggedGeometric3D::show( std::ostream & os ) const
{
  os << "Geometric value with the tag " << Lang::Symbol( key_ ).name( ) ;
}

void
Lang::TaggedGeometric3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Geometric3D * >( val_.getPtr( ) )->gcMark( marked );
}


Lang::TaggedDrawable3D::TaggedDrawable3D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Drawable3D > & val )
  : key_( key->getKey( ) ), val_( val )
{ }

Lang::TaggedDrawable3D::TaggedDrawable3D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Drawable3D > & val )
  : key_( key ), val_( val )
{ }

Lang::TaggedDrawable3D::~TaggedDrawable3D( )
{ }

RefCountPtr< const Lang::Drawable2D >
Lang::TaggedDrawable3D::typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
  return RefCountPtr< const Lang::Drawable2D >( new Lang::TaggedDrawable2D( key_, val_->typed_to2D( dyn, tf, val_ ) ) );
}

void
Lang::TaggedDrawable3D::polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const
{
  // Note that polygonization eats tags.

  val_->polygonize( zBufPile, linePile, dyn, tf, val_ );
}


void
Lang::TaggedDrawable3D::findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      dst->push_back( val_->transformed( tf, val_ ) );
    }
}

bool
Lang::TaggedDrawable3D::findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const
{
  // tf is ignored for this type of tag.
  if( key_ == key )
    {
      Kernel::ContRef cont = evalState->cont_;
      cont->takeValue( val_->transformed( tf, val_ ),
		       evalState );
      return true;
    }
  return false;
}

void
Lang::TaggedDrawable3D::show( std::ostream & os ) const
{
  os << "Drawable value with the tag " << Lang::Symbol( key_ ).name( ) ;
}

void
Lang::TaggedDrawable3D::gcMark( Kernel::GCMarkedSet & marked )
{
  const_cast< Lang::Drawable3D * >( val_.getPtr( ) )->gcMark( marked );
}

