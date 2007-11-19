#ifndef tagtypes_h
#define tagtypes_h

#include "drawabletypes.h"
#include "elementarytypes.h"

namespace Shapes
{
	namespace Lang
	{

		class TaggedValue2D : public Lang::Drawable2D
		{
			Lang::Symbol::KeyType key_;
			RefCountPtr< const Lang::Value > val_;
		public:
			TaggedValue2D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Value > & val );
			TaggedValue2D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Value > & val );
			virtual ~TaggedValue2D( );
			virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
			virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
			virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
			virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
			virtual void show( std::ostream & os ) const;
			Lang::Symbol::KeyType key( ) const;
			RefCountPtr< const Lang::Value > val( ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class TaggedGeometric2D : public Lang::Drawable2D
		{
			Lang::Symbol::KeyType key_;
			RefCountPtr< const Lang::Geometric2D > val_;
		public:
			TaggedGeometric2D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Geometric2D > & val );
			TaggedGeometric2D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Geometric2D > & val );
			virtual ~TaggedGeometric2D( );
			virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
			virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
			virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
			virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class TaggedDrawable2D : public Lang::Drawable2D
		{
			Lang::Symbol::KeyType key_;
			RefCountPtr< const Lang::Drawable2D > val_;
		public:
			TaggedDrawable2D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Drawable2D > & val );
			TaggedDrawable2D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Drawable2D > & val );
			virtual ~TaggedDrawable2D( );
			virtual void shipout( std::ostream & os, Kernel::PageContentStates * pdfState, const Lang::Transform2D & tf ) const;
			virtual RefCountPtr< const Lang::ElementaryPath2D > bbox( ) const;
			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
			virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
			virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform2D & tf ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class TaggedValue3D : public Lang::Drawable3D
		{
			Lang::Symbol::KeyType key_;
			RefCountPtr< const Lang::Value > val_;
		public:
			TaggedValue3D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Value > & val );
			TaggedValue3D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Value > & val );
			virtual ~TaggedValue3D( );
			virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
			virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
			virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
			virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class TaggedGeometric3D : public Lang::Drawable3D
		{
			Lang::Symbol::KeyType key_;
			RefCountPtr< const Lang::Geometric3D > val_;
		public:
			TaggedGeometric3D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Geometric3D > & val );
			TaggedGeometric3D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Geometric3D > & val );
			virtual ~TaggedGeometric3D( );
			virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
			virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
			virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
			virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		class TaggedDrawable3D : public Lang::Drawable3D
		{
			Lang::Symbol::KeyType key_;
			RefCountPtr< const Lang::Drawable3D > val_;
		public:
			TaggedDrawable3D( const RefCountPtr< const Lang::Symbol > & key, const RefCountPtr< const Lang::Drawable3D > & val );
			TaggedDrawable3D( const Lang::Symbol::KeyType key, const RefCountPtr< const Lang::Drawable3D > & val );
			virtual ~TaggedDrawable3D( );
			virtual RefCountPtr< const Lang::Drawable2D > typed_to2D( const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
			virtual void polygonize( std::list< RefCountPtr< Computation::PaintedPolygon3D > > * zBufPile, std::list< RefCountPtr< Computation::StrokedLine3D > > * linePile, const Kernel::PassedDyn & dyn, const Lang::Transform3D & tf, const RefCountPtr< const Lang::Drawable3D > & self ) const;
			virtual void findTags( std::vector< Kernel::ValueRef > * dst, const Kernel::PassedDyn & dyn, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
			virtual bool findOneTag( Kernel::EvalState * evalState, Lang::Symbol::KeyType key, const Lang::Transform3D & tf ) const;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

	}
}


#endif
