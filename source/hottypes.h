#ifndef hottypes_h
#define hottypes_h

#include "Shapes_Ast_decls.h"
#include "Shapes_Kernel_decls.h"
#include "Shapes_Lang_decls.h"
#include "FontMetrics_decls.h"
#include "SimplePDF_decls.h"

#include "shapesvalue.h"
#include "functiontypes.h"
#include "drawabletypes.h"
#include "lighttypes.h"
#include "fonttypes.h"
#include "charptrless.h"

#include <iostream>
#include <fstream>
#include <sys/time.h>


#define TYPEINFOIMPL_STATE( T )												\
	const RefCountPtr< const ::Shapes::Lang::Class > &				\
		Kernel::T::getClass( ) const												\
	{																								\
		return TypeID;																\
	}																								\
	RefCountPtr< const char >												\
		Kernel::T::staticTypeName( )												\
		{																								\
			return TypeID->getPrettyName( );								\
		}


namespace Shapes
{
	namespace Lang
	{

		class Hot : public Lang::NoOperatorOverloadValue
		{
		public:
			Hot( );
			virtual ~Hot( );
			virtual Kernel::State * newState( ) const = 0;
			TYPEINFODECL;
		};


		class HotTriple : public Lang::Hot
		{
			RefCountPtr< const Lang::Value > init_;
			RefCountPtr< const Lang::Function > update_;
			RefCountPtr< const Lang::Function > result_;
		public:
			HotTriple( const RefCountPtr< const Lang::Value > & init, RefCountPtr< const Lang::Function > update, RefCountPtr< const Lang::Function > result );
			virtual ~HotTriple( );
			virtual Kernel::State * newState( ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

		/* This template can be used to create hot types such as 2D and 3D, where the warm value can be created using a default constructor
		 */
		template< class S >
		class HotDefault : public Lang::Hot
		{
		public:
			HotDefault( ) { }
			virtual ~HotDefault( ) { }
			virtual Kernel::State * newState( ) const { return new S; }
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ }
		};

		class HotRandomSeed : public Lang::Hot
		{
			size_t sz_;
			char * state_;
		public:
			HotRandomSeed( size_t sz, Kernel::WarmRandomDevice * dev );
			HotRandomSeed( size_t sz, unsigned long seed );
			virtual ~HotRandomSeed( );
			virtual Kernel::State * newState( ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked );
		};

	}

	namespace Kernel
	{

		class WarmTriple : public Kernel::State
		{
			RefCountPtr< const Lang::Value > pile_;
			RefCountPtr< const Lang::Function > update_;
			RefCountPtr< const Lang::Function > result_;
		public:
			WarmTriple( const RefCountPtr< const Lang::Value > & pile, RefCountPtr< const Lang::Function > update, RefCountPtr< const Lang::Function > result );
			virtual ~WarmTriple( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmIgnore : public Kernel::State
		{
		public:
			WarmIgnore( );
			virtual ~WarmIgnore( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmOstream : public Kernel::State
		{
			std::ostream & os_;
		public:
			WarmOstream( std::ostream & os );
			virtual ~WarmOstream( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class Warm_ostringstream : public Kernel::State
		{
			std::ostringstream os_;
		public:
			Warm_ostringstream( );
			virtual ~Warm_ostringstream( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmGroup2D : public Kernel::State
		{
			RefCountPtr< const Lang::Group2D > pile_;
		public:
			WarmGroup2D( );
			virtual ~WarmGroup2D( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			RefCountPtr< const Lang::Group2D > getPile( ){ return pile_; } /* For special use with arrowheads and instances of user classes */
			void erase( );
			void remove( Lang::Symbol::KeyType key );
			TYPEINFODECL;
		};

		class WarmGroup3D : public Kernel::State
		{
			RefCountPtr< const Lang::Group3D > pile_;
		public:
			WarmGroup3D( );
			virtual ~WarmGroup3D( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			RefCountPtr< const Lang::Group3D > getPile( ){ return pile_; } /* For special use with arrowheads and instances of user classes */
			void erase( );
			void remove( Lang::Symbol::KeyType key );
			TYPEINFODECL;
		};

		class WarmGroupLights : public Kernel::State
		{
			RefCountPtr< const Lang::LightGroup > pile_;
		public:
			WarmGroupLights( );
			virtual ~WarmGroupLights( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			RefCountPtr< const Lang::LightGroup > getPile( ){ return pile_; } /* For special use with arrowheads and instances of user classes */
			TYPEINFODECL;
		};

		class WarmZBuf : public Kernel::State
		{
			RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > pile_;
			RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > strokePile_;
			RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > lightPile_;
		public:
			WarmZBuf( );
			virtual ~WarmZBuf( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmZSorter : public Kernel::State
		{
			RefCountPtr< std::list< RefCountPtr< Computation::PaintedPolygon3D > > > pile_;
			RefCountPtr< std::list< RefCountPtr< Computation::StrokedLine3D > > > strokePile_;
			RefCountPtr< std::list< RefCountPtr< const Lang::LightSource > > > lightPile_;
		public:
			WarmZSorter( );
			virtual ~WarmZSorter( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmTimer : public Kernel::State
		{
			timeval start_;
		public:
			WarmTimer( );
			virtual ~WarmTimer( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmText : public Kernel::State
		{
			RefCountPtr< std::list< RefCountPtr< const Lang::TextOperation > > > pile_;
		public:
			WarmText( );
			virtual ~WarmText( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmType3Font : public Kernel::State
		{
			std::list< RefCountPtr< const Lang::KernedText > > kernings_;
			std::list< RefCountPtr< const Lang::Type3Glyph > > glyphs_;
			RefCountPtr< FontMetrics::BaseFont > metrics_;
			Concrete::Length size_;
		public:
			WarmType3Font( );
			virtual ~WarmType3Font( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;

		private:
			static void initializeLegalStrechValues( std::set< const char *, charPtrLess > * legalStretchValues );
		};

		class WarmTime : public Kernel::State
		{
		public:
			WarmTime( );
			virtual ~WarmTime( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmRandomDevice : public Kernel::State
		{
			const char * deviceName_;
			std::ifstream idev_;
			std::ofstream odev_;
		public:
			WarmRandomDevice( const char * deviceName );
			virtual ~WarmRandomDevice( );
			void read( char * dst, size_t sz );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmRandomState : public Kernel::State
		{
			char * state_;
		public:
			WarmRandomState( char * state );
			void setState( );
			virtual ~WarmRandomState( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

		class WarmColorInterpolator : public Kernel::State
		{
		public:
			typedef Lang::ColorInterpolator::KeyContainer KeyContainer;
			typedef Lang::ColorInterpolator::RGBContainer RGBContainer;
			typedef Lang::ColorInterpolator::GrayContainer GrayContainer;
			typedef Lang::ColorInterpolator::CMYKContainer CMYKContainer;
			typedef Lang::ColorInterpolator::ColorType ColorType;

		private:
			RefCountPtr< KeyContainer > key_;
			RefCountPtr< RGBContainer > RGBcolor_;
			RefCountPtr< GrayContainer > graycolor_;
			RefCountPtr< CMYKContainer > CMYKcolor_;
			bool hasKey_;

			ColorType colorType_;

		public:
			WarmColorInterpolator( );
			virtual ~WarmColorInterpolator( );
			virtual void tackOnImpl( Kernel::EvalState * evalState, const RefCountPtr< const Lang::Value > & piece, const Kernel::PassedDyn & dyn, const Ast::SourceLocation & callLoc );
			virtual void peekImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void freezeImpl( Kernel::EvalState * evalState, const Ast::SourceLocation & callLoc );
			virtual void gcMark( Kernel::GCMarkedSet & marked );
			TYPEINFODECL;
		};

	}

}

#endif
