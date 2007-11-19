#ifndef elementarytypes_h
#define elementarytypes_h

#include "Shapes_Ast_decls.h"
#include "Shapes_Kernel_decls.h"
#include "Shapes_Concrete_decls.h"

#include "ptrowner.h"
#include "refcount.h"
#include "pdfstructure.h"
#include "shapesvalue.h"
#include "charptrless.h"
#include "elementarylength.h"
#include "consts.h"

#include <list>
#include <iostream>
#include <stack>
#include <set>


namespace Shapes
{
	
	namespace Concrete
	{
		class SplineTime
		{
			Concrete::Time t_;
			bool isPast_;
		public:
			SplineTime( const SplineTime & orig ) : t_( orig.t_ ), isPast_( orig.isPast_ ) { };
			SplineTime( Concrete::Time t ) : t_( t ), isPast_( false ) { }
			SplineTime( Concrete::Time t, bool isPast ) : t_( t ), isPast_( isPast ) { }
			
			const Concrete::Time & t( ) const { return t_; }
			const bool & isPast( ) const { return isPast_; }
			
			SplineTime & operator ++ ( ) { t_ += UNIT_TIME; return *this; }
		};
	}

	namespace Lang
	{

		class Void : public Lang::Value
		{
		public:
			Void( );
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			TYPEINFODECL;
			virtual Kernel::QuickTypeID getTypeID( ) const { return Kernel::TYPEID_NoOperatorOverloadValue; };
		};
		
		class Symbol : public Lang::Value
		{
		public:
			typedef int KeyType;
		private:
			typedef std::map< const char *, int, charPtrLess > NameTableType;
			typedef std::map< int, RefCountPtr< const char > > ReverseTableType;
			static NameTableType nameTable;
			static ReverseTableType reverseTable;
			static KeyType nextUnique;
			KeyType key_;
		public:
			Symbol( );
			Symbol( int key );
			Symbol( const char * name );
			
			bool operator == ( const Symbol & other ) const;
			bool operator != ( const Symbol & other ) const;
			bool operator < ( const Symbol & other ) const;
			bool operator > ( const Symbol & other ) const;
			bool operator <= ( const Symbol & other ) const;
			bool operator >= ( const Symbol & other ) const;
			bool isUnique( ) const;
			KeyType getKey( ) const { return key_; }
			RefCountPtr< const char > name( ) const;
			static RefCountPtr< const char > nameFromKey( KeyType key );
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		class Float : public Lang::Value
		{
		public:
			double val_;
			Float( double val ) : val_( val ) { };
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		class Integer : public Lang::Value
		{
		public:
			typedef int ValueType;
			ValueType val_;
			Integer( ValueType val ) : val_( val ) { };
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		class Length : public Lang::Value
		{
			bool isOffset_;
			Concrete::Length val_;
		public:
			Length( const Lang::Length & orig ) : isOffset_( orig.isOffset_ ), val_( orig.val_ ) { };
			// If the following is not explicit, there is a risk that
			//	 operator <<
			// gets applied to output a Concrete::Length via Lang::Length, which is typically not what we want.
			explicit Length( Concrete::Length val ) : isOffset_( false ), val_( val ) { };
			Length( bool isOffset, Concrete::Length val ) : isOffset_( isOffset ), val_( val ) { };
			Concrete::Length get( Concrete::Length baseLength ) const;
			Concrete::Length get( ) const;
			double getScalar( Concrete::Length baseLength ) const;
			double getScalar( ) const;
			Lang::Length operator + ( const Lang::Length & term ) const;
			Lang::Length operator - ( const Lang::Length & term ) const;
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			friend std::ostream & operator << ( std::ostream & os, const Lang::Length & self );
			DISPATCHDECL;
		};
		
		class Boolean : public Lang::Value
		{
		public:
			bool val_;
			Boolean( bool val ) : val_( val ) { };
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		class String : public Lang::Value
		{
		public:
			RefCountPtr< const char > val_;
			String( RefCountPtr< const char > val ) : val_( val ) { };
			String( const char * val ) : val_( val ) { };
			virtual ~String( );
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		class FloatPair : public Lang::Value
		{
		public:
			double x_;
			double y_;
			FloatPair( double x, double y ) : x_( x ), y_( y ) { };
			FloatPair( const Concrete::UnitFloatPair & orig );
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		class FloatTriple : public Lang::Value
		{
		public:
			double x_;
			double y_;
			double z_;
			FloatTriple( double x, double y, double z ) : x_( x ), y_( y ), z_( z ) { };
			FloatTriple( const Concrete::UnitFloatTriple & orig );
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			DISPATCHDECL;
		};
		
		inline
		Lang::FloatTriple
		cross( const Lang::FloatTriple & a, const Lang::FloatTriple & b )
		{
			return Lang::FloatTriple( a.y_ * b.z_ - a.z_ * b.y_,
																a.z_ * b.x_ - a.x_ * b.z_,
																a.x_ * b.y_ - a.y_ * b.x_ );
		}
		
		class Coords2D : public Lang::Geometric2D
		{
		public:
			Lang::Length x_;
			Lang::Length y_;
			Coords2D( const Lang::Coords2D & orig );
			Coords2D( const Lang::Length & x, const Lang::Length & y );
			Coords2D( const Concrete::Length & x, const Concrete::Length & y );
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			Coords2D * transformedPtr( const Lang::Transform2D & tf ) const;
			virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;
			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			friend std::ostream & operator << ( std::ostream & os, const Lang::Coords2D & self );
			DISPATCHDECL;
		};
		
		class CornerCoords2D : public Lang::Coords2D
		{
		public:
			double a_;
			CornerCoords2D( const Lang::Length & x, const Lang::Length & y, double a );
			CornerCoords2D( const Concrete::Length & x, const Concrete::Length & y, double a );
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			CornerCoords2D * transformedPtr( const Lang::Transform2D & tf ) const;
			virtual RefCountPtr< const Lang::Geometric2D > transformed( const Lang::Transform2D & tf, const RefCountPtr< const Lang::Geometric2D > & self ) const;
			virtual RefCountPtr< const Lang::Geometric3D > to3D( const RefCountPtr< const Lang::Geometric2D > & self ) const;
			TYPEINFODECL;
			DISPATCHDECL;
		};
		
		class Coords3D : public Lang::Geometric3D
		{
		public:
			Lang::Length x_;
			Lang::Length y_;
			Lang::Length z_;
			Coords3D( const Coords3D & orig );
			Coords3D( const Lang::Length & x, const Lang::Length & y, const Lang::Length & z );
			Coords3D( const Concrete::Length & x, const Concrete::Length & y, const Concrete::Length & z );
			virtual Kernel::VariableHandle getField( const char * fieldID, const RefCountPtr< const Lang::Value > & selfRef ) const;
			Coords3D * transformedPtr( const Lang::Transform3D & tf ) const;
			virtual RefCountPtr< const Lang::Geometric3D > transformed( const Lang::Transform3D & tf, const RefCountPtr< const Lang::Geometric3D > & self ) const;
			RefCountPtr< const Lang::Coords2D > make2D( Concrete::Length eyez ) const;
			virtual RefCountPtr< const Lang::Geometric2D > to2D( const Kernel::PassedDyn & dyn, const RefCountPtr< const Lang::Geometric3D > & self ) const;
			TYPEINFODECL;
			virtual void show( std::ostream & os ) const;
			virtual void gcMark( Kernel::GCMarkedSet & marked ){ };
			friend std::ostream & operator << ( std::ostream & os, const Lang::Coords3D & self );
			DISPATCHDECL;
		};
		
	}
}

#endif
