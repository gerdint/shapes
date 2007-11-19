#ifndef pdfstructure_h
#define pdfstructure_h

#include "SimplePDF_decls.h"

#include "refcount.h"

#include <iostream>
#include <list>
#include <map>
#include <vector>
#include <sstream>

namespace SimplePDF
{

	typedef std::map< class PDF_Indirect, RefCountPtr< class PDF_Indirect_out > > IndirectRemapType;

	class PDF_Object
	{
	public:
		bool complete;
		PDF_Object( );
		virtual ~PDF_Object( );
		virtual void writeTo( std::ostream & os ) const; // Not pure virtual only to make it possible to use ths class as base in std::map etc
		virtual RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > self, class PDF_out * pdfo, IndirectRemapType * remap );
	};

	template< class S >
		RefCountPtr< S > down_cast_follow( RefCountPtr< PDF_Object > maybeIndirect );

	class PDF_Null : public PDF_Object
	{
	public:
		PDF_Null( );
		virtual ~PDF_Null( );
		virtual void writeTo( std::ostream & os ) const;
	};

	class PDF_Boolean : public PDF_Object
	{
	public:
		typedef bool ValueType;
	private:
		ValueType my_value;
	public:
		PDF_Boolean( ValueType _value );
		virtual ~PDF_Boolean( );
		ValueType value( ) const;
		virtual void writeTo( std::ostream & os ) const;
	};

	class PDF_Int : public PDF_Object
	{
	public:
		typedef int ValueType;
	private:
		ValueType my_value;
	public:
		PDF_Int( ValueType _value );
		PDF_Int( const char * strvalue );
		virtual ~PDF_Int( );
		ValueType value( ) const;
		virtual void writeTo( std::ostream & os ) const;
	};

	class PDF_Float : public PDF_Object
	{
	public:
		typedef double ValueType;
	private:
		ValueType my_value;
	public:
		PDF_Float( ValueType _value );
		PDF_Float( const char * strvalue );
		virtual ~PDF_Float( );
		ValueType value( ) const;
		virtual void writeTo( std::ostream & os ) const;
	};

	class PDF_String : public PDF_Object
	{
	public:
		PDF_String( );
		virtual ~PDF_String( );
	};

	class PDF_LiteralString : public PDF_String
	{
		std::string my_str;
	public:
		PDF_LiteralString( const std::string & _str );
		PDF_LiteralString( const char * _str );
		PDF_LiteralString( const std::list< RefCountPtr< char > > & strs );
		virtual ~PDF_LiteralString( );
		const std::string & str( ) const;
		virtual void writeTo( std::ostream & os ) const;
	};

	class PDF_HexString : public PDF_String
	{
		std::string my_hexstr;
	public:
		PDF_HexString( const std::string & _hexstr );
		PDF_HexString( const char * _hexstr );
		virtual ~PDF_HexString( );
		const std::string & hexstr( ) const;
		virtual void writeTo( std::ostream & os ) const;
	};

	class PDF_Name : public PDF_Object
	{
		std::string my_name;
	public:
		PDF_Name( const std::string & _name );
		virtual ~PDF_Name( );
		const std::string & name( ) const;
		virtual void writeTo( std::ostream & os ) const;
		friend std::ostream & operator << ( std::ostream & os, const PDF_Name & self );
	};

	class PDF_Vector : public PDF_Object
	{
	public:
		PDF_Vector( );
		PDF_Vector( PDF_Float::ValueType c );
		PDF_Vector( PDF_Float::ValueType c1, PDF_Float::ValueType c2 );
		PDF_Vector( PDF_Float::ValueType c1, PDF_Float::ValueType c2, PDF_Float::ValueType c3 );;
		PDF_Vector( PDF_Float::ValueType x1, PDF_Float::ValueType y1,
								PDF_Float::ValueType x2, PDF_Float::ValueType y2 );
		PDF_Vector( PDF_Float::ValueType a, PDF_Float::ValueType b,
								PDF_Float::ValueType c, PDF_Float::ValueType d,
								PDF_Float::ValueType e, PDF_Float::ValueType f );
		PDF_Vector( PDF_Int::ValueType x1, PDF_Int::ValueType y1,
								PDF_Int::ValueType x2, PDF_Int::ValueType y2 );
		virtual ~PDF_Vector( );
		typedef std::vector< RefCountPtr<PDF_Object> > VecType;
		VecType vec;
		virtual void writeTo( std::ostream & os ) const;
		virtual RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > self, PDF_out * pdfo, IndirectRemapType * remap );
		RefCountPtr< PDF_Vector > rectangleIntersection( const RefCountPtr< PDF_Vector > & other ) const;
	};

	class PDF_Dictionary : public PDF_Object
	{
	public:
		PDF_Dictionary( );
		PDF_Dictionary( const PDF_Dictionary & orig );
		virtual ~PDF_Dictionary( );

		typedef std::map< std::string, RefCountPtr<PDF_Object> > DicType;
		DicType dic;
		virtual void writeTo( std::ostream & os ) const;
		virtual void writeToWithLength( std::ostream & os, size_t len ) const;

		PDF_Int::ValueType getLength( ) const;
		PDF_Int::ValueType getCount( ) const;
		bool isPages( ) const;
		RefCountPtr< PDF_Object > getInheritable( const char * name ) const;

		RefCountPtr< PDF_Object > & operator [] ( const char * key );
		bool hasKey( const char * key ) const;
		virtual RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > self, PDF_out * pdfo, IndirectRemapType * remap );
	};

	class PDF_Stream : public PDF_Dictionary
	{
	public:
		PDF_Stream( );
		PDF_Stream( const PDF_Dictionary & dic );
		virtual ~PDF_Stream( );

		virtual void writeTo( std::ostream & os ) const;
		virtual RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > self, PDF_out * pdfo, IndirectRemapType * remap );
	};

	class PDF_Stream_out : public PDF_Stream
	{
	public:
		PDF_Stream_out( );
		virtual ~PDF_Stream_out( );

		std::ostringstream data;
		virtual void writeTo( std::ostream & os ) const;
	};

	class PDF_Stream_in : public PDF_Stream
	{
	public:
		std::istream * is;
		std::streamoff dataStart;

		PDF_Stream_in( PDF_Dictionary * dic, std::istream * _is, std::streamoff _dataStart );
		PDF_Stream_in( std::istream * _is, std::streamoff _dataStart );
		virtual ~PDF_Stream_in( );

		virtual void writeTo( std::ostream & os ) const;
		void writeDataTo( std::ostream & os ) const;
		void writeDataDefilteredTo( std::ostream & os ) const;
		virtual RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > self, PDF_out * pdfo, IndirectRemapType * remap );
	};

	class PDF_Indirect : public PDF_Object
	{
	public:
		size_t i;
		size_t v;
		PDF_Indirect( size_t _i, size_t _v = 0 );
		virtual ~PDF_Indirect( );
		virtual void writeTo( std::ostream & os ) const;
		virtual RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > self, PDF_out * pdfo, IndirectRemapType * remap );
	};

	bool operator < ( const PDF_Indirect & o1, const PDF_Indirect & o2 );

	class PDF_Indirect_out : public PDF_Indirect
	{
	public:
		bool inUse;
		RefCountPtr< PDF_Object > obj;
		size_t byteOffset;
		PDF_Indirect_out( RefCountPtr< PDF_Object > _obj, size_t _i, size_t _v = 0 );
		PDF_Indirect_out( PDF_Object * _obj, size_t _i, size_t _v = 0 );
		virtual ~PDF_Indirect_out( );
		void writeObject( std::ostream & os ) const;
	};

	class PDF_Indirect_in : public PDF_Indirect
	{
	public:
		PDF_in * PDFin;
		PDF_Indirect_in( size_t _i, size_t _v = 0 );
		virtual ~PDF_Indirect_in( );
		RefCountPtr< PDF_Object > deref( );
		virtual RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > self, PDF_out * pdfo, IndirectRemapType * remap );
	};

	RefCountPtr< PDF_Object > deepCopy( RefCountPtr< PDF_Object > obj, PDF_out * pdfo, IndirectRemapType * remap );

}


#endif
