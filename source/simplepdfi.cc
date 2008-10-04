#include <iomanip>
#include <string>
#include <ctype.h>
#include <vector>

#include "simplepdfi.h"
//#include "globals.h"
#include "pdfscanner.h"

using namespace std;
using namespace SimplePDF;

namespace SimplePDF
{
	template< >
	RefCountPtr< PDF_Object >
	SimplePDF::PDF_in::follow< PDF_Object >( RefCountPtr< PDF_Object > maybeIndirect )
	{
		PDF_Indirect * tmp( dynamic_cast< PDF_Indirect * >( maybeIndirect.getPtr( ) ) );
		if( tmp == 0 )
			{
				return maybeIndirect;
			}
		return follow< PDF_Object >( readObjectNumbered( tmp->i, tmp->v ) );
	}

	template< >
	RefCountPtr< PDF_Float >
	SimplePDF::PDF_in::follow< PDF_Float >( RefCountPtr< PDF_Object > maybeIndirect )
		{
			PDF_Indirect * tmp( dynamic_cast< PDF_Indirect * >( maybeIndirect.getPtr( ) ) );
			if( tmp == 0 )
				{
					{
						RefCountPtr< PDF_Float > res( maybeIndirect.down_cast< PDF_Float >( ) );
						if( res != NullPtr< PDF_Float >( ) )
							{
								return res;
							}
					}
					{
						RefCountPtr< PDF_Int > res( maybeIndirect.down_cast< PDF_Int >( ) );
						if( res != NullPtr< PDF_Int >( ) )
							{
								return RefCountPtr< PDF_Float >( new PDF_Float( res->value( ) ) );
							}
					}
					throw( "Downcast in PDF_in::follow failed" );
				}
			return follow< PDF_Float >( readObjectNumbered( tmp->i, tmp->v ) );
		}

}


SimplePDF::PDF_in::PageIterator::PageIterator( SimplePDF::PDF_in & _in, int _pageNo )
	: pageNo( _pageNo ), in( _in )
{ }

SimplePDF::PDF_in::PageIterator::PageIterator( const PageIterator & orig )
	: pageNo( orig.pageNo ), in( orig.in )
{ }

SimplePDF::PDF_in::PageIterator &
SimplePDF::PDF_in::PageIterator::operator = ( const PageIterator & orig )
{
	pageNo = orig.pageNo;
	in = orig.in;
	return *this;
}

bool
SimplePDF::PDF_in::PageIterator::operator == ( const PageIterator & i2 ) const
{
	return pageNo == i2.pageNo;
}

bool
SimplePDF::PDF_in::PageIterator::operator != ( const PageIterator & i2 ) const
{
	return ! operator == ( i2 );
}

RefCountPtr< PDF_Dictionary >
SimplePDF::PDF_in::PageIterator::operator * ( )
{
	return in.getPage( pageNo );
}

SimplePDF::PDF_in::PageIterator
SimplePDF::PDF_in::PageIterator::operator ++ ( )
{
	operator += ( 1 );
	return *this;
}

SimplePDF::PDF_in::PageIterator
SimplePDF::PDF_in::PageIterator::operator -- ( )
{
	operator -= ( 1 );
	return *this;
}

SimplePDF::PDF_in::PageIterator
SimplePDF::PDF_in::PageIterator::operator ++ ( int )
{
	SimplePDF::PDF_in::PageIterator old( *this );
	operator += ( 1 );
	return old;
}

SimplePDF::PDF_in::PageIterator
SimplePDF::PDF_in::PageIterator::operator -- ( int )
{
	SimplePDF::PDF_in::PageIterator old( *this );
	operator -= ( 1 );
	return old;
}

SimplePDF::PDF_in::PageIterator &
SimplePDF::PDF_in::PageIterator::operator += ( int diff )
{
	if( diff == 0 )
		{
			return *this;
		}
	if( diff < 0 )
		{
			return operator -= ( -diff );
		}
	pageNo += diff;
	return *this;
}

SimplePDF::PDF_in::PageIterator &
SimplePDF::PDF_in::PageIterator::operator -= ( int diff )
{
	if( diff == 0 )
		{
			return *this;
		}
	if( diff < 0 )
		{
			return operator += ( -diff );
		}
	pageNo -= diff;
	return *this;
}


SimplePDF::PDF_in::PDF_in( RefCountPtr< istream > _is )
	: is( _is ), isPtr( is.getPtr( ) ),
		resources( new PDF_Dictionary )
{
	string str;
	for( int i( -6 ); ; --i )
		{
			isPtr->seekg( i, ios::end );
			char c;
			isPtr->get( c );
			if( c == 'f' )
				{
					isPtr->seekg( i - 8, ios::end );
					(*isPtr) >> str ;
					if( str != "startxref" )
						{
							throw "Expected \"startxref\", found " + str;
						}
					break;
				}
		}

	{
		streamoff xrefTmp;
		(*isPtr) >> xrefTmp ;
		isPtr->seekg( xrefTmp, ios::beg );
		string tmp;
		(*isPtr) >> tmp ;
		if( tmp != "xref" )
			{
				std::ostringstream msg;
				msg << "Expected \"xref\" at " << xrefTmp ;
				throw msg.str( );
			}
	}
	{
		int i;
		(*isPtr) >> i ;
		if( i != 0 )
			{
				throw "Expected a 0 before size of xref.";
			}
	}
	(*isPtr) >> xrefSize ;
	xref = isPtr->tellg( );

	isPtr->seekg( xref + 20 * xrefSize );
	(*isPtr) >> str ;
	if( str != "trailer" )
		{
			throw "Expected \"trailer\" at this point.";
		}

	RefCountPtr< PDF_Object > trailerMem( parse( ) );
	PDF_Dictionary * trailer( dynamic_cast< PDF_Dictionary * >( trailerMem.getPtr( ) ) );
	if( trailer == 0 )
		{
			throw "Failed to parse the trailer dictionary.";
		}
	RefCountPtr< PDF_Object > rootMem( trailer->dic[ "Root" ] );
	PDF_Indirect * indirectRoot( dynamic_cast< PDF_Indirect * >( rootMem.getPtr( ) ) );
	if( indirectRoot == 0)
		{
			throw "I believe the Root field of the trailer dictionary is an indirect object...";
		}
	pages = follow<PDF_Vector>( follow<PDF_Dictionary>( follow<PDF_Dictionary>( trailer->operator[]( "Root" ) )->operator[]( "Pages" ) )->operator[]( "Kids" ) );
}

SimplePDF::PDF_in::~PDF_in( )
{
}

streamoff
SimplePDF::PDF_in::xreflookup( size_t i, size_t v )
{
	if( i >= xrefSize )
		{
			throw( "xref index out of bounds" );
		}
	isPtr->seekg( xref + 20 * i, ios::beg );
	streamoff res;
	(*isPtr) >> res;
	return res;
}

RefCountPtr< PDF_Object > PDF_in::readObjectAt( streamoff pos )
{
	isPtr->seekg( pos, ios::beg );
	return parse( );
}

RefCountPtr< PDF_Object > PDF_in::readObjectNumbered( size_t i, size_t v )
{
	try
		{
			return readObjectAt( xreflookup( i, v ) );
		}
	catch( const std::string & ball )
		{
			std::ostringstream oss;
			oss << "While parsing object " << i << " " << v << " at byte offset " << xreflookup( i, v )
					<< ", the following error occurred: " << ball ;
			throw oss.str( );
		}
}

RefCountPtr< PDF_Object >
PDF_in::parse( )
{
	PdfScanner pdfscanner( isPtr );
	vector< PdfScanner::UnionType > objectStack;
	vector< int > tokenStack;
	ostringstream stringMem;
	PdfScanner::UnionType dummyVal;
	while( true )
		{
			int token( pdfscanner.yylex( ) );
			switch( token )
				{
				case T_OpenDic:
				case '[':
					objectStack.push_back( dummyVal );
					tokenStack.push_back( token );
					break;
				case T_obj:
				case T_Constant:
				case T_Name:
					objectStack.push_back( pdfscanner.yylval );
					tokenStack.push_back( token );
					break;
				case T_R:
					objectStack.push_back( pdfscanner.yylval );
					objectStack.back( ).pdfR->PDFin = this;
					tokenStack.push_back( T_Constant );
				case '(':
					stringMem.str( "" );
					break;
				case T_String:
					{
						stringMem << *pdfscanner.yylval.str ;
						delete( pdfscanner.yylval.str );
					}
					break;
				case ')':
					objectStack.push_back( PdfScanner::UnionType( ) );
					objectStack.back( ).pdfObj = new PDF_LiteralString( stringMem.str( ).c_str( ) );
					tokenStack.push_back( T_Constant );
					break;
				case T_endobj:
					{
						RefCountPtr< PDF_Object > res;
						switch( tokenStack.back( ) )
							{
							case T_Constant:
								res = RefCountPtr< PDF_Object >( objectStack.back( ).pdfObj );
								break;
							case T_Name:
								res = RefCountPtr< PDF_Object >( new PDF_Name( objectStack.back( ).str ) );
								break;
							default:
								throw( string( "Expected a complete object before endobj." ) );
							}
						objectStack.pop_back( );
						tokenStack.pop_back( );

						if( tokenStack.back( ) != T_obj )
							{
								throw( string( "There wasn't exactly 1 object contained within obj...endobj." ) );
							}
						tokenStack.pop_back( );
						PDF_Indirect * objRef( reinterpret_cast< PDF_Indirect * >( objectStack.back( ).pdfObj ) );
						objectStack.pop_back( );

						delete objRef;
						return res;
					}
					break;
				case T_stream:
					{
						PDF_Dictionary * streamDic( dynamic_cast< PDF_Dictionary * >( objectStack.back( ).pdfObj ) );
						objectStack.pop_back( );
						tokenStack.pop_back( );
						if( streamDic == 0 )
							{
								throw( string( "Missing stream dictionary" ) );
							}
						objectStack.push_back( PdfScanner::UnionType( ) );
						objectStack.back( ).pdfObj = new PDF_Stream_in( streamDic, isPtr, isPtr->tellg( ) );
						tokenStack.push_back( T_Constant );
						{
							/* streamDic->getLength( ) may destroy the get position! */
							streamoff tmp = isPtr->tellg( );
							size_t length = streamDic->getLength( );
							isPtr->seekg( tmp + length, ios::beg );
						}
						delete streamDic;
						pdfscanner.yyrestart( isPtr );
						token = pdfscanner.yylex( );
						if( token != T_endstream )
							{
								throw( string( "Stream dictionary didn't tell the right length of the stream." ) );
							}
					}
					break;
				case T_endstream:
					throw( string( "Isolated endstream encountered" ) );
					break;
				case ']':
					{
						list< PDF_Object * > tmpList;
						int popToken( tokenStack.back( ) );
						while( popToken != '[' )
							{
								switch( popToken )
									{
									case T_Constant:
										tmpList.push_front( objectStack.back( ).pdfObj );
										break;
									case T_Name:
										tmpList.push_front( new PDF_Name( objectStack.back( ).str ) );
										break;
									default:
										throw( string( "Expected only constant values when closing vector" ) );
									}
								tokenStack.pop_back( );
								objectStack.pop_back( );

								popToken = tokenStack.back( );

							}
						tokenStack.pop_back( );
						objectStack.pop_back( );

						PDF_Vector * newVec( new PDF_Vector( ) );
						newVec->vec.reserve( tmpList.size( ) );
						for( list< PDF_Object * >::iterator i( tmpList.begin( ) ); i != tmpList.end( ); ++i )
							{
								newVec->vec.push_back( RefCountPtr< PDF_Object >( *i ) );
							}

						tokenStack.push_back( T_Constant );
						objectStack.push_back( PdfScanner::UnionType( ) );
						objectStack.back( ).pdfObj = newVec;
					}
					break;
				case T_CloseDic:
					{
						PDF_Dictionary * newDic( new PDF_Dictionary( ) );
						int popToken( tokenStack.back( ) );
						while( popToken != T_OpenDic )
							{
								PDF_Object * theObj;
								switch( popToken )
									{
									case T_Constant:
										theObj = objectStack.back( ).pdfObj;
										break;
									case T_Name:
										theObj = new PDF_Name( objectStack.back( ).str );
										break;
									default:
										throw( string( "Expected constant value at this position when closing dictionary" ) );
									}
								tokenStack.pop_back( );
								objectStack.pop_back( );

								popToken = tokenStack.back( );
								if( popToken != T_Name )
									{
										throw( string( "Expected name at this position when closing dictionary" ) );
									}
								char * theName = objectStack.back( ).str;
								newDic->dic[ theName ] = RefCountPtr< PDF_Object >( theObj );
								delete theName;
								tokenStack.pop_back( );
								objectStack.pop_back( );

								popToken = tokenStack.back( );
							}
						tokenStack.pop_back( );
						objectStack.pop_back( );

						if( objectStack.empty( ) )
							{
								return RefCountPtr< PDF_Object >( newDic );
							}
						tokenStack.push_back( T_Constant );
						objectStack.push_back( PdfScanner::UnionType( ) );
						objectStack.back( ).pdfObj = newDic;
					}
					break;
				default:
					throw( string( "Unrecognized token type." ) );
				}
		}
	throw( "Internal error in PDF_in::parse: Infinite loop should not be broken, only returned from." );
}

SimplePDF::PDF_in::PageIterator
SimplePDF::PDF_in::beginPages( )
{
	return SimplePDF::PDF_in::PageIterator( *this, 0 );
}

SimplePDF::PDF_in::PageIterator
SimplePDF::PDF_in::endPages( )
{
	return SimplePDF::PDF_in::PageIterator( *this, getPageCount( ) );
}

size_t
SimplePDF::PDF_in::getPageCount( )
{
	size_t count = 0;
	typedef typeof pages->vec ListType;
	for( ListType::const_iterator i = pages->vec.begin( ); i != pages->vec.end( ); ++i )
		{
			RefCountPtr< PDF_Dictionary > kid( follow< PDF_Dictionary >( *i ) );
			if( kid->isPages( ) )
				{
					count += kid->getCount( );
				}
			else
				{
					++count;
				}
		}

	return count;
}

RefCountPtr< PDF_Dictionary >
SimplePDF::PDF_in::getPage( size_t pageNo )
{
	RefCountPtr< PDF_Vector > kids = pages;
	while( true )
		{
			bool doAgain = false;
			typedef typeof kids->vec ListType;
			for( ListType::const_iterator i = kids->vec.begin( ); i != kids->vec.end( ); ++i )
				{
					RefCountPtr< PDF_Dictionary > kid( follow< PDF_Dictionary >( *i ) );
					if( kid->isPages( ) )
						{
							size_t count = kid->getCount( );
							if( pageNo < count )
								{
									kids = follow< PDF_Vector >( (*kid)[ "Kids" ] );
									doAgain = true;
									break;
								}
							else
								{
									pageNo -= count;
								}
						}
					else
						{
							if( pageNo == 0 )
								{
									return kid;
								}
							else
								{
									--pageNo;
								}
						}
				}
			if( ! doAgain )
				{
					throw( "Page number out of range" );
				}
		}
	throw( "Internal error in PDF_in::getPage: Infinite loop should not be broken, only returned from." );
}
