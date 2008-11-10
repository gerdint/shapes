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

#include "shapesexceptions.h"
#include "ast.h"
#include "astclass.h"
#include "globals.h"
#include "functiontypes.h"

using namespace Shapes;
using namespace std;


const std::string Exceptions::additionalLinesPrefix = "| ";

namespace Shapes
{
	namespace Exceptions
	{
		void prefixEachLine( const std::string & prefix, std::istream & src, std::ostream & dst );
		void prefixEachLine( const std::string & prefix, const RefCountPtr< const char > & src, std::ostream & dst );
	}
}

Exceptions::NotImplemented::NotImplemented( const char * _functionality )
	: functionality( _functionality )
{ }

Exceptions::NotImplemented::~NotImplemented( )
{ }

void
Exceptions::NotImplemented::display( std::ostream & os ) const
{
	os << "Under construction: " << functionality << " is not implemented." << std::endl ;
}

const char * Exceptions::Exception::locsep = ": ";

Exceptions::Exception::Exception( )
{ }

Exceptions::Exception::~Exception( )
{ }

Exceptions::MiscellaneousRequirement::MiscellaneousRequirement( RefCountPtr< const char > msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::MiscellaneousRequirement::MiscellaneousRequirement( const char * msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::MiscellaneousRequirement::~MiscellaneousRequirement( )
{ }

void
Exceptions::MiscellaneousRequirement::display( ostream & os ) const
{
	os << "Failed to meet requirement: " << msg_ << std::endl ;
}


Exceptions::HandlerError::HandlerError( RefCountPtr< const char > _msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg( _msg )
{ }

Exceptions::HandlerError::HandlerError( const char * _msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg( strrefdup( _msg ) )
{ }

Exceptions::HandlerError::~HandlerError( )
{ }

void
Exceptions::HandlerError::display( ostream & os ) const
{
	os << "Error handler message: " << msg << std::endl ;
}


Exceptions::ScannerError::ScannerError( const Ast::SourceLocation & loc, RefCountPtr< const char > _msg )
	: Exceptions::StaticInconsistency( loc ), msg( _msg )
{ }

Exceptions::ScannerError::~ScannerError( )
{ }

void
Exceptions::ScannerError::display( ostream & os ) const
{
	os << "Scanner error: " << msg << std::endl ;
}


Exceptions::ParserError::ParserError( const Ast::SourceLocation & loc, RefCountPtr< const char > _msg )
	: Exceptions::StaticInconsistency( loc ), msg( _msg )
{ }

Exceptions::ParserError::~ParserError( )
{ }

void
Exceptions::ParserError::display( ostream & os ) const
{
	os << "Parser error: " << msg << std::endl ;
}


Exceptions::MemberAlsoAbstract::MemberAlsoAbstract( const Ast::SourceLocation & _memberLoc, const char * _id, const Ast::SourceLocation & _abstractLoc )
	: Exceptions::StaticInconsistency( _memberLoc ), memberLoc( _memberLoc ), id( _id ), abstractLoc( _abstractLoc )
{ }

Exceptions::MemberAlsoAbstract::~MemberAlsoAbstract( )
{ }

void
Exceptions::MemberAlsoAbstract::display( ostream & os ) const
{
	os << "The member " << id << " is also declared abstract, at " << abstractLoc << "." << std::endl ;
}


Exceptions::MemberAlreadyDeclared::MemberAlreadyDeclared( const Ast::SourceLocation & _memberLoc, const char * _id, const Ast::SourceLocation & _oldLoc )
	: Exceptions::StaticInconsistency( _memberLoc ), memberLoc( _memberLoc ), id( _id ), oldLoc( _oldLoc )
{ }

Exceptions::MemberAlreadyDeclared::~MemberAlreadyDeclared( )
{ }

void
Exceptions::MemberAlreadyDeclared::display( ostream & os ) const
{
	os << "The member " << id << " has already been declared, at " << oldLoc << "." << std::endl ;
}


Exceptions::PublicGetSetInNonfinalClass::PublicGetSetInNonfinalClass( const Ast::SourceLocation & _memberLoc, const char * _id )
	: Exceptions::StaticInconsistency( _memberLoc ), memberLoc( _memberLoc ), id( _id )
{ }

Exceptions::PublicGetSetInNonfinalClass::~PublicGetSetInNonfinalClass( )
{ }

void
Exceptions::PublicGetSetInNonfinalClass::display( ostream & os ) const
{
	os << "Only members of final classes may have the public get or set specifier.	Member in question: " << id << "." << std::endl ;
}


Exceptions::TransformingMemberInNonfinalClass::TransformingMemberInNonfinalClass( const Ast::SourceLocation & _memberLoc, const char * _id )
	: Exceptions::StaticInconsistency( _memberLoc ), memberLoc( _memberLoc ), id( _id )
{ }

Exceptions::TransformingMemberInNonfinalClass::~TransformingMemberInNonfinalClass( )
{ }

void
Exceptions::TransformingMemberInNonfinalClass::display( ostream & os ) const
{
	os << "Only members of final classes may have the <transforming> specifier.	Member in question: " << id << "." << std::endl ;
}


Exceptions::RepeatedFormal::RepeatedFormal( const Ast::SourceLocation & _loc, const char * _id )
	: Exceptions::StaticInconsistency( _loc ), loc( _loc ), id( _id )
{ }

Exceptions::RepeatedFormal::~RepeatedFormal( )
{ }

void
Exceptions::RepeatedFormal::display( ostream & os ) const
{
	os << "Repeated formal: " << id << std::endl ;
}


Exceptions::PassingStateOut::PassingStateOut( const Ast::SourceLocation & loc, const char * id )
	: Exceptions::StaticInconsistency( loc ), loc_( loc ), id_( id )
{ }

Exceptions::PassingStateOut::~PassingStateOut( )
{ }

void
Exceptions::PassingStateOut::display( std::ostream & os ) const
{
	os << "States may not be returned." << std::endl ;
}


Exceptions::IntroducingExisting::IntroducingExisting( const Ast::SourceLocation & _loc, const char * _id )
	: Exceptions::StaticInconsistency( _loc ), loc( _loc ), id( _id )
{ }

Exceptions::IntroducingExisting::~IntroducingExisting( )
{ }

void
Exceptions::IntroducingExisting::display( std::ostream & os ) const
{
	os << "The variable " << id << " is already introduced in this scope." << std::endl ;
}


Exceptions::ExpectedImperative::ExpectedImperative( const Ast::SourceLocation & _loc )
	: Exceptions::StaticInconsistency( _loc ), loc( _loc )
{ }

Exceptions::ExpectedImperative::~ExpectedImperative( )
{ }

void
Exceptions::ExpectedImperative::display( std::ostream & os ) const
{
	os << "It makes no sense to put a pure expression before the end of a code bracket." << std::endl ;
}


Exceptions::IllegalImperative::IllegalImperative( const Ast::SourceLocation & _loc )
	: Exceptions::StaticInconsistency( _loc ), loc( _loc )
{ }

Exceptions::IllegalImperative::~IllegalImperative( )
{ }

void
Exceptions::IllegalImperative::display( std::ostream & os ) const
{
	os << "Imperative expressions are not allowed here." << std::endl ;
}


Exceptions::FreezingUndefined::FreezingUndefined( const Ast::SourceLocation & _loc, const char * _id )
	: Exceptions::StaticInconsistency( _loc ), loc( _loc ), id( _id )
{ }

Exceptions::FreezingUndefined::~FreezingUndefined( )
{ }

void
Exceptions::FreezingUndefined::display( std::ostream & os ) const
{
	os << "Internal error:	The variable " << id << " is freezed before it was defined.	Either this should be caught by during the parse, or it is due to an error in the setup of the initial environment." << std::endl ;
}


Exceptions::FileReadOpenError::FileReadOpenError( const Ast::SourceLocation & _loc, RefCountPtr< const char > _filename, const std::string * sourceDir, const std::list< std::string > * searchPath, Type _type )
	: loc( _loc ), filename( _filename ), type( _type ), sourceDir_( sourceDir ), searchPath_( searchPath )
{ }

Exceptions::FileReadOpenError::~FileReadOpenError( )
{ }

void
Exceptions::FileReadOpenError::display( std::ostream & os ) const
{
	switch( type )
		{
		case OPEN:
			os << loc << locsep << "Could not open file: " << filename ;
			break;
		case STAT:
			os << loc << locsep << "Could not stat() file: " << filename ;
			break;
		default:
			os << loc << locsep << "Internal error when displaying FileReadOpenError on file: " << filename ;
		}
	if( searchPath_ != 0 )
		{
			os << " searching \"" ;
			typedef typeof *searchPath_ ListType;
			for( ListType::const_iterator i = searchPath_->begin( ); i != searchPath_->end( ); ++i )
				{
					if( i != searchPath_->begin( ) )
						{
							os << ":" ;
						}
					if( sourceDir_ != 0 && (*i)[0] != '/' )
						{
							os << *sourceDir_ ;
						}
					os << *i ;
				}
			os << "\"" ;
		}
	os << std::endl ;
}


Exceptions::FileWriteOpenError::FileWriteOpenError( const Ast::SourceLocation & _loc, RefCountPtr< const char > _filename, const char * purpose )
	: loc( _loc ), filename( _filename ), purpose_( purpose )
{ }

Exceptions::FileWriteOpenError::~FileWriteOpenError( )
{ }

void
Exceptions::FileWriteOpenError::display( std::ostream & os ) const
{
	os << loc << locsep << "Could not open " ;
	if( purpose_ != 0 )
		{
			os << purpose_ ;
		}
	os << " file for write: " << filename ;
	os << std::endl ;
}


Exceptions::TeXSetupTooLate::TeXSetupTooLate( const Ast::SourceLocation & _loc )
	: loc( _loc )
{ }

Exceptions::TeXSetupTooLate::~TeXSetupTooLate( )
{ }

void
Exceptions::TeXSetupTooLate::display( std::ostream & os ) const
{
	os << loc << locsep << "It is too late to make modifications to the TeX context." << std::endl ;
}


Exceptions::EmptyFinalPicture::EmptyFinalPicture( )
{ }

Exceptions::EmptyFinalPicture::~EmptyFinalPicture( )
{ }

void
Exceptions::EmptyFinalPicture::display( std::ostream & os ) const
{
	os << "Nothing was ever put in the global #" << Lang::CATALOG_ID << " or drawn to the global #" << Lang::CANVAS_ID << "." << std::endl ;
}


Exceptions::TeXSetupHasChanged::TeXSetupHasChanged( )
	: Shapes::Exceptions::InternalError( "The TeX context check failed while loading fresh labels." )
{ }

Exceptions::TeXSetupHasChanged::~TeXSetupHasChanged( )
{ }


Exceptions::TypeMismatch::TypeMismatch( const Ast::SourceLocation & loc, RefCountPtr< const char > _valueType, RefCountPtr< const char > _expectedType )
	: Exceptions::RuntimeError( loc ), hint_( 0 ), valueType( _valueType ), expectedType( _expectedType )
{ }

Exceptions::TypeMismatch::TypeMismatch( const Ast::SourceLocation & loc, const char * hint, RefCountPtr< const char > _valueType, RefCountPtr< const char > _expectedType )
	: Exceptions::RuntimeError( loc ), hint_( hint ), valueType( _valueType ), expectedType( _expectedType )
{ }

Exceptions::TypeMismatch::~TypeMismatch( )
{ }

void
Exceptions::TypeMismatch::display( std::ostream & os ) const
{
	if( hint_ != 0 )
		{
			os << hint_ << ": " ;
		}
	os << "Expected " << expectedType << ", got a " << valueType << std::endl ;
}

Exceptions::PDFVersionError::PDFVersionError( SimplePDF::PDF_Version::Version version, SimplePDF::PDF_Version::Version required, const RefCountPtr< const char > & msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), version_( version ), required_( required ), msgMem_( msg ), msg_( msgMem_.getPtr( ) )
{ }

Exceptions::PDFVersionError::PDFVersionError( SimplePDF::PDF_Version::Version version, SimplePDF::PDF_Version::Version required, const char * msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), version_( version ), required_( required ), msgMem_( NullPtr< const char >( ) ), msg_( msg )
{ }

Exceptions::PDFVersionError::~PDFVersionError( )
{ }

void
Exceptions::PDFVersionError::display( std::ostream & os ) const
{
	os << SimplePDF::PDF_Version::toString( required_ ) << " error: " << msg_ << std::endl ;
}


Exceptions::RedefiningLexical::RedefiningLexical( const char * _id )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), id( _id )
{ }

Exceptions::RedefiningLexical::~RedefiningLexical( )
{ }

void
Exceptions::RedefiningLexical::display( std::ostream & os ) const
{
	os << "Redefining " << id << std::endl ;
}


Exceptions::RedefiningDynamic::RedefiningDynamic( const char * _id )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), id( _id )
{ }

Exceptions::RedefiningDynamic::~RedefiningDynamic( )
{ }

void
Exceptions::RedefiningDynamic::display( std::ostream & os ) const
{
	os << "Redefining " << "@" << id << std::endl ;
}


Exceptions::ConditionTypeMismatch::ConditionTypeMismatch( RefCountPtr< const char > _valueType )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), valueType( _valueType )
{ }

Exceptions::ConditionTypeMismatch::~ConditionTypeMismatch( )
{ }

void
Exceptions::ConditionTypeMismatch::display( std::ostream & os ) const
{
	os << "A variable condition check must result in a " << Lang::Boolean::staticTypeName( ) << ", got a " << valueType << std::endl ;
}


Exceptions::VariableTypeMismatch::VariableTypeMismatch( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id, RefCountPtr< const char > _valueType, RefCountPtr< const char > _expectedType )
	: Exceptions::RuntimeError( _loc ), id( _id ), valueType( _valueType ), expectedType( _expectedType )
{ }

Exceptions::VariableTypeMismatch::~VariableTypeMismatch( )
{ }

void
Exceptions::VariableTypeMismatch::display( std::ostream & os ) const
{
	os << "The variable " << id << " was expected to refer to a " << expectedType << ", found a " << valueType << std::endl ;
}


Exceptions::NonObjectMemberAssignment::NonObjectMemberAssignment( const Ast::SourceLocation & _loc, RefCountPtr< const char > _valueType )
	: Exceptions::RuntimeError( _loc ), valueType( _valueType )
{ }

Exceptions::NonObjectMemberAssignment::~NonObjectMemberAssignment( )
{ }

void
Exceptions::NonObjectMemberAssignment::display( std::ostream & os ) const
{
	os << "Only object oriented values have fields that can be assigned.	This value is of type " << valueType << "." << std::endl ;
}


Exceptions::ElementaryTypeWithoutFields::ElementaryTypeWithoutFields( RefCountPtr< const char > _valueType )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), valueType( _valueType )
{ }

Exceptions::ElementaryTypeWithoutFields::~ElementaryTypeWithoutFields( )
{ }

void
Exceptions::ElementaryTypeWithoutFields::display( std::ostream & os ) const
{
	os << "This value is of the elementary type " << valueType << ", which has no fields." << std::endl ;
}


Exceptions::IllegalFinalReference::IllegalFinalReference( RefCountPtr< const char > _valueType, const char * _fieldID )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), valueType( _valueType ), fieldID( _fieldID )
{ }

Exceptions::IllegalFinalReference::~IllegalFinalReference( )
{ }

void
Exceptions::IllegalFinalReference::display( std::ostream & os ) const
{
	os << "Attempt to access field of non-final class " << valueType << " without home class.	Use <HomeClass>#" << fieldID << " instead." << std::endl ;
}


Exceptions::NonExistentMember::NonExistentMember( RefCountPtr< const char > _valueType, const char * _fieldID )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), valueType( _valueType ), fieldID( _fieldID )
{ }

Exceptions::NonExistentMember::~NonExistentMember( )
{ }

void
Exceptions::NonExistentMember::display( std::ostream & os ) const
{
	os << "Class " << valueType << " has no (non-private) field called " << fieldID << "." << std::endl ;
}


Exceptions::NonExistentPosition::NonExistentPosition( size_t pos, size_t maxPos )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), pos_( pos ), maxPos_( maxPos )
{ }

Exceptions::NonExistentPosition::~NonExistentPosition( )
{ }

void
Exceptions::NonExistentPosition::display( std::ostream & os ) const
{
	os << "Referencing field at position " << pos_ << " is an error since the user struct has only " << maxPos_ << " ordered fields." << std::endl ;
}


Exceptions::ProtectedMemberPublicScope::ProtectedMemberPublicScope( RefCountPtr< const char > _valueType, const char * _fieldID )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), valueType( _valueType ), fieldID( _fieldID )
{ }

Exceptions::ProtectedMemberPublicScope::~ProtectedMemberPublicScope( )
{ }

void
Exceptions::ProtectedMemberPublicScope::display( std::ostream & os ) const
{
	os << "Trying to access protected member " << fieldID << " of class " << valueType << " via public instance." << std::endl ;
}


Exceptions::MemberNotAssignable::MemberNotAssignable( RefCountPtr< const char > _valueType, const char * _fieldID, RefCountPtr< const char > _scope )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), valueType( _valueType ), fieldID( _fieldID ), scope( _scope )
{ }

Exceptions::MemberNotAssignable::~MemberNotAssignable( )
{ }

void
Exceptions::MemberNotAssignable::display( std::ostream & os ) const
{
	os << "In " << scope << " scope of " << valueType << " :	The member " << fieldID << " is not assignable." << std::endl ;
}


Exceptions::NotApplicable::NotApplicable( )
{ }

Exceptions::NotApplicable::~NotApplicable( )
{ }

void
Exceptions::NotApplicable::display( std::ostream & os ) const
{
	os << "An operator was not applicable.	It is an internal error that no better error message is given." << std::endl ;
}


Exceptions::UnaryPrefixNotApplicable::UnaryPrefixNotApplicable( const Ast::SourceLocation & _loc, const Ast::Expression * _expr, RefCountPtr< const char > _valueType )
	: Exceptions::RuntimeError( _loc ), expr( _expr ), valueType( _valueType ), operatorSymbol( "< ? >" )
{ }

Exceptions::UnaryPrefixNotApplicable::~UnaryPrefixNotApplicable( )
{ }

void
Exceptions::UnaryPrefixNotApplicable::setOperatorSymbol( const char * _operatorSymbol )
{
	operatorSymbol = _operatorSymbol;
}

void
Exceptions::UnaryPrefixNotApplicable::display( std::ostream & os ) const
{
	if( expr != 0 )
		{
			os << "(" << valueType << ") Operator not defined for operand at " << expr->loc( ) << std::endl ;
		}
	else
		{
			os << "Operator " << operatorSymbol << " is not defined for the operand of type " << valueType << "." << std::endl ;
		}
}


Exceptions::UnaryPostfixNotApplicable::UnaryPostfixNotApplicable( const Ast::SourceLocation & _loc, const Ast::Expression * _expr, RefCountPtr< const char > _valueType )
	: Exceptions::RuntimeError( _loc ), expr( _expr ), valueType( _valueType ), operatorSymbol( "< ? >" )
{ }

Exceptions::UnaryPostfixNotApplicable::~UnaryPostfixNotApplicable( )
{ }

void
Exceptions::UnaryPostfixNotApplicable::setOperatorSymbol( const char * _operatorSymbol )
{
	operatorSymbol = _operatorSymbol;
}

void
Exceptions::UnaryPostfixNotApplicable::display( std::ostream & os ) const
{
	if( expr != 0 )
		{
			os << "(" << valueType << ") Operator not defined for operand at " << expr->loc( ) << std::endl ;
		}
	else
		{
			os << "Operator " << operatorSymbol << " is not defined for the operand of type " << valueType << "." << std::endl ;
		}
}


Exceptions::BinaryInfixNotApplicable::BinaryInfixNotApplicable( const Ast::SourceLocation & _loc, const Ast::Expression * _expr1, RefCountPtr< const char > _valueType1, const Ast::Expression * _expr2, RefCountPtr< const char > _valueType2 )
	: Exceptions::RuntimeError( _loc ), expr1( _expr1 ), valueType1( _valueType1 ), expr2( _expr2 ), valueType2( _valueType2 ), operatorSymbol( "< ? >" )
{ }

Exceptions::BinaryInfixNotApplicable::~BinaryInfixNotApplicable( )
{ }

void
Exceptions::BinaryInfixNotApplicable::setOperatorSymbol( const char * _operatorSymbol )
{
	operatorSymbol = _operatorSymbol;
}

void
Exceptions::BinaryInfixNotApplicable::display( std::ostream & os ) const
{
	if( expr1 != 0 && expr2 != 0 )
		{
			os << "(" << valueType1 << "," << valueType2 << ") Operator not defined for operands at " << expr1->loc( ) << ", " << expr2->loc( ) << std::endl ;
		}
	else
		{
			os << "Operator " << operatorSymbol << " is not defined for the operand combination (" << valueType1 << "," << valueType2 << ")" << std::endl ;
		}
}


Exceptions::ProhibitedTypeChange::ProhibitedTypeChange( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id, RefCountPtr< const char > _oldType, RefCountPtr< const char > _newType )
	: Exceptions::RuntimeError( _loc ), id( _id ), oldType( _oldType ), newType( _newType )
{ }

Exceptions::ProhibitedTypeChange::~ProhibitedTypeChange( )
{ }

void
Exceptions::ProhibitedTypeChange::display( std::ostream & os ) const
{
	os << "The type of " << id << " is locked to " << oldType << ", but the new value is of type " << newType << "." << std::endl ;
}


Exceptions::LookupUnknown::LookupUnknown( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id, Type type )
	: Exceptions::StaticInconsistency( _loc ), id( _id ), type_( type )
{ }

Exceptions::LookupUnknown::~LookupUnknown( )
{ }

void
Exceptions::LookupUnknown::display( std::ostream & os ) const
{
	switch( type_ )
		{
		case VARIABLE:
			os << "The variable " << id << " is unbound." << std::endl ;
			break;
		case STATE:
			os << "There is no state called " << id << " around." << std::endl ;
			break;
		case DYNAMIC_VARIABLE:
			os << "There is no dynamic variable called " << id << " around." << std::endl ;
			break;
		case DYNAMIC_STATE:
			os << "There is no dynamic state called " << id << " around." << std::endl ;
			break;
		case TYPE:
			os << "There is no type called " << id << " around." << std::endl ;
			break;
		default:
			os << "<?ERROR?" ;
		}
}


Exceptions::StateBeyondFunctionBoundary::StateBeyondFunctionBoundary( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id )
	: Exceptions::StaticInconsistency( _loc ), id( _id )
{ }

Exceptions::StateBeyondFunctionBoundary::~StateBeyondFunctionBoundary( )
{ }

void
Exceptions::StateBeyondFunctionBoundary::display( std::ostream & os ) const
{
	os << "The state " << id << " resides outside a function boundary." << std::endl ;
}


Exceptions::IntroducingExistingUnit::IntroducingExistingUnit( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id )
	: Exceptions::StaticInconsistency( _loc ), id( _id )
{ }

Exceptions::IntroducingExistingUnit::~IntroducingExistingUnit( )
{ }

void
Exceptions::IntroducingExistingUnit::display( std::ostream & os ) const
{
	os << "Inconsistent definition of the existing unit " << id << "." << std::endl ;
}


Exceptions::LookupUnknownUnit::LookupUnknownUnit( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id )
	: Exceptions::StaticInconsistency( _loc ), id( _id )
{ }

Exceptions::LookupUnknownUnit::~LookupUnknownUnit( )
{ }

void
Exceptions::LookupUnknownUnit::display( std::ostream & os ) const
{
	os << "The unit " << id << " is unbound" << std::endl ;
}


Exceptions::OutOfRange::OutOfRange( RefCountPtr< const char > msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::OutOfRange::OutOfRange( const char * msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::OutOfRange::OutOfRange( const Ast::SourceLocation & _loc, RefCountPtr< const char > msg )
	: Exceptions::RuntimeError( _loc ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::OutOfRange::OutOfRange( const Ast::SourceLocation & _loc, const char * msg )
	: Exceptions::RuntimeError( _loc ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::OutOfRange::~OutOfRange( )
{ }

void
Exceptions::OutOfRange::display( std::ostream & os ) const
{
	os << "Out-of-range error: " << msg_ << std::endl ;
}


Exceptions::NonVoidStatement::NonVoidStatement( const Ast::SourceLocation & _loc, RefCountPtr< const Lang::Value > _val )
	: Exceptions::RuntimeError( _loc ), val( _val )
{ }

Exceptions::NonVoidStatement::~NonVoidStatement( )
{ }

void
Exceptions::NonVoidStatement::display( std::ostream & os ) const
{
	os << "No implicit ignore of non-void value (of type " << val->getTypeName( ) <<	")" << std::endl ;
}

// Exceptions::RuntimeError::RuntimeError( )
//	 : loc_( "< unlocated runtime error >" )
// { }

Exceptions::RuntimeError::~RuntimeError( )
{ }

Exceptions::RuntimeError::RuntimeError( const Ast::SourceLocation & loc )
	: loc_( loc )
{ }

Exceptions::RuntimeError::RuntimeError( Ast::Expression * expr )
	: loc_( expr->loc( ) )
{ }

const Ast::SourceLocation &
Exceptions::RuntimeError::getLoc( ) const
{
	return loc_;
}


Exceptions::UserError::UserError( RefCountPtr< const char > _msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg( _msg )
{ }

Exceptions::UserError::~UserError( )
{ }

void
Exceptions::UserError::display( ostream & os ) const
{
	os << "User error: " << msg << std::endl ;
}


Exceptions::InternalError::InternalError( const Ast::SourceLocation & loc, RefCountPtr< const char > msg )
	: Exceptions::RuntimeError( loc ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::InternalError::InternalError( const Ast::SourceLocation & loc, const char * msg )
	: Exceptions::RuntimeError( loc ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::InternalError::InternalError( RefCountPtr< const char > msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::InternalError::InternalError( const char * msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::InternalError::InternalError( const std::ostringstream & msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( strdup( msg.str( ).c_str( ) ) ), msgMem_( msg_ )
{ }

Exceptions::InternalError::~InternalError( )
{ }

void
Exceptions::InternalError::display( ostream & os ) const
{
	os << "Internal error: " << msg_ << std::endl ;
}


Exceptions::ExternalError::ExternalError( RefCountPtr< const char > msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::ExternalError::ExternalError( const char * msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::ExternalError::~ExternalError( )
{ }

void
Exceptions::ExternalError::display( ostream & os ) const
{
	os << "External error: " << msg_ << std::endl ;
}

Exceptions::ExternalError *
Exceptions::ExternalError::clone( ) const
{
	return new Exceptions::ExternalError( msgMem_ );
}

Exceptions::UserArityMismatch::UserArityMismatch( const Ast::SourceLocation _formalsLoc, size_t _functionArity, size_t _callArity, Type type )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), formalsLoc( _formalsLoc ), functionArity( _functionArity ), callArity( _callArity ), type_( type )
{ }

Exceptions::UserArityMismatch::~UserArityMismatch( )
{ }

void
Exceptions::UserArityMismatch::display( std::ostream & os ) const
{
	os << "Function with formals at " << formalsLoc << " expects " << functionArity ;
	switch( type_ )
		{
		case VARIABLE:
			os << " value arguments" ;
			break;
		case STATE:
			os << " state arguments" ;
			break;
		default:
			os << " ?ERROR?" ;
		}
	os << ", not " << callArity << std::endl ;
}

Exceptions::SinkRequired::SinkRequired( const Ast::SourceLocation loc, size_t formalsArity, size_t callArity )
	: Exceptions::RuntimeError( loc ), formalsArity_( formalsArity ), callArity_( callArity )
{ }

Exceptions::SinkRequired::~SinkRequired( )
{ }

void
Exceptions::SinkRequired::display( std::ostream & os ) const
{
	os << "Formals at " << loc_ << " contains no sink, so passing " << callArity_ << " ordered arguments is " << callArity_ - formalsArity_ << " too many." << std::endl ;
}


Exceptions::NamedFormalMismatch::NamedFormalMismatch( const Ast::SourceLocation _formalsLoc, RefCountPtr< const char > _name, Exceptions::NamedFormalMismatch::Type type )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), formalsLoc( _formalsLoc ), name( _name ), type_( type )
{ }

Exceptions::NamedFormalMismatch::~NamedFormalMismatch( )
{ }

void
Exceptions::NamedFormalMismatch::display( std::ostream & os ) const
{
	os << "Function with formals at " << formalsLoc << " has no named ";
	switch( type_ )
		{
		case VARIABLE:
			os << "variable" ;
			break;
		case STATE:
			os << "state" ;
			break;
		default:
			os << "<?ERROR?" ;
		}
	os << " called " << name << "." << std::endl ;
}


Exceptions::NamedFormalAlreadySpecified::NamedFormalAlreadySpecified( const Ast::SourceLocation _formalsLoc, RefCountPtr< const char > _name, size_t _pos, Exceptions::NamedFormalAlreadySpecified::Type type )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), formalsLoc( _formalsLoc ), name( _name ), pos( _pos ), type_( type	)
{ }

Exceptions::NamedFormalAlreadySpecified::~NamedFormalAlreadySpecified( )
{ }

void
Exceptions::NamedFormalAlreadySpecified::display( std::ostream & os ) const
{
	os << "The formal " ;
	switch( type_ )
		{
		case VARIABLE:
			os << "variable" ;
			break;
		case STATE:
			os << "state" ;
			break;
		default:
			os << "<?ERROR?" ;
		}
	os << " named " << name << ", defined at " << formalsLoc << " is already defined by order, at position " << pos << "." << std::endl ;
}

Exceptions::MissingArguments::MissingArguments( const Ast::SourceLocation _formalsLoc, std::map< size_t, RefCountPtr< const char > > * _missingVariables, std::map< size_t, RefCountPtr< const char > > * _missingStates )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), formalsLoc( _formalsLoc ), missingVariables( _missingVariables ), missingStates( _missingStates )
{ }

Exceptions::MissingArguments::~MissingArguments( )
{
	if( missingVariables != 0 )
		{
			delete missingVariables;
		}
	if( missingStates != 0 )
		{
			delete missingStates;
		}
}

void
Exceptions::MissingArguments::display( std::ostream & os ) const
{
	os << "Among the formals at " << formalsLoc ;
	if( missingVariables != 0 )
		{
			os << ", the following variables are missing:" ;
			typedef typeof *missingVariables MapType;
			for( MapType::const_iterator i = missingVariables->begin( ); i != missingVariables->end( ); ++i )
				{
					if( i != missingVariables->begin( ) )
						{
							os << "," ;
						}
					os << " (" << i->first << ")" << i->second.getPtr( ) ;
				}
		}
	if( missingStates != 0 )
		{
			os << ", " ;
			if( missingVariables != 0 )
				{
					os << "and " ;
				}
			os << "the following states are missing:" ;
			typedef typeof *missingStates MapType;
			for( MapType::const_iterator i = missingStates->begin( ); i != missingStates->end( ); ++i )
				{
					if( i != missingStates->begin( ) )
						{
							os << "," ;
						}
					os << " (" << i->first << ")" << i->second.getPtr( ) ;
				}
		}
	os << "." << std::endl ;
}


Exceptions::CoreArityMismatch::CoreArityMismatch( const char * _title, size_t _functionArity, size_t _callArity )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title ), titleMem( NullPtr< const char >( ) ), functionArityLow( _functionArity ), functionArityHigh( _functionArity ), callArity( _callArity )
{ }

Exceptions::CoreArityMismatch::CoreArityMismatch( const char * _title, size_t _functionArityLow, size_t _functionArityHigh, size_t _callArity )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title ), titleMem( NullPtr< const char >( ) ), functionArityLow( _functionArityLow ), functionArityHigh( _functionArityHigh ), callArity( _callArity )
{ }

Exceptions::CoreArityMismatch::CoreArityMismatch( const RefCountPtr< const char > & _title, size_t _functionArity, size_t _callArity )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title.getPtr( ) ), titleMem( _title ), functionArityLow( _functionArity ), functionArityHigh( _functionArity ), callArity( _callArity )
{ }

Exceptions::CoreArityMismatch::CoreArityMismatch( const RefCountPtr< const char > & _title, size_t _functionArityLow, size_t _functionArityHigh, size_t _callArity )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title.getPtr( ) ), titleMem( _title ), functionArityLow( _functionArityLow ), functionArityHigh( _functionArityHigh ), callArity( _callArity )
{ }

Exceptions::CoreArityMismatch::~CoreArityMismatch( )
{ }

void
Exceptions::CoreArityMismatch::display( std::ostream & os ) const
{
	if( functionArityLow == functionArityHigh )
		{
			os << "Core function " << title << " expects " << functionArityLow << " arguments, not " << callArity << std::endl ;
		}
	else
		{
			os << "Core function " << title << " expects [" << functionArityLow << ", " << functionArityHigh << "] arguments, not " << callArity << std::endl ;
		}
}


Exceptions::CoreNoNamedFormals::CoreNoNamedFormals( const char * _title )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title ), titleMem( NullPtr< const char >( ) )
{ }

Exceptions::CoreNoNamedFormals::CoreNoNamedFormals( const RefCountPtr< const char > & _title )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title.getPtr( ) ), titleMem( _title )
{ }

Exceptions::CoreNoNamedFormals::~CoreNoNamedFormals( )
{ }

void
Exceptions::CoreNoNamedFormals::display( std::ostream & os ) const
{
	os << "Core function " << title << " does not accept named arguments." << std::endl ;
}


Exceptions::CoreTypeMismatch::CoreTypeMismatch( const Ast::SourceLocation & callLoc,
																						 const char * title,
																						 const Ast::SourceLocation & argLoc,
																						 RefCountPtr< const char > valueType,
																						 RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title ),
		titleMem_( NullPtr< const char >( ) ),
		argLoc_( argLoc ),
		valueType_( valueType ),
		expectedType_( expectedType )
{ }

Exceptions::CoreTypeMismatch::CoreTypeMismatch( const Ast::SourceLocation & callLoc,
																						 RefCountPtr< const char > title,
																						 const Ast::SourceLocation & argLoc,
																						 RefCountPtr< const char > valueType,
																						 RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title.getPtr( ) ),
		titleMem_( title ),
		argLoc_( argLoc ),
		valueType_( valueType ),
		expectedType_( expectedType )
{ }

Exceptions::CoreTypeMismatch::CoreTypeMismatch( const Ast::SourceLocation & callLoc,
																								const char * title,
																								Kernel::Arguments & args,
																								size_t argNo,
																								RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title ),
		titleMem_( NullPtr< const char >( ) ),
		argLoc_( args.getLoc( argNo ) ),
		valueType_( args.getValue( argNo )->getTypeName( ) ),
		expectedType_( expectedType )
{ }

Exceptions::CoreTypeMismatch::CoreTypeMismatch( const Ast::SourceLocation & callLoc,
																						 RefCountPtr< const char > title,
																						 Kernel::Arguments & args,
																						 size_t argNo,
																						 RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title.getPtr( ) ),
		titleMem_( title ),
		argLoc_( args.getLoc( argNo ) ),
		valueType_( args.getValue( argNo )->getTypeName( ) ),
		expectedType_( expectedType )
{ }

Exceptions::CoreTypeMismatch::~CoreTypeMismatch( )
{ }

void
Exceptions::CoreTypeMismatch::display( std::ostream & os ) const
{
	os << "Core function " << title_ << ", argument ";
	//	if( argName_ != 0 )
	//		{
	//			os << "\"" << argName_ << "\" " ;
	//		}
	os << "at " << argLoc_ << ": expected a " << expectedType_ << ", got a " << valueType_ << std::endl ;
}


Exceptions::CoreStateTypeMismatch::CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
																						 const char * title,
																						 const Ast::SourceLocation & argLoc,
																						 RefCountPtr< const char > valueType,
																						 RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title ),
		titleMem_( NullPtr< const char >( ) ),
		argLoc_( argLoc ),
		valueType_( valueType ),
		expectedType_( expectedType )
{ }

Exceptions::CoreStateTypeMismatch::CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
																						 RefCountPtr< const char > title,
																						 const Ast::SourceLocation & argLoc,
																						 RefCountPtr< const char > valueType,
																						 RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title.getPtr( ) ),
		titleMem_( title ),
		argLoc_( argLoc ),
		valueType_( valueType ),
		expectedType_( expectedType )
{ }

Exceptions::CoreStateTypeMismatch::CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
																								const char * title,
																								Kernel::Arguments & args,
																								size_t argNo,
																								RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title ),
		titleMem_( NullPtr< const char >( ) ),
		argLoc_( args.getStateLoc( argNo ) ),
		valueType_( args.getState( argNo )->getTypeName( ) ),
		expectedType_( expectedType )
{ }

Exceptions::CoreStateTypeMismatch::CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
																						 RefCountPtr< const char > title,
																						 Kernel::Arguments & args,
																						 size_t argNo,
																						 RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title.getPtr( ) ),
		titleMem_( title ),
		argLoc_( args.getStateLoc( argNo ) ),
		valueType_( args.getState( argNo )->getTypeName( ) ),
		expectedType_( expectedType )
{ }

Exceptions::CoreStateTypeMismatch::~CoreStateTypeMismatch( )
{ }

void
Exceptions::CoreStateTypeMismatch::display( std::ostream & os ) const
{
	os << "Core function " << title_ << ", state ";
	//	if( argName_ != 0 )
	//		{
	//			os << "\"" << argName_ << "\" " ;
	//		}
	os << "at " << argLoc_ << ": expected a " << expectedType_ << ", got a " << valueType_ << std::endl ;
}


Exceptions::CoreDynamicTypeMismatch::CoreDynamicTypeMismatch( const Ast::SourceLocation & callLoc,
																															const char * title,
																															const char * id,
																															RefCountPtr< const char > valueType,
																															RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( callLoc ),
		title_( title ),
		titleMem_( NullPtr< const char >( ) ),
		id_( id ),
		idMem_( NullPtr< const char >( ) ),
		valueType_( valueType ),
		expectedType_( expectedType )
{ }

Exceptions::CoreDynamicTypeMismatch::~CoreDynamicTypeMismatch( )
{ }

void
Exceptions::CoreDynamicTypeMismatch::display( std::ostream & os ) const
{
	os << "Core function " << title_ << " encountered a type mismatch in the dynamic variable @" << id_
		 << ": expected a " << expectedType_ << ", got a " << valueType_ << std::endl ;
}

Exceptions::ContinuationTypeMismatch::ContinuationTypeMismatch( const Kernel::Continuation * locationCont,
																																RefCountPtr< const char > valueType,
																																RefCountPtr< const char > expectedType )
	: Exceptions::RuntimeError( locationCont->traceLoc( ) ),
		valueType_( valueType ),
		expectedType_( expectedType )
{ }

Exceptions::ContinuationTypeMismatch::~ContinuationTypeMismatch( )
{ }

void
Exceptions::ContinuationTypeMismatch::display( std::ostream & os ) const
{
	os << "The continuation expected a " << expectedType_ << ", got a " << valueType_ << "." << std::endl ;
}


Exceptions::EmptyApplication::EmptyApplication( )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION )
{ }

Exceptions::EmptyApplication::~EmptyApplication( )
{ }

void
Exceptions::EmptyApplication::display( std::ostream & os ) const
{
	os << "Empty application." << std::endl ;
}


Exceptions::CoreOutOfRange::CoreOutOfRange( const char * _title, Kernel::Arguments & args, size_t argNo, RefCountPtr< const char > msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title ), argLoc_( args.getLoc( argNo ) ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::CoreOutOfRange::CoreOutOfRange( const char * _title, Kernel::Arguments & args, size_t argNo, const char * msg )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), title( _title ), argLoc_( args.getLoc( argNo ) ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::CoreOutOfRange::~CoreOutOfRange( )
{ }

void
Exceptions::CoreOutOfRange::display( std::ostream & os ) const
{
	os << "Core function " << title << ": ";
	os << "Argument at " << argLoc_ << " is out of range; " << msg_ << std::endl ;
}

Exceptions::CoreRequirement::CoreRequirement( RefCountPtr< const char > msg, const char * _title, const Ast::SourceLocation & callLoc )
	: Exceptions::RuntimeError( callLoc ), title( _title ), msg_( msg.getPtr( ) ), msgMem_( msg )
{ }

Exceptions::CoreRequirement::CoreRequirement( const char * msg, const char * _title, const Ast::SourceLocation & callLoc )
	: Exceptions::RuntimeError( callLoc ), title( _title ), msg_( msg ), msgMem_( NullPtr< const char >( ) )
{ }

Exceptions::CoreRequirement::~CoreRequirement( )
{ }

void
Exceptions::CoreRequirement::display( std::ostream & os ) const
{
	os << "Core function " << title << ": " << msg_ << std::endl ;
}


Exceptions::TeXLabelError::TeXLabelError( bool quoted, const char * region, RefCountPtr< const char > summary, RefCountPtr< const char > details, const Ast::SourceLocation & loc )
	: Exceptions::RuntimeError( loc ), strLoc_( loc ), quoted_( quoted ), region_( region ), summary_( summary ), details_( details )
{ }

Exceptions::TeXLabelError::~TeXLabelError( )
{ }

void
Exceptions::TeXLabelError::display( ostream & os ) const
{
	if( quoted_ )
		{
			os << "TeX error in " << region_ ;
		}
	else
		{
			os << "Error in TeX " << region_ ;
		}
	if( ! strLoc_.isUnknown( ) )
		{
			os << " at " << strLoc_ ;
		}
	os << ": " ;
	if( quoted_ )
		{
			os << "\"" ;
		}
	os << summary_ ;
	if( quoted_ )
		{
			os << "\"" ;
		}
	os << std::endl ;
	if( details_ != NullPtr< const char >( ) )
		{
			Exceptions::prefixEachLine( additionalLinesPrefix, details_, os );
		}
}


Exceptions::StaticTeXLabelError::StaticTeXLabelError( bool quoted, const char * region, RefCountPtr< const char > summary, RefCountPtr< const char > details, const Ast::SourceLocation & loc )
	: Exceptions::StaticInconsistency( loc ), quoted_( quoted ), region_( region ), summary_( summary ), details_( details )
{ }

Exceptions::StaticTeXLabelError::~StaticTeXLabelError( )
{ }

void
Exceptions::StaticTeXLabelError::display( ostream & os ) const
{
	if( quoted_ )
		{
			os << "TeX error in " << region_ ;
		}
	else
		{
			os << "Error in TeX " << region_ ;
		}
	os << ": " ;
	if( quoted_ )
		{
			os << "\"" ;
		}
	os << summary_ ;
	if( quoted_ )
		{
			os << "\"" ;
		}
	os << std::endl ;
	if( details_ != NullPtr< const char >( ) )
		{
			Exceptions::prefixEachLine( additionalLinesPrefix, details_, os );
		}
}


Exceptions::InstantiatingAbstractClass::InstantiatingAbstractClass( RefCountPtr< const Lang::Class > _cls )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls )
{ }

Exceptions::InstantiatingAbstractClass::~InstantiatingAbstractClass( )
{ }

void
Exceptions::InstantiatingAbstractClass::display( ostream & os ) const
{
	os << "Trying to instantiate abstract class: " << cls->getPrettyName( ) << std::endl ;
}


Exceptions::FailedToDeclareClassAbstract::FailedToDeclareClassAbstract( RefCountPtr< const Lang::Class > _cls, const Shapes::Ast::ClassFunction * _classExpr )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), classExpr( _classExpr )
{ }

Exceptions::FailedToDeclareClassAbstract::~FailedToDeclareClassAbstract( )
{ }

void
Exceptions::FailedToDeclareClassAbstract::display( ostream & os ) const
{
	os << classExpr->loc( ) << " " << "Class " << cls->getPrettyName( ) << " has abstract methods but is not declared abstract: " ;
	cls->showAbstractSet( os );
	os << std::endl ;
}


Exceptions::RepeatedImmediateParent::RepeatedImmediateParent( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), parent( _parent )
{ }

Exceptions::RepeatedImmediateParent::~RepeatedImmediateParent( )
{ }

void
Exceptions::RepeatedImmediateParent::display( ostream & os ) const
{
	os << "When creating the class " << cls->getPrettyName( ) << ", the following class is a repeated immediate parent: " << parent->getPrettyName( ) << std::endl ;
}


Exceptions::OverridingNonParent::OverridingNonParent( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), parent( _parent )
{ }

Exceptions::OverridingNonParent::~OverridingNonParent( )
{ }

void
Exceptions::OverridingNonParent::display( ostream & os ) const
{
	os << "The class " << cls->getPrettyName( ) << " cannot override methods from the class " << parent->getPrettyName( ) << ", since the latter is not a parent of the former." << std::endl ;
}


Exceptions::InheritingFinal::InheritingFinal( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), parent( _parent )
{ }

Exceptions::InheritingFinal::~InheritingFinal( )
{ }

void
Exceptions::InheritingFinal::display( ostream & os ) const
{
	os << "When creating the class " << cls->getPrettyName( ) << ", trying to inherit from final class: " << parent->getPrettyName( ) << std::endl ;
}


Exceptions::OverridingUndeclaredMethod::OverridingUndeclaredMethod( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent, const char * _id )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), parent( _parent ), id( _id )
{ }

Exceptions::OverridingUndeclaredMethod::~OverridingUndeclaredMethod( )
{ }

void
Exceptions::OverridingUndeclaredMethod::display( ostream & os ) const
{
	os << "In override declaration in class " << cls->getPrettyName( ) << ": there is no method called " << id << " in class " << parent->getPrettyName( ) << " to override." << std::endl ;
}


Exceptions::OverridingFinalMethod::OverridingFinalMethod( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent, const char * _id )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), parent( _parent ), id( _id )
{ }

Exceptions::OverridingFinalMethod::~OverridingFinalMethod( )
{ }

void
Exceptions::OverridingFinalMethod::display( ostream & os ) const
{
	os << "In override declaration in class " << cls->getPrettyName( ) << ": The method " << id << " in class " << parent->getPrettyName( ) << " is final." << std::endl ;
}


Exceptions::IllegalRepeatedBase::IllegalRepeatedBase( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), parent( _parent )
{ }

Exceptions::IllegalRepeatedBase::~IllegalRepeatedBase( )
{ }

void
Exceptions::IllegalRepeatedBase::display( ostream & os ) const
{
	os << "When creating the class " << cls->getPrettyName( ) << ", the following repeated base class may not be repeated: " << parent->getPrettyName( ) << std::endl ;
}


Exceptions::AmbiguousInheritedMethod::AmbiguousInheritedMethod( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const char > _id, std::set< RefCountPtr< const Lang::Class > > _parents )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), id( _id ), parents( _parents )
{ }

Exceptions::AmbiguousInheritedMethod::~AmbiguousInheritedMethod( )
{ }

void
Exceptions::AmbiguousInheritedMethod::display( ostream & os ) const
{
	os << "In class " << cls->getPrettyName( ) << ": the method name " << id << "	is ambiguous, as it is inherited from more than one class:" ;
	for( std::set< RefCountPtr< const Lang::Class > >::const_iterator i = parents.begin( ); i != parents.end( ); ++i )
		{
			os << " " << (*i)->getPrettyName( ) ;
		}
	os << std::endl ;
}


Exceptions::MisplacedSuperReference::MisplacedSuperReference( const Ast::SourceLocation & _loc )
	: Exceptions::RuntimeError( _loc )
{ }

Exceptions::MisplacedSuperReference::~MisplacedSuperReference( )
{ }

void
Exceptions::MisplacedSuperReference::display( std::ostream & os ) const
{
	os << "Misplaced reference to parent instance." << std::endl ;
}


Exceptions::SuperReferenceClassNotParent::SuperReferenceClassNotParent( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), parent( _parent )
{ }

Exceptions::SuperReferenceClassNotParent::~SuperReferenceClassNotParent( )
{ }

void
Exceptions::SuperReferenceClassNotParent::display( std::ostream & os ) const
{
	os << "When referencing super instance, " << parent->getPrettyName( ) << " is not a parent of " << cls->getPrettyName( ) << "." << std::endl ;
}


Exceptions::NoSuchMethod::NoSuchMethod( RefCountPtr< const Lang::Class > _cls, const Kernel::MethodId & _method )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), method( _method )
{ }

Exceptions::NoSuchMethod::~NoSuchMethod( )
{ }

void
Exceptions::NoSuchMethod::display( std::ostream & os ) const
{
	os << "Class " << cls->getPrettyName( ) << " has no method called " << method.prettyName( ) << std::endl ;
}


Exceptions::NoSuchLocalMethod::NoSuchLocalMethod( RefCountPtr< const Lang::Class > _cls, const Kernel::MethodId & _method )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), cls( _cls ), method( _method )
{ }

Exceptions::NoSuchLocalMethod::~NoSuchLocalMethod( )
{ }

void
Exceptions::NoSuchLocalMethod::display( std::ostream & os ) const
{
	os << "Class " << cls->getPrettyName( ) << " does not itself define the method " << method.prettyName( ) << std::endl ;
}


Exceptions::MultipleDynamicBind::MultipleDynamicBind( const char * _id, const Ast::SourceLocation & _loc, const Ast::SourceLocation & _prevLoc )
	: Exceptions::RuntimeError( _loc ), id( _id ), prevLoc( _prevLoc )
{ }

Exceptions::MultipleDynamicBind::~MultipleDynamicBind( )
{ }

void
Exceptions::MultipleDynamicBind::display( std::ostream & os ) const
{
	os << "This dynamic binding of " << id << " is merged with the conflicting binding at " << prevLoc << "." << std::endl ;
}


Exceptions::UndefinedEscapeContinuation::UndefinedEscapeContinuation( const char * _id, const Ast::SourceLocation & _loc )
	: Exceptions::RuntimeError( _loc ), id( _id )
{ }

Exceptions::UndefinedEscapeContinuation::~UndefinedEscapeContinuation( )
{ }

void
Exceptions::UndefinedEscapeContinuation::display( std::ostream & os ) const
{
	os << "The escape continuation " << id << " was not defined in the dynamic context." << std::endl ;
}


Exceptions::DeadStateAccess::DeadStateAccess( )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION )
{ }

Exceptions::DeadStateAccess::~DeadStateAccess( )
{ }

void
Exceptions::DeadStateAccess::display( std::ostream & os ) const
{
	os << "Accessing dead state." << std::endl ;
}


Exceptions::UnFreezable::UnFreezable( )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION )
{ }

Exceptions::UnFreezable::~UnFreezable( )
{ }

void
Exceptions::UnFreezable::display( std::ostream & os ) const
{
	os << "The state cannot be frozen." << std::endl ;
}


Exceptions::UnPeekable::UnPeekable( )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION )
{ }

Exceptions::UnPeekable::~UnPeekable( )
{ }

void
Exceptions::UnPeekable::display( std::ostream & os ) const
{
	os << "The state cannot be peeked.	Consider freezing it." << std::endl ;
}


Exceptions::UninitializedAccess::UninitializedAccess( )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION )
{ }

Exceptions::UninitializedAccess::~UninitializedAccess( )
{ }

void
Exceptions::UninitializedAccess::display( std::ostream & os ) const
{
	os << "The accessed variable or state has not been initialized yet." << std::endl ;
}


Exceptions::DtMinError::DtMinError( double _dt )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), dt( _dt )
{ }

Exceptions::DtMinError::~DtMinError( )
{ }

void
Exceptions::DtMinError::display( std::ostream & os ) const
{
	os << "Path segment too long in relation to arcdelta: dt = " << dt << " < " << Computation::the_dtMin << ".	Either increase arcdelta, or inhibit this error." << std::endl ;
}


Exceptions::AffineTransformKillsPlane::AffineTransformKillsPlane( double sigma2 )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), sigma2_( sigma2 )
{ }

Exceptions::AffineTransformKillsPlane::~AffineTransformKillsPlane( )
{ }

void
Exceptions::AffineTransformKillsPlane::display( std::ostream & os ) const
{
	os << "When transforming a plane normal, it was found that the affine transform is too singular, with a second largest singular value of " << sigma2_ << "." << std::endl ;
}

Exceptions::MissingFontMetrics::MissingFontMetrics( RefCountPtr< const char > fontname, const std::list< std::string > * searchPath )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), fontname_( fontname ), searchPath_( searchPath )
{ }

Exceptions::MissingFontMetrics::~MissingFontMetrics( )
{ }

void
Exceptions::MissingFontMetrics::display( std::ostream & os ) const
{
	os << "The font metrics for " << fontname_ << ", named " << fontname_ << ".afm, could not be found in {" ;
	typedef typeof *searchPath_ ListType;
	for( ListType::const_iterator i = searchPath_->begin( ); i != searchPath_->end( ); ++i )
		{
			os << " " << *i ;
		}
	os << " }." << std::endl ;
}


Exceptions::FontMetricsError::FontMetricsError( const RefCountPtr< const char > & fontname, const RefCountPtr< const char > & message )
	: Exceptions::RuntimeError( Ast::THE_UNKNOWN_LOCATION ), fontname_( fontname ), message_( message )
{ }

Exceptions::FontMetricsError::~FontMetricsError( )
{ }

void
Exceptions::FontMetricsError::display( std::ostream & os ) const
{
	os << "There was a problem with the font metrics for " << fontname_ << ": " << message_ << std::endl ;
}


Exceptions::InsertingEmptyPage::InsertingEmptyPage( const Ast::SourceLocation & loc )
	: Exceptions::RuntimeError( loc )
{ }

Exceptions::InsertingEmptyPage::~InsertingEmptyPage( )
{ }

void
Exceptions::InsertingEmptyPage::display( std::ostream & os ) const
{
	os << "Attempt to place empty page in catalog." << std::endl ;
}


Exceptions::UndefinedCrossRef::UndefinedCrossRef( const Ast::SourceLocation & _loc, RefCountPtr< const char > ref )
	: Exceptions::PostCondition( _loc ), ref_( ref )
{ }

Exceptions::UndefinedCrossRef::~UndefinedCrossRef( )
{ }

void
Exceptions::UndefinedCrossRef::display( std::ostream & os ) const
{
	os << "The cross reference \"" << ref_ << "\" is not defined." << std::endl ;
}


void
Exceptions::prefixEachLine( const std::string & prefix, std::istream & src, std::ostream & dst )
{
	std::string line;
	for( std::getline( src, line ); ! src.eof( ); std::getline( src, line ) )
		{
			dst << prefix << line << std::endl ;
		}
}

void
Exceptions::prefixEachLine( const std::string & prefix, const RefCountPtr< const char > & src, std::ostream & dst )
{
	std::istringstream is( src.getPtr( ) );
	Exceptions::prefixEachLine( prefix, is, dst );
}
