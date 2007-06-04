#ifndef metapdfexceptions_h
#define metapdfexceptions_h

#include "MetaPDF_Ast_decls.h"
#include "MetaPDF_Lang_decls.h"
#include "MetaPDF_Kernel_decls.h"
#include "MetaPDF_Exceptions_decls.h"

#include "sourcelocation.h"
#include "methodid.h"
#include "simplepdfo.h"
#include "strrefdup.h" // this is not needed in this file, but it is convenient to get these declarations for users of this file.

#include <exception>
#include <iostream>
#include <sstream>
#include <list>
#include <set>
#include <map>


namespace MetaPDF
{
  namespace Exceptions
  {

    class Exception
    {
    public:
      Exception( );
      virtual ~Exception( );
      virtual void display( std::ostream & os ) const = 0;
    };
    
    class NotImplemented : public Exception
    {
      const char * functionality;
    public:
      NotImplemented( const char * _functionality );
      virtual ~NotImplemented( );
      virtual void display( std::ostream & os ) const;
    };

    class StaticInconsistency : public Exception
    {
    protected:
      Ast::SourceLocation primaryLoc_;
    public:
      StaticInconsistency( Ast::SourceLocation primaryLoc ) : primaryLoc_( primaryLoc ) { }
      virtual ~StaticInconsistency( ) { }
      const Ast::SourceLocation & loc( ) const { return primaryLoc_; }
    };

    class ScannerError : public StaticInconsistency
    {
      RefCountPtr< const char > msg;
    public:
      ScannerError( const Ast::SourceLocation & _loc, RefCountPtr< const char > _msg );
      virtual ~ScannerError( );
      virtual void display( std::ostream & os ) const;
    };

    class ParserError : public StaticInconsistency
    {
      RefCountPtr< const char > msg;
    public:
      ParserError( const Ast::SourceLocation & _loc, RefCountPtr< const char > _msg );
      virtual ~ParserError( );
      virtual void display( std::ostream & os ) const;
    };

    class MemberAlsoAbstract : public StaticInconsistency
    {
      Ast::SourceLocation memberLoc;
      const char * id;
      Ast::SourceLocation abstractLoc;
    public:
      MemberAlsoAbstract( const Ast::SourceLocation & _memberLoc, const char * _id, const Ast::SourceLocation & _abstractLoc );
      virtual ~MemberAlsoAbstract( );
      virtual void display( std::ostream & os ) const;
    };

    class MemberAlreadyDeclared : public StaticInconsistency
    {
      Ast::SourceLocation memberLoc;
      const char * id;
      Ast::SourceLocation oldLoc;
    public:
      MemberAlreadyDeclared( const Ast::SourceLocation & _memberLoc, const char * _id, const Ast::SourceLocation & _oldLoc );
      virtual ~MemberAlreadyDeclared( );
      virtual void display( std::ostream & os ) const;
    };

    class PublicGetSetInNonfinalClass : public StaticInconsistency
    {
      Ast::SourceLocation memberLoc;
      const char * id;
    public:
      PublicGetSetInNonfinalClass( const Ast::SourceLocation & _memberLoc, const char * _id );
      virtual ~PublicGetSetInNonfinalClass( );
      virtual void display( std::ostream & os ) const;
    };

    class TransformingMemberInNonfinalClass : public StaticInconsistency
    {
      Ast::SourceLocation memberLoc;
      const char * id;
    public:
      TransformingMemberInNonfinalClass( const Ast::SourceLocation & _memberLoc, const char * _id );
      virtual ~TransformingMemberInNonfinalClass( );
      virtual void display( std::ostream & os ) const;
    };

    class RepeatedFormal : public StaticInconsistency
    {
      Ast::SourceLocation loc;
      const char * id;
    public:
      RepeatedFormal( const Ast::SourceLocation & _Loc, const char * _id );
      virtual ~RepeatedFormal( );
      virtual void display( std::ostream & os ) const;
    };

    class PassingStateOut : public StaticInconsistency
    {
      Ast::SourceLocation loc_;
      const char * id_;
    public:
      PassingStateOut( const Ast::SourceLocation & loc, const char * id );
      virtual ~PassingStateOut( );
      virtual void display( std::ostream & os ) const;
    };

    class IntroducingExisting : public StaticInconsistency
    {
      Ast::SourceLocation loc;
      const char * id;
    public:
      IntroducingExisting( const Ast::SourceLocation & _loc, const char * _id );
      virtual ~IntroducingExisting( );
      virtual void display( std::ostream & os ) const;
    };

    class FreezingUndefined : public StaticInconsistency
    {
      Ast::SourceLocation loc;
      const char * id;
    public:
      FreezingUndefined( const Ast::SourceLocation & _loc, const char * _id );
      virtual ~FreezingUndefined( );
      virtual void display( std::ostream & os ) const;
    };

    class ExpectedImperative : public StaticInconsistency
    {
      Ast::SourceLocation loc;
    public:
      ExpectedImperative( const Ast::SourceLocation & _loc );
      virtual ~ExpectedImperative( );
      virtual void display( std::ostream & os ) const;
    };

    class IllegalImperative : public StaticInconsistency
    {
      Ast::SourceLocation loc;
    public:
      IllegalImperative( const Ast::SourceLocation & _loc );
      virtual ~IllegalImperative( );
      virtual void display( std::ostream & os ) const;
    };

    class FileOpenError : public Exception
    {
    public:
      enum Type{ OPEN, STAT };
    private:
      Ast::SourceLocation loc;
      RefCountPtr< const char > filename;
      Type type;
    public:
      FileOpenError( const Ast::SourceLocation & _loc, RefCountPtr< const char > _filename, Type _type = OPEN );
      virtual ~FileOpenError( );
      virtual void display( std::ostream & os ) const;
    };

    class TeXSetupTooLate : public Exception
    {
      Ast::SourceLocation loc;
    public:
      TeXSetupTooLate( const Ast::SourceLocation & _loc );
      virtual ~TeXSetupTooLate( );
      virtual void display( std::ostream & os ) const;
    };

    class EmptyFinalPicture : public Exception
    {
    public:
      EmptyFinalPicture( );
      virtual ~EmptyFinalPicture( );
      virtual void display( std::ostream & os ) const;
    };

    class RuntimeError : public Exception
    {
      Ast::SourceLocation loc_;
    public:
      RuntimeError( const Ast::SourceLocation & loc );
      RuntimeError( Ast::Expression * expr );
      const Ast::SourceLocation & getLoc( ) const;
      virtual ~RuntimeError( );
    };

    class UserError : public RuntimeError
    {
      RefCountPtr< const char > msg;
    public:
      UserError( RefCountPtr< const char > _msg );
      virtual ~UserError( );
      virtual void display( std::ostream & os ) const;
    };

    class InternalError : public RuntimeError
    {
      const char * msg_;
      RefCountPtr< const char > msgMem_;
    public:
      InternalError( const Ast::SourceLocation & loc, RefCountPtr< const char > msg );
      InternalError( const Ast::SourceLocation & loc, const char * msg );
      InternalError( RefCountPtr< const char > msg );
      InternalError( const char * msg );
      InternalError( const std::ostringstream & msg );
      virtual ~InternalError( );
      virtual void display( std::ostream & os ) const;
    };

    class ExternalError : public RuntimeError
    {
      const char * msg_;
      RefCountPtr< const char > msgMem_;
    public:
      ExternalError( RefCountPtr< const char > msg );
      ExternalError( const char * msg );
      virtual ~ExternalError( );
      virtual void display( std::ostream & os ) const;
    };

    class TeXSetupHasChanged : public InternalError
    {
    public:
      TeXSetupHasChanged( );
      virtual ~TeXSetupHasChanged( );
    };

    class MiscellaneousRequirement : public RuntimeError
    {
      const char * msg_;
      RefCountPtr< const char > msgMem_;
    public:
      MiscellaneousRequirement( RefCountPtr< const char > msg );
      MiscellaneousRequirement( const char * msg );
      virtual ~MiscellaneousRequirement( );
      virtual void display( std::ostream & os ) const;
    };

    class HandlerError : public RuntimeError
    {
      RefCountPtr< const char > msg;
    public:
      HandlerError( RefCountPtr< const char > _msg );
      HandlerError( const char * _msg );
      virtual ~HandlerError( );
      virtual void display( std::ostream & os ) const;
    };

    class PDFVersionError : public RuntimeError
    {
      SimplePDF::PDF_out::Version version_;
      SimplePDF::PDF_out::Version required_;
      RefCountPtr< const char > msgMem_;
      const char * msg_;
    public:
      PDFVersionError( SimplePDF::PDF_out::Version version, SimplePDF::PDF_out::Version required, const RefCountPtr< const char > & msg );
      PDFVersionError( SimplePDF::PDF_out::Version version, SimplePDF::PDF_out::Version required, const char * msg );
      virtual ~PDFVersionError( );
      virtual void display( std::ostream & os ) const;
    };

    class RedefiningLexical : public RuntimeError
    {
      const char * id;
    public:
      RedefiningLexical( const char * _id );
      virtual ~RedefiningLexical( );
      virtual void display( std::ostream & os ) const;
    };

    class RedefiningDynamic : public RuntimeError
    {
      const char * id;
    public:
      RedefiningDynamic( const char * _id );
      virtual ~RedefiningDynamic( );
      virtual void display( std::ostream & os ) const;
    };

    class TypeMismatch : public RuntimeError
    {
      const char * hint_;
      RefCountPtr< const char > valueType;
      RefCountPtr< const char > expectedType;
    public:
      TypeMismatch( const Ast::SourceLocation & loc, const char * hint, RefCountPtr< const char > _valueType, RefCountPtr< const char > _expectedType );
      TypeMismatch( const Ast::SourceLocation & loc, RefCountPtr< const char > _valueType, RefCountPtr< const char > _expectedType );
      virtual ~TypeMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class ConditionTypeMismatch : public RuntimeError
    {
      RefCountPtr< const char > valueType;
    public:
      ConditionTypeMismatch( RefCountPtr< const char > _valueType );
      virtual ~ConditionTypeMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class VariableTypeMismatch : public RuntimeError
    {
      RefCountPtr< const char > id;
      RefCountPtr< const char > valueType;
      RefCountPtr< const char > expectedType;
    public:
      VariableTypeMismatch( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id, RefCountPtr< const char > _valueType, RefCountPtr< const char > _expectedType );
      virtual ~VariableTypeMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class NonObjectMemberAssignment : public RuntimeError
    {
      RefCountPtr< const char > valueType;
    public:
      NonObjectMemberAssignment( const Ast::SourceLocation & _loc, RefCountPtr< const char > _valueType );
      virtual ~NonObjectMemberAssignment( );
      virtual void display( std::ostream & os ) const;
    };

    class ElementaryTypeWithoutFields : public RuntimeError
    {
      RefCountPtr< const char > valueType;
    public:
      ElementaryTypeWithoutFields( RefCountPtr< const char > _valueType );
      virtual ~ElementaryTypeWithoutFields( );
      virtual void display( std::ostream & os ) const;
    };

    class IllegalFinalReference : public RuntimeError
    {
      RefCountPtr< const char > valueType;
      const char * fieldID;
    public:
      IllegalFinalReference( RefCountPtr< const char > _valueType, const char * _fieldID );
      virtual ~IllegalFinalReference( );
      virtual void display( std::ostream & os ) const;
    };

    class NonExistentMember : public RuntimeError
    {
      RefCountPtr< const char > valueType;
      const char * fieldID;
    public:
      NonExistentMember( RefCountPtr< const char > _valueType, const char * _fieldID );
      virtual ~NonExistentMember( );
      virtual void display( std::ostream & os ) const;
    };

    class NonExistentPosition : public RuntimeError
    {
      size_t pos_;
      size_t maxPos_;
    public:
      NonExistentPosition( size_t pos, size_t maxPos );
      virtual ~NonExistentPosition( );
      virtual void display( std::ostream & os ) const;
    };

    class ProtectedMemberPublicScope : public RuntimeError
    {
      RefCountPtr< const char > valueType;
      const char * fieldID;
    public:
      ProtectedMemberPublicScope( RefCountPtr< const char > _valueType, const char * _fieldID );
      virtual ~ProtectedMemberPublicScope( );
      virtual void display( std::ostream & os ) const;
    };

    class MemberNotAssignable : public RuntimeError
    {
      RefCountPtr< const char > valueType;
      const char * fieldID;
      RefCountPtr< const char > scope;
    public:
      MemberNotAssignable( RefCountPtr< const char > _valueType, const char * _fieldID, RefCountPtr< const char > _scope );
      virtual ~MemberNotAssignable( );
      virtual void display( std::ostream & os ) const;
    };

    class NotApplicable : public Exception
    {
    public:
      NotApplicable( );
      ~NotApplicable( );
      virtual void display( std::ostream & os ) const;
    };

    class UnaryPrefixNotApplicable : public RuntimeError
    {
      const Ast::Expression * expr;
      RefCountPtr< const char > valueType;
      const char * operatorSymbol;
    public:
      UnaryPrefixNotApplicable( const Ast::SourceLocation & _loc, const Ast::Expression * _expr, RefCountPtr< const char > _valueType );
      virtual ~UnaryPrefixNotApplicable( );
      void setOperatorSymbol( const char * _operatorSymbol );
      virtual void display( std::ostream & os ) const;
    };

    class UnaryPostfixNotApplicable : public RuntimeError
    {
      const Ast::Expression * expr;
      RefCountPtr< const char > valueType;
      const char * operatorSymbol;
    public:
      UnaryPostfixNotApplicable( const Ast::SourceLocation & _loc, const Ast::Expression * _expr, RefCountPtr< const char > _valueType );
      virtual ~UnaryPostfixNotApplicable( );
      void setOperatorSymbol( const char * _operatorSymbol );
      virtual void display( std::ostream & os ) const;
    };

    class BinaryInfixNotApplicable : public RuntimeError
    {
      const Ast::Expression * expr1;
      RefCountPtr< const char > valueType1;
      const Ast::Expression * expr2;
      RefCountPtr< const char > valueType2;
      const char * operatorSymbol;
    public:
      BinaryInfixNotApplicable( const Ast::SourceLocation & _loc, const Ast::Expression * _expr1, RefCountPtr< const char > _valueType1, const Ast::Expression * _expr2, RefCountPtr< const char > _valueType2 );
      virtual ~BinaryInfixNotApplicable( );
      void setOperatorSymbol( const char * _operatorSymbol );
      virtual void display( std::ostream & os ) const;
    };

    class RedefiningUnknown : public RuntimeError
    {
      RefCountPtr< const char > id;
    public:
      RedefiningUnknown( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id );
      virtual ~RedefiningUnknown( );
      virtual void display( std::ostream & os ) const;
    };

    class ProhibitedTypeChange : public RuntimeError
    {
      RefCountPtr< const char > id;
      RefCountPtr< const char > oldType;
      RefCountPtr< const char > newType;
    public:
      ProhibitedTypeChange( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id, RefCountPtr< const char > _oldType, RefCountPtr< const char > _newType );
      virtual ~ProhibitedTypeChange( );
      virtual void display( std::ostream & os ) const;
    };

    class LookupUnknown : public StaticInconsistency
    {
    public:
      enum Type{ VARIABLE, STATE, DYNAMIC_VARIABLE, DYNAMIC_STATE, TYPE };
    private:
      RefCountPtr< const char > id;
      Type type_;
    public:
      LookupUnknown( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id, Type type );
      virtual ~LookupUnknown( );
      virtual void display( std::ostream & os ) const;
    };

    class StateBeyondFunctionBoundary : public StaticInconsistency
    {
      RefCountPtr< const char > id;
    public:
      StateBeyondFunctionBoundary( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id );
      virtual ~StateBeyondFunctionBoundary( );
      virtual void display( std::ostream & os ) const;
    };

    class IntroducingExistingUnit : public StaticInconsistency
    {
      RefCountPtr< const char > id;
    public:
      IntroducingExistingUnit( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id );
      virtual ~IntroducingExistingUnit( );
      virtual void display( std::ostream & os ) const;
    };

    class LookupUnknownUnit : public StaticInconsistency
    {
      RefCountPtr< const char > id;
    public:
      LookupUnknownUnit( const Ast::SourceLocation & _loc, RefCountPtr< const char > _id );
      virtual ~LookupUnknownUnit( );
      virtual void display( std::ostream & os ) const;
    };

    class OutOfRange : public RuntimeError
    {
      const char * msg_;
      RefCountPtr< const char > msgMem_;
    public:
      OutOfRange( RefCountPtr< const char > msg );
      OutOfRange( const char * msg );
      OutOfRange( const Ast::SourceLocation & _loc, RefCountPtr< const char > msg );
      OutOfRange( const Ast::SourceLocation & _loc, const char * msg );
      virtual ~OutOfRange( );
      virtual void display( std::ostream & os ) const;
    };

    class NonVoidStatement : public RuntimeError
    {
      RefCountPtr< const Lang::Value > val;
    public:
      NonVoidStatement( const Ast::SourceLocation & _loc, RefCountPtr< const Lang::Value > _val );
      virtual ~NonVoidStatement( );
      virtual void display( std::ostream & os ) const;
    };

    class UserArityMismatch : public RuntimeError
    {
    public:
      enum Type{ VARIABLE, STATE };
    private:
      const Ast::SourceLocation formalsLoc;    
      const size_t functionArity;
      const size_t callArity;
      const Type type_;
    public:
      UserArityMismatch( const Ast::SourceLocation _formalsLoc, size_t _functionArity, size_t _callArity, const Type type );
      virtual ~UserArityMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class SinkRequired : public RuntimeError
    {
    private:
      const Ast::SourceLocation loc_;    
      const size_t formalsArity_;
      const size_t callArity_;
    public:
      SinkRequired( const Ast::SourceLocation loc, size_t formalsArity, size_t callArity );
      virtual ~SinkRequired( );
      virtual void display( std::ostream & os ) const;
    };

    class NamedFormalMismatch : public RuntimeError
    {
    public:
      enum Type{ VARIABLE, STATE };
    private:
      const Ast::SourceLocation formalsLoc;    
      RefCountPtr< const char > name;
      Type type_;
    public:
      NamedFormalMismatch( const Ast::SourceLocation _formalsLoc, RefCountPtr< const char > _name, Type type );
      virtual ~NamedFormalMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class NamedFormalAlreadySpecified : public RuntimeError
    {
    public:
      enum Type{ VARIABLE, STATE };
    private:
      const Ast::SourceLocation formalsLoc;    
      RefCountPtr< const char > name;
      size_t pos;
      Type type_;
    public:
      NamedFormalAlreadySpecified( const Ast::SourceLocation _formalsLoc, RefCountPtr< const char > _name, size_t _pos, Type type );
      virtual ~NamedFormalAlreadySpecified( );
      virtual void display( std::ostream & os ) const;
    };

    class MissingArguments : public RuntimeError
    {
      const Ast::SourceLocation formalsLoc;
      std::map< size_t, RefCountPtr< const char > > * missingVariables;
      std::map< size_t, RefCountPtr< const char > > * missingStates;
    public:
      MissingArguments( const Ast::SourceLocation _formalsLoc, std::map< size_t, RefCountPtr< const char > > * _missingVariables, std::map< size_t, RefCountPtr< const char > > * _missingStates );
      ~MissingArguments( );
      virtual void display( std::ostream & os ) const;
    };

    class CoreArityMismatch : public RuntimeError
    {
      const char * title;
      RefCountPtr< const char > titleMem;
      const size_t functionArityLow;
      const size_t functionArityHigh;
      const size_t callArity;
    public:
      CoreArityMismatch( const char * _title, size_t _functionArity, size_t _callArity );
      CoreArityMismatch( const char * _title, size_t _functionArityLow, size_t _functionArityHigh, size_t _callArity );
      CoreArityMismatch( const RefCountPtr< const char > & _title, size_t _functionArity, size_t _callArity );
      CoreArityMismatch( const RefCountPtr< const char > & _title, size_t _functionArityLow, size_t _functionArityHigh, size_t _callArity );
      virtual ~CoreArityMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class CoreNoNamedFormals : public RuntimeError
    {
      const char * title;
      RefCountPtr< const char > titleMem;
    public:
      CoreNoNamedFormals( const char * _title );
      CoreNoNamedFormals( const RefCountPtr< const char > & _title );
      virtual ~CoreNoNamedFormals( );
      virtual void display( std::ostream & os ) const;
    };

    class CoreTypeMismatch : public RuntimeError
    {
      const char * title_;
      RefCountPtr< const char > titleMem_;
      const Ast::SourceLocation argLoc_;
      RefCountPtr< const char > valueType_;
      RefCountPtr< const char > expectedType_;
    public:
      CoreTypeMismatch( const Ast::SourceLocation & callLoc,
			const char * title,
			const Ast::SourceLocation & argLoc,
			RefCountPtr< const char > valueType,
			RefCountPtr< const char > expectedType );
      CoreTypeMismatch( const Ast::SourceLocation & callLoc,
			RefCountPtr< const char > title,
			const Ast::SourceLocation & argLoc,
			RefCountPtr< const char > valueType,
			RefCountPtr< const char > expectedType );
      CoreTypeMismatch( const Ast::SourceLocation & callLoc,
			const char * title,
			Kernel::Arguments & args,
			size_t argNo,
			RefCountPtr< const char > expectedType );
      CoreTypeMismatch( const Ast::SourceLocation & callLoc,
			RefCountPtr< const char > title,
			Kernel::Arguments & args,
			size_t argNo,
			RefCountPtr< const char > expectedType );
      virtual ~CoreTypeMismatch( );
      virtual void display( std::ostream & os ) const;
    };
  
    class CoreDynamicTypeMismatch : public RuntimeError
    {
      const char * title_;
      RefCountPtr< const char > titleMem_;
      const char * id_;
      RefCountPtr< const char > idMem_;
      RefCountPtr< const char > valueType_;
      RefCountPtr< const char > expectedType_;
    public:
      CoreDynamicTypeMismatch( const Ast::SourceLocation & callLoc,
			       const char * title,
			       const char * id,
			       RefCountPtr< const char > valueType,
			       RefCountPtr< const char > expectedType );
      virtual ~CoreDynamicTypeMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class CoreStateTypeMismatch : public RuntimeError
    {
      const char * title_;
      RefCountPtr< const char > titleMem_;
      const Ast::SourceLocation argLoc_;
      RefCountPtr< const char > valueType_;
      RefCountPtr< const char > expectedType_;
    public:
      CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
			     const char * title,
			     const Ast::SourceLocation & argLoc,
			     RefCountPtr< const char > valueType,
			     RefCountPtr< const char > expectedType );
      CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
			     RefCountPtr< const char > title,
			     const Ast::SourceLocation & argLoc,
			     RefCountPtr< const char > valueType,
			     RefCountPtr< const char > expectedType );
      CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
			     const char * title,
			     Kernel::Arguments & args,
			     size_t argNo,
			     RefCountPtr< const char > expectedType );
      CoreStateTypeMismatch( const Ast::SourceLocation & callLoc,
			     RefCountPtr< const char > title,
			     Kernel::Arguments & args,
			     size_t argNo,
			     RefCountPtr< const char > expectedType );
      virtual ~CoreStateTypeMismatch( );
      virtual void display( std::ostream & os ) const;
    };
  

    class ContinuationTypeMismatch : public RuntimeError
    {
      RefCountPtr< const char > valueType_;
      RefCountPtr< const char > expectedType_;
    public:
      ContinuationTypeMismatch( const Kernel::Continuation * locationCont,
				RefCountPtr< const char > valueType,
				RefCountPtr< const char > expectedType );
      virtual ~ContinuationTypeMismatch( );
      virtual void display( std::ostream & os ) const;
    };

    class EmptyApplication : public RuntimeError
    {
    public:
      EmptyApplication( );
      virtual ~EmptyApplication( );
      virtual void display( std::ostream & os ) const;
    };

    class CoreOutOfRange : public RuntimeError
    {
      const char * title;
      const Ast::SourceLocation argLoc_;
      const char * msg_;
      RefCountPtr< const char > msgMem_;
    public:
      CoreOutOfRange( const char * _title, Kernel::Arguments & args, size_t argNo, RefCountPtr< const char > msg );
      CoreOutOfRange( const char * _title, Kernel::Arguments & args, size_t argNo, const char * msg );
      virtual ~CoreOutOfRange( );
      virtual void display( std::ostream & os ) const;
    };

    class CoreRequirement : public RuntimeError
    {
      const char * title;
      const char * msg_;
      RefCountPtr< const char > msgMem_;
    public:
      CoreRequirement( RefCountPtr< const char > msg, const char * _title, const Ast::SourceLocation & callLoc );
      CoreRequirement( const char * msg, const char * _title, const Ast::SourceLocation & callLoc );
      virtual ~CoreRequirement( );
      virtual void display( std::ostream & os ) const;
    };

    class TeXLabelError : public RuntimeError
    {
      RefCountPtr< const char > msg;
    public:
      TeXLabelError( RefCountPtr< const char > _msg );
      virtual ~TeXLabelError( );
      virtual void display( std::ostream & os ) const;
    };

    class InstantiatingAbstractClass : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
    public:
      InstantiatingAbstractClass( RefCountPtr< const Lang::Class > _cls );
      virtual ~InstantiatingAbstractClass( );
      virtual void display( std::ostream & os ) const;
    };

    class FailedToDeclareClassAbstract : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      const MetaPDF::Ast::ClassFunction * classExpr;
    public:
      FailedToDeclareClassAbstract( RefCountPtr< const Lang::Class > _cls, const MetaPDF::Ast::ClassFunction * _classExpr );
      virtual ~FailedToDeclareClassAbstract( );
      virtual void display( std::ostream & os ) const;
    };

    class RepeatedImmediateParent : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const Lang::Class > parent;
    public:
      RepeatedImmediateParent( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent );
      virtual ~RepeatedImmediateParent( );
      virtual void display( std::ostream & os ) const;
    };

    class OverridingNonParent : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const Lang::Class > parent;
    public:
      OverridingNonParent( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent );
      virtual ~OverridingNonParent( );
      virtual void display( std::ostream & os ) const;
    };

    class InheritingFinal : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const Lang::Class > parent;
    public:
      InheritingFinal( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent );
      virtual ~InheritingFinal( );
      virtual void display( std::ostream & os ) const;
    };

    class OverridingUndeclaredMethod : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const Lang::Class > parent;
      const char * id;
    public:
      OverridingUndeclaredMethod( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent, const char * _id );
      virtual ~OverridingUndeclaredMethod( );
      virtual void display( std::ostream & os ) const;
    };

    class OverridingFinalMethod : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const Lang::Class > parent;
      const char * id;
    public:
      OverridingFinalMethod( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent, const char * _id );
      virtual ~OverridingFinalMethod( );
      virtual void display( std::ostream & os ) const;
    };

    class IllegalRepeatedBase : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const Lang::Class > parent;
    public:
      IllegalRepeatedBase( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent );
      virtual ~IllegalRepeatedBase( );
      virtual void display( std::ostream & os ) const;
    };

    class AmbiguousInheritedMethod : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const char > id;
      std::set< RefCountPtr< const Lang::Class > > parents;
    public:
      AmbiguousInheritedMethod( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const char > _id, std::set< RefCountPtr< const Lang::Class > > _parents );
      virtual ~AmbiguousInheritedMethod( );
      virtual void display( std::ostream & os ) const;
    };

    class MisplacedSuperReference : public RuntimeError
    {
    public:
      MisplacedSuperReference( const Ast::SourceLocation & _loc );
      virtual ~MisplacedSuperReference( );
      virtual void display( std::ostream & os ) const;
    };

    class SuperReferenceClassNotParent : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      RefCountPtr< const Lang::Class > parent;
    public:
      SuperReferenceClassNotParent( RefCountPtr< const Lang::Class > _cls, RefCountPtr< const Lang::Class > _parent );
      virtual ~SuperReferenceClassNotParent( );
      virtual void display( std::ostream & os ) const;
    };

    class NoSuchMethod : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      Kernel::MethodId method;
    public:
      NoSuchMethod( RefCountPtr< const Lang::Class > _cls, const Kernel::MethodId & _method );
      virtual ~NoSuchMethod( );
      virtual void display( std::ostream & os ) const;
    };

    class NoSuchLocalMethod : public RuntimeError
    {
      RefCountPtr< const Lang::Class > cls;
      Kernel::MethodId method;
    public:
      NoSuchLocalMethod( RefCountPtr< const Lang::Class > _cls, const Kernel::MethodId & _method );
      virtual ~NoSuchLocalMethod( );
      virtual void display( std::ostream & os ) const;
    };

    class MultipleDynamicBind : public RuntimeError
    {
      const char * id;
      Ast::SourceLocation prevLoc;
    public:
      MultipleDynamicBind( const char * _id, const Ast::SourceLocation & _loc, const Ast::SourceLocation & _prevLoc );
      virtual ~MultipleDynamicBind( );
      virtual void display( std::ostream & os ) const;
    };

    class UndefinedEscapeContinuation : public RuntimeError
    {
      const char * id;
    public:
      UndefinedEscapeContinuation( const char * _id, const Ast::SourceLocation & _loc );
      virtual ~UndefinedEscapeContinuation( );
      virtual void display( std::ostream & os ) const;
    };

    class DeadStateAccess : public RuntimeError
    {
    public:
      DeadStateAccess( );
      virtual ~DeadStateAccess( );
      virtual void display( std::ostream & os ) const;
    };

    class UnFreezable : public RuntimeError
    {
    public:
      UnFreezable( );
      virtual ~UnFreezable( );
      virtual void display( std::ostream & os ) const;
    };

    class UnPeekable : public RuntimeError
    {
    public:
      UnPeekable( );
      virtual ~UnPeekable( );
      virtual void display( std::ostream & os ) const;
    };

    class UninitializedAccess : public RuntimeError
    {
    public:
      UninitializedAccess( );
      virtual ~UninitializedAccess( );
      virtual void display( std::ostream & os ) const;
    };

    class DtMinError : public RuntimeError
    {
      double dt;
    public:
      DtMinError( double _dt );
      virtual ~DtMinError( );
      virtual void display( std::ostream & os ) const;
    };

    class AffineTransformKillsPlane : public RuntimeError
    {
      double sigma2_;
    public:
      AffineTransformKillsPlane( double sigma2 );
      virtual ~AffineTransformKillsPlane( );
      virtual void display( std::ostream & os ) const;
    };

    class MissingFontMetrics : public RuntimeError
    {
      RefCountPtr< const char > fontname_;
      const std::list< std::string > * searchPath_;
    public:
      MissingFontMetrics( RefCountPtr< const char > fontname, const std::list< std::string > * searchPath );
      virtual ~MissingFontMetrics( );
      virtual void display( std::ostream & os ) const;
    };

    class FontMetricsError : public RuntimeError
    {
      RefCountPtr< const char > fontname_;
      RefCountPtr< const char > message_;
    public:
      FontMetricsError( const RefCountPtr< const char > & fontname, const RefCountPtr< const char > & message );
      virtual ~FontMetricsError( );
      virtual void display( std::ostream & os ) const;
    };

  }



  namespace NonLocalExit
  {
    // The exceptions in this namespace does not represent ordinary exception conditions in the program.
    // Rather, they can be seen as alternative ways for particular functions to return.  Thus, the caller
    // of such a function shall always take care of any such exceptions, and not let these be passed up.
    // For this reason, the classes in this namespace does not inherit from MetaPDF::Exceptions::Exception.

    class NonLocalExitBase
    {
    public:
      NonLocalExitBase( ){ }
    };

    class DynamicBindingNotFound : public NonLocalExitBase
    {
    public:
      DynamicBindingNotFound( ){ }
    };
    
    class CrossDirectionOfParallel : public NonLocalExitBase
    {
    public:
      CrossDirectionOfParallel( ){ }
    };
    
    class NotThisType : public NonLocalExitBase
    {
    public:
      NotThisType( ){ }
    };

  }
}

#endif
