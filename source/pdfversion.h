#ifndef pdfversion_h
#define pdfversion_h

#include "SimplePDF_decls.h"


namespace SimplePDF
{

	class PDF_Version
	{
	public:
		enum Version { PDF_X = 0, PDF_1_1, PDF_1_2, PDF_1_3, PDF_1_4, PDF_1_5, PDF_1_6, VERSION_UNDEFINED };
		enum Action { ERROR = 0, WARN, SILENT };
	private:
		Version version_;
		Action versionAction_;

	public:
		PDF_Version( );
		const char * string( ) const;

		void setVersion( Version version ){ version_ = version; }
		void setAction( Action action ){ versionAction_ = action; }
		bool greaterOrEqual( Version required ) const { return version_ >= required; }
		bool greaterOrEqualOrX( Version required ) const { return version_ >= required || version_ == PDF_X; }
		void message( Version required, const char * message ) const;

		static const char * toString( Version version );
	};

}

#endif
