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
		Version maxRequestVersion_;
		Action versionAction_;

	public:
		PDF_Version( );
		const char * maxRequestVersionString( ) const;

		void setVersion( Version version );
		void setAction( Action action );
		bool greaterOrEqual( Version required, bool justCurious = false );
		bool greaterOrEqualOrX( Version required, bool justCurious = false );
		void message( Version required, const char * message ) const;

		static const char * toString( Version version );
	};

}

#endif
