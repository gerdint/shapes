#include "afmscanner.h"
#include "strrefdup.h"
#include "charconverters.h"

#include <sstream>


AfmScanner::AfmScanner( FontMetrics::BaseFont * fontMetricsDst, std::istream * yyin )
	: yyFlexLexer( yyin, 0 ), fontMetricsDst_( fontMetricsDst ),
		currentDirectionDst_( 0 ),
		currentDirectionID_( -1 ),
		activateDirectionID_( 0 ),
		metricsSets_( 0 ),
		tellQue_( false ),
		encoding_( & Shapes::Helpers::requireMacRomanEncoding( ) )
{ }

void
AfmScanner::throwError( const char * msg )
{
	std::ostringstream oss;
	oss << "In font metrics for " ;
	if( fontMetricsDst_->fullName_ == NullPtr< const char >( ) )
		{
			oss << "???" ;
		}
	else
		{
			oss << fontMetricsDst_->fullName_;
		}
	oss << ": " << msg ;
	throw strrefdup( oss );
}

void
AfmScanner::synchWritingDirection( )
{
	if( currentDirectionID_ == activateDirectionID_ )
		{
			return;
		}

	typedef FontMetrics::WritingDirectionMetrics MetricsType;

	switch( activateDirectionID_ )
		{
		case 0:
			{
				if( fontMetricsDst_->horizontalMetrics_ != NullPtr< MetricsType >( ) )
					{
						throwError( "Reactivating writing direction 0." );
					}
				fontMetricsDst_->horizontalMetrics_ = RefCountPtr< MetricsType >( new MetricsType );
				currentDirectionDst_ = fontMetricsDst_->horizontalMetrics_.getPtr( );
			}
			break;
		case 1:
			{
				if( fontMetricsDst_->verticalMetrics_ != NullPtr< MetricsType >( ) )
					{
						throwError( "Reactivating writing direction 1." );
					}
				fontMetricsDst_->verticalMetrics_ = RefCountPtr< MetricsType >( new MetricsType );
				currentDirectionDst_ = fontMetricsDst_->verticalMetrics_.getPtr( );
			}
			break;
		case 2:
			{
				if( fontMetricsDst_->horizontalMetrics_ != NullPtr< MetricsType >( ) )
					{
						throwError( "Reactivating writing direction 0." );
					}
				if( fontMetricsDst_->verticalMetrics_ != NullPtr< MetricsType >( ) )
					{
						throwError( "Reactivating writing direction 1." );
					}
				fontMetricsDst_->horizontalMetrics_ = RefCountPtr< MetricsType >( new MetricsType );
				fontMetricsDst_->verticalMetrics_ = fontMetricsDst_->horizontalMetrics_;
				currentDirectionDst_ = fontMetricsDst_->horizontalMetrics_.getPtr( );
			}
			break;
		default:
			throwError( "activateDirectionID_ out of range." );
		}

	currentDirectionID_ = activateDirectionID_;
}

