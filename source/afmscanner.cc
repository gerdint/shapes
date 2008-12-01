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

