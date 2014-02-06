package BVA::XUI::DATETIME;

$BVA::XUI::DATETIME::VERSION	= '1.020.020';   # 2012-12-21

use strict;

BEGIN {
	eval {use warnings qw/all/};
}

use Time::Local;

### tell_time #####
	## Formats: date time, file date, stamp, 
	## long time, exact time, date, time, secs
	## iso, iso date, iso time
	## use: tell_time('exact time' [, time in secs | iso date-time ]);
	## New version 2004-12-01
sub tell_time {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	# name of the format for output
	my $format		= shift() || 'iso';
	
	# date-time string defaults to now
	my $time		= shift() || time;
	
	# optional flag regarding non-date strings
	my $return_text	= shift() || '';
	
	# get system time in secs
	my $seconds = parse_to_secs($time);
	
	return $time if $return_text and not $seconds;
	
	format_secs($format, $seconds);
}

sub format_secs {
	# name of the format for output
	my $format		= shift() || 'iso';
	
	# the time in system seconds
	my $seconds	= shift;
	
	# Sanity
	$seconds		 		or return '';
	$seconds =~ /^\d+$/ 	or return '';

	# Derive standard local date values/indices
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime($seconds);
	
	$year			+= 1900;
	my $mil_hour	= $hour;
	my $AP			= $hour > 11 ? 'PM' : 'AM' ;
	$hour			= $hour % 12 || 12;
	my $season		= $isdst ? 'DST' : 'STD' ; # local savings/standard time
	
	my $day_secs	= $sec + (60 * $min) + (3600 * $mil_hour) || 0;

	{
		local $_ = $format;
		/date.{0,3}time/i and
		sprintf "%s %s, %04d, at %d:%02d %s", $BVA::XUI::DATETIME::MONTHS[$mon], $mday, $year, $hour, $min, $AP
			or
		/long.{0,3}date/i and 
		sprintf "%s, %s %d, %d", $BVA::XUI::DATETIME::WEEKDAYS[$wday], $BVA::XUI::DATETIME::MONTHS[$mon], $mday, $year
			or
		/abbr.{0,3}date/i and 
		sprintf "%s %d, %d", $BVA::XUI::DATETIME::ABBR_MONTHS[$mon], $mday, $year
			or
		/^file.{0,2}date/i and 
		sprintf "%02d.%02d.%02d", $mon+1, $mday, $year % 100
			or
		/iso.{0,2}file.{0,2}date$/i and 
		sprintf "%04d.%02d.%02d", $year, $mon+1, $mday
			or
		/iso.{0,2}file.{0,2}date.{0,2}secs/i and 
		sprintf "%04d.%02d.%02d.%05d", $year, $mon+1, $mday, $day_secs
			or
		/dot.{0,2}yearmon/i and 
		sprintf "%04d.%02d", $year, $mon+1
			or
		/^\s*date\s*$/i and 
		sprintf "%s %d, %d", $BVA::XUI::DATETIME::MONTHS[$mon], $mday, $year
			or
		/^\s*month.{0,2}date\s*$/i and 
		sprintf "%s %d", $BVA::XUI::DATETIME::MONTHS[$mon], $mday
			or
		/^\s*day.{0,2}date\s*$/i and 
		sprintf "%s, %s %d, %d", $BVA::XUI::DATETIME::WEEKDAYS[$wday], $BVA::XUI::DATETIME::MONTHS[$mon], $mday, $year
			or
		/^\s*iso\s*$/i and 
		sprintf "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d",
		$year, $mon + 1, $mday, $mil_hour, $min, $sec
			or
		/^iso.{0,2}store\s*$/i and 
		sprintf "%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2d",
		$year, $mon + 1, $mday, $mil_hour, $min, $sec
			or
		/^iso.{0,2}date\s*$/i and 
		sprintf "%4.4d-%2.2d-%2.2d", $year, $mon + 1, $mday
			or
		/^\s*(time\s*)?stamp\s*$/i and 
		sprintf "%02d/%02d/%04d %2d:%02d %s", $mon+1, $mday, $year, $hour, $min, $AP
			or
		/^iso\s*(time\s*)?stamp\s*$/i and 
		sprintf "%4.4d-%2.2d-%2.2d %2d:%02d %s", $year, $mon + 1, $mday, $hour, $min, $AP
			or
		/^\s*date\s*stamp\s*$/i and 
		sprintf "%02d/%02d/%02d", $mon+1, $mday, $year % 100
			or
		/short.{0,3}date\s*$/i and 
		sprintf "%d/%d/%02d", $mon+1, $mday, $year % 100
			or
		/e?x(ce)?l.{0,3}date\s*$/i and 
		sprintf "%d-%s-%02d", $mday, $BVA::XUI::DATETIME::ABBR_MONTHS[$mon], $year % 100
			or
		/long.{0,3}time\s*$/i and 
		sprintf "%d:%02d %s %s", $hour, $min, $AP, $season
			or
		/exact.{0,3}time\s*$/i and 
		sprintf "%02d:%02d:%02d", $mil_hour, $min, $sec
			or
		/iso.{0,3}time\s*$/i and 
		sprintf "%2.2d:%2.2d:%2.2d", $mil_hour, $min, $sec
			or
		/^\s*time\s*$/i and 
		sprintf "%d:%02d %s", $hour, $min, $AP
			or
		/^\s*wday\s*num\s*$/i and 
		$wday + 1
			or
		/^\s*wday\s*$/i and 
		$BVA::XUI::DATETIME::WEEKDAYS[$wday]
			or
		/^\s*mday\s*$/i and 
		$mday
			or
		/^\s*yday\s*$/i and 
		$yday + 1
			or
		/^\s*mon\s*$/i and 
		$BVA::XUI::DATETIME::MONTHS[$mon]
			or
		/^\s*short\s*mon\s*$/i and 
		$BVA::XUI::DATETIME::ABBR_MONTHS[$mon]
			or
		/^\s*year\s*$/i and 
		$year
			or
		/^\s*yy\s*$/i and 
		sprintf "%02d", $year % 100
			or
		/^\s*mm\s*$/i and 
		sprintf "%02d", $mon+1
			or
		/^\s*secs\s*$/i and 
		$seconds
	}
}

sub parse_to_secs {
	my $time	= shift;
	my (@date,$AP,$secs);
	$time						or return;
	$time =~ s/^\s*(.*)\s*$/$1/;
	$time eq '-' 				and return $time;
	$time eq 'now' 				and return time;
	$time =~ /^[-0: \/]*$/		and return time;
	
	# rough way to find dates like 082602 (2002-08-26)
	$time =~ s{^((?:0|1)\d)(\d\d)(\d\d)$}{$1/$2/$3};
	$time =~ s{^((?:0|1)\d)(\d\d)((?:19|20)\d\d)$}{$1/$2/$3};
	$time =~ s{^((?:19|20)\d\d)(\d\d)(\d\d)$}{$1-$2-$3};
	$time =~ /^\d+$/ && $time > 123103		and return $time;  # it's not 12-31-03
	
	# dates in iso format CCYY-MM-DD[[T| ]HH:MM[:SS]][ AM|PM][ *]
	iso_order($time)
		||
	# date-times in iCal format 20111024T171316Z
	ical_date_time($time)
		||
	# dates starting with month names
	month_name($time)
		||
	# dates starting with day number before month names
	euro_month_name($time)
		||
	# dates in US format MM/DD/[YY]YY, MM.DD.[YY]YY, MM-DD-[YY]YY [HH:MM[:SS]][ AM|PM][ *]
	us_string($time)
		||
	''	
}

sub iso_order {
	my $time = shift;
	my (@date,$AP);
	
	# dates in iso format CCYY-MM-DD[[T| ]HH:MM[:SS]][ AM|PM][ *]
	if ( (@date[0..5],$AP) = 
	( $time =~ m#^
 		\s*
 		(\d{4})
 		[-/]
 		(\d{2})
 		[-/]
 		(\d{2})
 		(?:
 		[ |T]
 		(\d{1,2})
 		:
 		(\d{2})
 		(?:
 		:
 		(\d{2})
 		)?
 		)?
 		\s*
 		(?:
 		(AM?|PM?)
 		)?
 		(?:
 		(.*?)
 		)?
 		$#ix )
 		) {
 			@date = map { defined($_) ? $_+0 : 0 } @date;
			$date[1] > 0 and $date[1]--;
			$date[2] ||= 1;
			$date[3] += 12 if ($AP and $AP =~/P/i and $date[3] < 12);
			$date[3] = 0 if ($AP and $AP =~/A/i and $date[3] == 12);
			@date = reverse @date;
			return timelocal(@date);
	}
}

sub ical_date_time {
	my $time = shift;
	my (@date);

	# dates in iCal format CCYYMMDDTHHMMSS[Z]
	if ( (@date[0..5]) = 
	( $time =~ m#^
 		\s*
 		(\d{4})
 		(\d{2})
 		(\d{2})
 		T
 		(\d{2})
 		(\d{2})
 		(\d{2})
 		Z?
 		\s*
 		$#ix )
 		) {
 			@date = map { defined($_) ? $_+0 : 0 } @date;
			$date[1] > 0 and $date[1]--;
			$date[2] ||= 1;
			@date = reverse @date;
			return timelocal(@date);
	}
}

sub month_name {
	my $time = shift;
	my (@date, $AP, $mo, $cent,$short_yr);
	if ( ($mo, $date[2],$cent,$short_yr,@date[3,4,5],$AP) =
 	( $time =~ m#^
 		([a-zA-Z]+)
 		\.?
 		[ ]
 		(\d\d?)
 		,?
 		(?:
 		[ ]?
 		(19|20)?
 		(\d{2})
 		)?
 		(?:
 		\s*
 		(\d{1,2})
 		(?:
 		:
 		(\d{2})
 		)?
 		(?:
 		:
 		(\d{2})
 		)?
 		)?
 		\s*
 		(?:
 		(AM?|PM?)
 		)?
 		(?:
 		(.*?)
 		)?$#ix ) 
 		) {
		
		$mo			= lc(substr($mo,0,3));
		return unless $BVA::XUI::DATETIME::MONTH_NUM_TBL{$mo};
# 		$short_yr	||= ($mo eq 'jan' ? $BVA::XUI::DATETIME::THIS_YY : $BVA::XUI::DATETIME::THIS_YY - 1);
 		$short_yr	||= $BVA::XUI::DATETIME::THIS_YY;
		$cent		||= ($short_yr < 70 ? $BVA::XUI::DATETIME::THIS_CC : $BVA::XUI::DATETIME::THIS_CC - 1);
		$date[0]	= sprintf qq{%2d%02d}, $cent, $short_yr;		
		$date[1]	= $BVA::XUI::DATETIME::MONTH_NUM_TBL{$mo} - 1;

		@date		= map { defined($_) ? $_+0 : 0 } @date;
		$date[2] ||= 1;
		return if $date[2] > 31; 
		$date[3]	+= 12 if ($AP and $AP =~/P/i and $date[3] < 12);
		$date[3]	= 0 if ($AP and $AP =~/A/i and $date[3] == 12);
		@date		= reverse @date;
		return timelocal(@date);
	}
}

sub euro_month_name {
	my $time = shift;
	my (@date, $AP, $mo, $cent,$short_yr);
	if ( ($date[2], $mo, $cent,$short_yr,@date[3,4,5],$AP) =
 	( $time =~ m#^
 		(\d\d?)
 		[ -]
 		([a-zA-Z]+)
 		\.?
 		,?
 		(?:
 		[ -]?
 		(19|20)?
 		(\d{2})
 		)?
 		(?:
 		\s*
 		(\d{1,2})
 		(?:
 		:
 		(\d{2})
 		)?
 		(?:
 		:
 		(\d{2})
 		)?
 		)?
 		\s*
 		(?:
 		(AM?|PM?)
 		)?
 		(?:
 		(.*?)
 		)?$
 		#ix) 
 		) {
		
		$mo			= lc(substr($mo,0,3));
		return unless $BVA::XUI::DATETIME::MONTH_NUM_TBL{$mo};
#		$short_yr	||= ($mo eq 'jan' ? $BVA::XUI::DATETIME::THIS_YY : $BVA::XUI::DATETIME::THIS_YY - 1);
 		$short_yr	||= $BVA::XUI::DATETIME::THIS_YY;
		$cent		||= ($short_yr < 70 ? $BVA::XUI::DATETIME::THIS_CC : $BVA::XUI::DATETIME::THIS_CC - 1);
		$date[0]	= sprintf qq{%2d%02d}, $cent, $short_yr;		
		$date[1]	= $BVA::XUI::DATETIME::MONTH_NUM_TBL{$mo} - 1;

		@date		= map { defined($_) ? $_+0 : 0 } @date;
		$date[3]	+= 12 if ($AP and $AP =~/P/i and $date[3] < 12);
		$date[3]	= 0 if ($AP and $AP =~/A/i and $date[3] == 12);
		@date		= reverse @date;
		return timelocal(@date);
	}
}


sub us_string {
	my $time = shift;
	my (@date,$AP,$cent,$short_yr);
	if ( (@date[1,2],$cent,$short_yr,@date[3,4,5],$AP) =
 	( $time =~m#^
 		(\d{1,2})
 		[-\/\.]
 		(\d{1,2})
 		(?:[-\/ \.]|[, ]+)
 		(19|20)?
 		(\d{2})
 		(?:
 		\s*
 		(\d{1,2})
 		:
 		(\d{2})
 		(?:
 		:
 		(\d{2})
 		)?
 		)?
 		\s*
 		(?:
 		(AM?|PM?)
 		)?
 		(?:
 		(.*?)
 		)?
 		#ix)
 		) {

		$short_yr	||= $BVA::XUI::DATETIME::THIS_YY;
		$cent		||= ($short_yr < 39 ? $BVA::XUI::DATETIME::THIS_CC : $BVA::XUI::DATETIME::THIS_CC - 1);
		$date[0]	= sprintf qq{%2d%02d}, $cent, $short_yr;		
		
		@date		= map { defined($_) ? $_+0 : 0 } @date;
		$date[1]	> 0 and $date[1]--;
		$date[3]	+= 12 if ($AP and $AP =~/P/i and $date[3] < 12);
		$date[3]	= 0 if ($AP and $AP =~/A/i and $date[3] == 12);
		@date		= reverse @date;
		return timelocal(@date);
	}

}

sub list_months {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	wantarray ? @BVA::XUI::DATETIME::MONTHS : [ @BVA::XUI::DATETIME::MONTHS ]
}

sub list_abbr_months {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	wantarray ? @BVA::XUI::DATETIME::ABBR_MONTHS : [ @BVA::XUI::DATETIME::ABBR_MONTHS ]
}

sub list_weekdays {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	wantarray ? @BVA::XUI::DATETIME::WEEKDAYS : [ @BVA::XUI::DATETIME::WEEKDAYS ]
}

sub list_abbr_weekdays {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	wantarray ? @BVA::XUI::DATETIME::ABBR_WEEKDAYS : [ @BVA::XUI::DATETIME::ABBR_WEEKDAYS ]
}

sub list_initial_weekdays {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	wantarray ? @BVA::XUI::DATETIME::INITIAL_WEEKDAYS : [ @BVA::XUI::DATETIME::INITIAL_WEEKDAYS ]
}

sub list_monthday_nums {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	wantarray ? @BVA::XUI::DATETIME::MONTHDAY_NUMS : [ @BVA::XUI::DATETIME::MONTHDAY_NUMS ]
}

sub this_yyyy {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	$BVA::XUI::DATETIME::THIS_YYYY
}

sub this_yy {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	$BVA::XUI::DATETIME::THIS_YY
}

sub this_cc {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	$BVA::XUI::DATETIME::THIS_CC
}

sub month_num_lookup {
	@BVA::XUI::DATETIME::MONTH_NUM_TBL{@_};
}

sub month_max_days {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	@BVA::XUI::DATETIME::MONTH_MAX_DAYNUM{@_};	
}

BEGIN {

	# Derive standard local date values/indices
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();

	$BVA::XUI::DATETIME::THIS_YYYY	= $year + 1900;
	
	$BVA::XUI::DATETIME::THIS_YY	= $year % 100;
	
	$BVA::XUI::DATETIME::THIS_CC	= int(($year + 1900)/100);

	@BVA::XUI::DATETIME::MONTHS			= qw/
		January
		February
		March
		April
		May
		June
		July
		August
		September
		October
		November
		December
	/;
	
	@BVA::XUI::DATETIME::ABBR_MONTHS	= qw/
		Jan
		Feb
		Mar
		Apr
		May
		Jun
		Jul
		Aug
		Sep
		Oct
		Nov
		Dec
	/;
	
	@BVA::XUI::DATETIME::WEEKDAYS		= qw/
		Sunday
		Monday
		Tuesday
		Wednesday
		Thursday
		Friday
		Saturday
	/;
	
	@BVA::XUI::DATETIME::ABBR_WEEKDAYS		= qw/
		Sun
		Mon
		Tue
		Wed
		Thu
		Fri
		Sat
	/;

	@BVA::XUI::DATETIME::INITIAL_WEEKDAYS	= qw/
		S
		M
		T
		W
		T
		F
		S
	/;
	
	@BVA::XUI::DATETIME::MONTHDAY_NUMS		=qw/
		1
		2
		3
		4
		5
		6
		7
		8
		9
		10
		11
		12
		13
		14
		15
		16
		17
		18
		19
		20
		21
		22
		23
		24
		25
		26
		27
		28
		29
		30
		31
	/;
	
	%BVA::XUI::DATETIME::MONTH_NUM_TBL		= (
		jan	=> 1,	Jan	=> 1,	January		=> 1,
		feb	=> 2,	Feb	=> 2,	February	=> 2,
		mar	=> 3,	Mar	=> 3,	March		=> 3,
		apr	=> 4,	Apr	=> 4,	April		=> 4,
		may	=> 5,	May	=> 5,	May			=> 5,
		jun	=> 6,	Jun	=> 6,	June		=> 6,
		jul	=> 7,	Jul	=> 7,	July		=> 7,
		aug	=> 8,	Aug	=> 8,	August		=> 8,
		sep	=> 9,	Sep	=> 9,	September	=> 9,
		oct	=> 10,	Oct	=> 10,	October		=> 10,
		nov	=> 11,	Nov	=> 11,	November	=> 11,
		dec => 12,	Dec => 12,	December	=> 12,
	);	


	%BVA::XUI::DATETIME::MONTH_MAX_DAYNUM		= (
		1		=> 31,	'01'	=> 31,	jan		=> 31,	Jan		=> 31,	January		=> 31,
		2		=> 28,	'02'	=> 28,	feb		=> 28,	Feb		=> 28,	February	=> 28,
		'2ly'	=> 29,	'02ly'	=> 29,	febly	=> 29,	FebLY	=> 29,	FebruaryLY	=> 29,
		3		=> 31,	'03'	=> 31,	mar		=> 31,	Mar		=> 31,	March		=> 31,
		4		=> 30,	'04'	=> 30,	apr		=> 30,	Apr		=> 30,	April		=> 30,
		5		=> 31,	'05'	=> 31,	may		=> 31,	May		=> 31,	May			=> 31,
		6		=> 30,	'06'	=> 30,	jun		=> 30,	Jun		=> 30,	June		=> 30,
		7		=> 31,	'07'	=> 31,	jul		=> 31,	Jul		=> 31,	July		=> 31,
		8		=> 31,	'08'	=> 31,	aug		=> 31,	Aug		=> 31,	August		=> 31,
		9		=> 30,	'09'	=> 30,	sep		=> 30,	Sep		=> 30,	September	=> 30,
						'10'	=> 31,	oct		=> 31,	Oct		=> 31,	October		=> 31,
						'11'	=> 30,	nov		=> 30,	Nov		=> 30,	November	=> 30,
						'12'	=> 31,	dec 	=> 31,	Dec 	=> 31,	December	=> 31,
	);	

}





1;
