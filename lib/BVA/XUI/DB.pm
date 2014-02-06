package BVA::XUI::DB;

$BVA::XUI::DB::VERSION	= '1.010.020'; # 2012-08-31 bva@cruzio.com

use strict;

use warnings;

use vars qw/*KEY/;

## _DBI  The basic database interface
sub _DBI {
	my $obj				= $_[0];
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	require BVA::XUI::DATA;
	BVA::XUI::DATA->init;
}

## dbi
## A wrapper around the core DBI method
## Initializes a dbi object, passing its args to the DBI's init()
sub dbi ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
	$KEY{_dbi}			||= $obj->_DBI(@_);
}

## dbh
## Returns the UI object's database handler
## Creates the handler if necessary, passing its args to db_connect()
## Stores the database handler in the metadata fld _dbh 
sub dbh ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
 	$KEY{_dbh} 			||= $obj->db_connect(@_) || '';  # changed 2012-08-31  ||=
}

## db_prepare
## Prepares a statement handler with the optional args.
## Defaults to 'SELECT' (select all fields of all records)
## Returns a statement handler for execution
## Also stores the statement handler in the metadata fld _sth, accessible with method ->sth()
sub db_prepare ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
 	$KEY{_sth} 			= @_ ? $obj->dbh->prepare(shift(@_),@_) : $obj->dbh->prepare('SELECT') || '';
}

## sth
## Returns the current statement handler,
## or returns false with an error message if no statement handler exists.
sub sth ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
	unless ($KEY{_sth}) {
		$obj->charge_err( "This database handler has not prepared a statement handler." );
		return
	}
	
 	$KEY{_sth};
}

## auto_sth
## Returns the current statement handler,
## creating a new one if a) none exists, or
## b) if a statement is provided as an argument.
sub auto_sth ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
 	$KEY{_sth} 			= @_ ? $obj->db_prepare(shift(@_),@_) : $KEY{_sth} ? $KEY{_sth} : $obj->db_prepare();
}

## sth_destroy
## Deletes the current statement handler, returning it
## or returns false with an error message if no statement handler exists.
sub sth_destroy ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
	delete $KEY{_sth} || '';
}


## db_cursor
## Returns a response from the statement handler's iterator, according to the
## optional arg.
## [hdr hdr_str hdr_pat_str pat rec_pat msg sel where stmt pos reset array arrayref hash hashref row str]
## Returns a hashref of the next record if no optional arg is provided.
sub db_cursor ($;$) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
	return $KEY{_sth}->cursor(shift) if $KEY{_sth} && $KEY{_sth}->{iterator};
	
	return;
}

## db_charge
## Charges the object's fields with data from the next record from the iterator;
## returns false if there is no statement handler, or if the 
## iterator has already emitted the last record and not been reset. 
## Charging is restricted to any optional args that are valid fields
## available from the iterator (ie, specified by the handler's SELECT list);
## if no args are provided, all fields available from the iterator are charged.
## 
## NOTE: db_charge IMMEDIATELY clears ALL data fields associated with the database,
## not just the fields available or requested from the iterator;
## this happens even if there turns out to be no handler or no remaining record.
sub db_charge ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	
 	$obj->clear(@{ $KEY{_dbh}->{_head} });
	
 	return unless $KEY{_sth};
 	
 	my $data			= $KEY{_sth}->fetchrow_hashref;
 	return unless $data;
 	
 	my @itr_hdr			= @{ $KEY{_sth}->cursor('hdr') };
 	my %itr_flds		= map { $_ => 1 } @itr_hdr;
	my @flds			= @_ ? grep( { $itr_flds{$_} } @_ ) : @itr_hdr;
	
 	$obj->charge_these([@flds], @$data{@flds});
}

## db_clear
## clears ALL data fields associated with the database
sub db_clear ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

 	$obj->clear(@{ $KEY{_dbh}->{_head} });
 	
	%KEY
}




## db_connect
## A wrapper around the dbi's connect(), allowing args to be tweaked
## before they're passed to the dbi's connect()
## Automatically infuses the database's meta-data.
sub db_connect ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
	my %args;
	my $dbh;
	
	if (ref($_[0]) =~ /HASH/) { 
		%args = %{ $_[0] }
	} elsif (@_>1) {
		push @_ => '' if @_ % 2;
		%args = @_;
	} else {
		%args	= ( file => shift )
	}
	
	if ($args{dbx}) {
		$dbh	= do $args{dbx};
	} elsif ( ($args{dbx} = $args{file} || $args{db} || '') =~ /\.dbx$/ ) {
		$dbh	= do $args{dbx};
	} else {
		$args{input_sep}	||= "\t";
		$dbh 				= $obj->dbi()->connect(\%args);
	}
	
	if ($dbh) {
		$obj->charge_db_meta($dbh)
	} else {
# 		warnings::warnif('io', "Connect problem:" . $dbi->messages('err'));
		$obj->charge_err( "Connect problem:" . $obj->dbi()->messages('err') );
		return
	}
}

## dbx_connect
## A wrapper around the dbi's connect(), allowing args to be tweaked
## before they're passed to the dbi's connect()
## Automatically infuses the database's meta-data.
## dbx_connect uses a dbx initialization cache for quicker repeat connects.
## Will use an existing dbx cache if the filename is given with the arg 'dbx'
## or if the filename of the  'file' or 'db' arg ends in '.dbx'.
## Otherwise, checks for an existing dbx cache, and creates
## one if none exists.
## Always creates a new cache if arg 'refresh' has a TRUE value.
sub dbx_connect ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
	my (%args,$dbh,$dbx);
	
	if (ref($_[0]) =~ /HASH/) { 
		%args = %{ $_[0] };
	} elsif (@_>1) {
		push @_ => '' if @_ % 2;
		%args = @_;
	} else {
		%args	= ( file => shift );
	}
	$args{input_sep}	||= "\t";
	
	if ($args{refresh}) {
		my $tmp_dbh	= $obj->dbi()->connect(\%args)
					or $obj->charge_err( "Connect problem:" . $obj->dbi()->messages('err') ) and return;
		$dbx	= $tmp_dbh->save_connection()
					or $obj->charge_err( "Save Connect problem:" . $obj->dbi()->messages('err') ) and return;
	} elsif ($args{dbx}) {
		$dbx	= $args{dbx};
	} elsif ( ($args{dbx} = $args{file} || $args{db} || '') =~ /\.dbx$/ ) {
		$dbx	= $args{dbx};
	} elsif ( ($args{dbx} = $args{file} || $args{db} || '') =~ s/^(.*)\.txt$/"$1.dbx"/e  ) {
		if (-e $args{dbx}) {
			$dbx	= $args{dbx};
		}
	}

	unless ($dbx) {
		my $new_dbh = $obj->dbi()->connect(\%args)
			or $obj->charge_err( "Connect problem:" . $obj->dbi()->messages('err') ) and return;
		$dbx	= $new_dbh->save_connection();	
	}
	
	$dbh	= do $dbx;
	if ($dbh) {
		$obj->charge_db_meta($dbh)
	} else {
# 		warnings::warnif('io', "Connect problem:" . $dbi->messages('err'));
		$obj->charge_err( "Connect problem:" . $obj->dbi()->messages('err') );
		return
	}
}

## charge_db_meta
## Automatically infuses meta-data from the database handler.
## Creates the handler if necessary, passing its args to db_connect().
sub charge_db_meta ($;@) {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
		
	my $dbh				= $_[0] || $obj->db_connect(@_)
		or return '';
	
	## Give the XUI object the database handler 		
	## and get the meta-data from the database
	$obj->charge_meta({
 		_dbh			=> $dbh,
		_flds			=> [ $dbh->fields ],
		_meta			=> $dbh->meta,
		_type_RE		=> $dbh->type_RE,
		_struct			=> $dbh->db_struct,
	});
	
	$obj->list_items($dbh->db_base_name() . '_fld_list', $dbh->fields);
	$obj->list_items($dbh->db_base_name() . '_label_list', $dbh->labels);
	
}


1;



