package BVA::XUI::DATA;

$BVA::XUI::DATA::VERSION	= '4.030_001';	# 2013-03-06 bva@cruzio.com

use warnings;
use strict;

use Fcntl qw{:DEFAULT :flock};

use parent qw{BVA::XUI::DATA::WHERE BVA::XUI::DATA::META};

## Create a database handler
sub init {
	my $class	= shift;
	my $args	= shift;
	my %args;
	if (ref($args) =~ /HASH/) {
		%args = %{ $args }
	} elsif ($args) {
		unshift @_, $args;
		push @_ => '' if @_ % 2;
		%args = @_;
	}

    my %settings = (
		_status			=> {saveable	=> 0,						 },
		_msg			=> make_db_msg( $args{msg}				|| ''),
		_err			=> make_db_msg( $args{err_msg}			|| ''),
		_procedures		=> $args{procedures}					|| { default => sub { @_ } },
		_file			=> $args{file} || $args{db}				|| '',
		_output_sep		=> $args{output_sep}					|| '|',
		_input_sep		=> $args{input_sep}						|| "\\|",
		_input_ftype	=> $args{input_ftype}					|| "txt",
		_record_sep		=> $args{record_sep}					|| "\n",
		_item_sep		=> $args{item_sep}						|| ',|;',
		_record_out_sep	=> $args{record_out_sep}				|| "\n",
		_idx_sep		=> $args{idx_sep}						|| "\n\n",
		_table_col_sep	=> $args{_table_col_sep}				|| '=x=',
		_table_sep		=> $args{table_sep}						|| '#-#-#',
		_db_end			=> $args{db_end}						|| '^\#\-\#\-\#',
		_db_suf			=> $args{db_suf}						|| '.txt',
		_sem_suf		=> $args{sem_suf}						|| '.lock',
		_idx_suf		=> $args{idx_suf}						|| '.idx',
		_dbx_suf		=> $args{dbx_suf}						|| '.dbx',
# 		_line_skip		=> $args{line_skip}						|| '^($| *\#| *\<\!)',
		_line_skip		=> $args{line_skip}						|| '^($| *\#)',
		_line_blank		=> $args{line_blank}					|| '^\s*$',
		_line_max		=> $args{line_max}						|| 2_000_000,
		_id_col			=> $args{id_col}						|| '',
		_id_override	=> $args{id_override}					|| 0,
		_head_max		=> $args{head_max}						|| 10,
		_def_type		=> $args{def_type}						|| 't',
		_def_size		=> $args{def_size}						|| '24',
		_tracking		=> $args{tracking}						|| [ @BVA::XUI::DATA::PROC::tracks ],
		_sort_packs		=> { %BVA::XUI::DATA::META::sort_packs,		%{ $args{sort_packs}	|| {} } },
		_record_packs	=> { %BVA::XUI::DATA::META::record_packs,	%{ $args{record_packs}	|| {} } },
		_fld_packs		=> { %BVA::XUI::DATA::META::fld_packs,		%{ $args{fld_packs}		|| {} } },
		_fld_fmts		=> { %BVA::XUI::DATA::META::fld_fmts,		%{ $args{fld_fmts}		|| {} } },
		_nulls			=> { %BVA::XUI::DATA::META::nulls,			%{ $args{nulls}			|| {} } },
	);

    return bless \%settings, $class;
}

 ## create a database handler from a saved connection (.dbx)
sub init_from_dbx {
	my $class				= shift;
    my %settings    		= %{ shift() };
    $settings{_msg} 		= make_db_msg('');
    $settings{_err} 		= make_db_msg('');
    $settings{_procedures}  = { default => sub { @_ } };
    my $dbh	= bless \%settings, $class;
    return $dbh;
}

## Create a database handler
sub connect {
	my $self	= shift;
	my %args;
	if (ref($_[0]) =~ /HASH/) {
		%args = %{ $_[0] }
	} elsif ($_[0]) {
		push @_ => '' if @_ % 2;
		%args = @_;
	}

	## Make a statement handler object,
	## starting with a clone of the database handler
	my $obj				= { %$self };

	## get filename from file= or db= arg
    $obj->{_file} = ($args{file} || $args{db} || $obj->{_file})
        or do { $self->{_err}->("$0: No filename."); return; };

	$obj->{_file} =~ /^([_\w.\/\\: -]+)$/
		and $obj->{_file} = $1
			or do { $self->{_err}->(qq/$0: Connect failed on $obj->{_file}: Filename.\n/), return};

	-e $obj->{_file}
		or do { $self->{_err}->(qq/$0: Connect failed on $obj->{_file}: File doesn't exist.\n/), return};

	$obj->{_fsize} = -s $obj->{_file};
	$obj->{_fsize} > 0
		or do { $self->{_err}->(qq/$0: Connect failed on $obj->{_file}: File is empty.\n/), return};

	$obj->{_fsize_note}	= reverse int $obj->{_fsize}/1024;
	$obj->{_fsize_note}	=~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
	$obj->{_fsize_note}	= reverse $obj->{_fsize_note};
	$obj->{_fsize_note}	= '<1' if (!$obj->{_fsize_note} and $obj->{_fsize} > 0);

	## Separators
	$obj->{_input_sep}	= $args{input_sep}			||= $obj->{_input_sep};
	$obj->{_output_sep}	= $args{output_sep} 		||= $obj->{_input_sep}		|| $obj->{_output_sep};
	$obj->{_item_sep}	= $args{item_sep} 			||= $obj->{_item_sep}		|| $obj->{_item_sep};

	## Have to be able to accept a null value for $/
	$obj->{_record_sep} 	= (exists $args{record_sep}		?  $args{record_sep} 		: $obj->{_record_sep});
	$obj->{_record_out_sep} = (exists $args{record_out_sep}	?  $args{record_out_sep} 	: $obj->{_record_out_sep});
	$obj->{_idx_sep}		= (exists $args{idx_sep}		?  $args{idx_sep}    		: $obj->{_idx_sep});
	$obj->{_table_col_sep}	= $args{_table_col_sep}										|| $obj->{_table_col_sep};
	$obj->{_table_sep}		= $args{table_sep}											|| $obj->{_table_sep};

	## Line Filtering
	$obj->{_line_skip}	= $args{line_skip}			|| $obj->{_line_skip};
	$obj->{_line_blank}	= $args{line_blank}			|| $obj->{_line_blank};

	## Internal limit of records read
	$obj->{_line_max}	= $args{line_max}			|| $obj->{_line_max};

	## File suffixes
	( $obj->{_db_suf}	= $args{db_suf}		|| $obj->{_db_suf}	)	=~ s/^(\.?[A-Za-z_0-9])$/$1 || ''/e;
	( $obj->{_sem_suf}	= $args{sem_suf}	|| $obj->{_sem_suf}	)	=~ s/^(\.?[A-Za-z_0-9])$/$1 || ''/e;
	( $obj->{_idx_suf}	= $args{idx_suf}	|| $obj->{_idx_suf}	)	=~ s/^(\.?[A-Za-z_0-9])$/$1 || ''/e;
	( $obj->{_dbx_suf}	= $args{dbx_suf}	|| $obj->{_dbx_suf}	)	=~ s/^(\.?[A-Za-z_0-9])$/$1 || ''/e;

	## Number of lines before connect() stops waiting
	## for a blank line separating header from data:
	$obj->{_head_max}	= $args{head_max} ||= $obj->{_head_max};

	## Check file locking on semaphore file, creating it if necessary
	my $lock			= $self->check_lock($obj->{_file}) or return;
	my @try_lines;

	{
		local $/ = $obj->{_record_sep};

		## Open the database file
		open (DB_IN, "<$lock->{FILE}")
			or do { $self->{_err}->(qq/log: $0: Connect failed on $lock->{FILE}: \n$!\n/), return};

		## Grab the first [head_max] + 1 # of lines

		for (0..$obj->{_head_max}) {
			my $line = <DB_IN>;
			last unless (defined($line)); #  and $line !~ /^$/
			chomp $line;
#			last unless (defined($line) and $line !~ /$obj->{_line_blank}/);
#			last unless (defined($line) and $line !~ /^$obj->{_input_sep}*$obj->{_record_sep}$/);
			push @try_lines, $line;
		};

		## Close database file.
		close DB_IN
			or do { $self->{_err}->(qq/log: $0: Connect failed on closing $lock->{FILE}: \n$!\n/), return};
	}

	## Now get the good header lines
	my @headlines;
	for (@try_lines) {
		last unless (defined($_) and /[^$obj->{_input_sep}]/ and !/^$/);
		push @headlines, $_
	}

	if (@headlines == @try_lines) {
		## The case where the first line is the entire header, with no blank line separator
		@headlines = ($try_lines[0]);
		$obj->{_line0} = 0;
		$obj->{_line0_byte} = 0;
	} else {
		## Otherwise, must have been a blank line
		$obj->{_line0} = @headlines + 1;
	}

	## Now re-open the database file, get the line zero byte count, and
	## count or estimate the number of records; close again.
	$obj->{_num_recs}		= 0;
	$obj->{_num_recs_note}	= '';

	{
		local $/ = $obj->{_record_sep};

		open (DB_IN, "<$lock->{FILE}");
		SKIP: while (<DB_IN>) {
			last SKIP if $. >= $obj->{_line0} + 0;
		}

		$obj->{_line0_byte} 	= tell DB_IN;

		if ($obj->{_fsize} < 20480000) {
			while (<DB_IN>) { ++$obj->{_num_recs} }
			$obj->{_num_recs_note}	= "count";
		} else {
			my $bytes	= 0; my $recs = 0;
			while (<DB_IN>) { ++$recs; $bytes += length($_); last if $. == 1000 }
			$obj->{_num_recs}		=  $bytes ? int($recs * $obj->{_fsize} / ($bytes * 1.024)) : 0;
			$obj->{_num_recs_note}	= "estimate";
		}
		#	# Fast way to get exact record count ?
		# 	$obj->{_num_recs}		+= tr/\n/\n/ while sysread(DB_IN, $_, 2 ** 16);
		#  	$obj->{_num_recs_note}	= "tr_count";

		close DB_IN;
	}

	$self->{_msg}->("log: Connect: $lock->{FILE} ");


	## THEN release the lock
	$lock->{RELEASE}->();

	my @str			= grep { defined and !/^( *\#| *\<\!)/ } @headlines;

	my %struct		= map { /^([^$obj->{_input_sep}]*?):(.*)$/s ? ($1 => [ split $obj->{_input_sep}, $2 ]) : () } @str;

	unless (exists $struct{header}) {
		$struct{header}	= $struct{fields} || [ split $obj->{_input_sep}, $str[0] ]
	}

	my @head			= map { /\S/ ? $_ : 'xfld' . ++$obj->{_anon_fld_num} } @{ $struct{header} };
	$head[0]			= $head[0] eq 'xfld1' ? '_num' : $head[0];
	my @cols			= ("0E0",1..$#head);

	$obj->{_def_type}	= $args{def_type} ||= $obj->{_def_type};
	$obj->{_def_size}	= $args{def_size} ||= $obj->{_def_size};

	$obj->{_sort_packs}	= { %{ $obj->{_sort_packs} },	%{ $args{sort_packs} || {} } };

	$obj->{_fld_fmts}	= { %{ $obj->{_fld_fmts} },		%{ $args{fld_fmts} || {} } };

	$obj->{_nulls}		= { %{ $obj->{_nulls} },		%{ $args{nulls} || {} } };

	for my $col (@cols) {
		my $fld = $head[$col];

		## Labels (field names)
		$struct{labels}->[$col] = $args{labels}->[$col] || $struct{labels}->[$col] 
			|| join ' ' => map { ucfirst } split /[ _]+/ => lc $fld;

		## Formats (field type & size)
		$struct{formats}->[$col] 	= $args{formats}->[$col] || $struct{formats}->[$col] || "$obj->{_def_type}:$obj->{_def_size}";
		my ($type, $size) 			= split /:/ => $struct{formats}->[$col];
		$struct{types}->[$col]		= $type ||= $obj->{_def_type};
		$struct{sizes}->[$col]		= $size ||= $obj->{_def_size};

		# Deal with fmt a:60.4
		($size)						= split /\./ => $size;

		# Formats for packing and listing
		($struct{pack_pats}->[$col]		= $obj->{_sort_packs}->{$type})	=~ s/#/$size/ge;
		($struct{record_packs}->[$col]	= $obj->{_record_packs}->{$type}) =~ s/#/$size/ge;
		($struct{col_pats}->[$col] 		= $obj->{_fld_fmts}->{$type})	=~ s/#/$size/ge;

		# Null values  by field type
		$struct{nulls}->[$col]		= $obj->{_nulls}->{$type};

		## Indexing keys and default index column.
		if (defined($args{keys}->[$col])) {
			$struct{keys}->[$col]	= (!$args{keys}->[$col] && ~$args{keys}->[$col]) ? '0' :
											$args{keys}->[$col] || $struct{keys}->[$col]
		}
		$struct{keys}->[$col]		||= 0;

		## Set index column. Unpredictable behavior if more than one column has the key == 1
		if ($struct{keys}->[$col] and $struct{keys}->[$col] == 1) {
			$obj->{_id_col} ||= $col; # To avoid problems, only use the first (left-most) key == 1
		}

		## Default Values start with Nulls
		my $fld_default				= $struct{nulls}->[$col];

		## Default values provided by the db struct
		if (defined($struct{defaults}->[$col])) {
			$fld_default			=  (!$struct{defaults}->[$col] && ~$struct{defaults}->[$col]) ? '0' :
											$struct{defaults}->[$col] || $fld_default
		}

		## Default values provided by args to connect()
		if ( defined($args{defaults}->[$col]) ) {
			$fld_default			= (!$args{defaults}->[$col] && ~$args{defaults}->[$col]) ? '0' :
												$args{defaults}->[$col] || $fld_default
		}

		## It's my default!
		$struct{defaults}->[$col]	= $fld_default;
		
		next unless $fld eq $obj->{_table_col_sep};   # config default: =x=

		$struct{types}->[$col]			= 's';
		$struct{sizes}->[$col]			= '0';
		$struct{nulls}->[$col]			= $obj->{_nulls}->{'s'};;
		$struct{keys}->[$col]			= '0';
		$struct{defaults}->[$col]		= '';
		($struct{pack_pats}->[$col]		= $obj->{_sort_packs}->{'s'})	=~ s/#/0/ge;
		($struct{record_packs}->[$col]	= $obj->{_record_packs}->{'s'}) =~ s/#/0/ge;
		($struct{col_pats}->[$col] 		= $obj->{_fld_fmts}->{'s'})		=~ s/#/0/ge;
	}

	## Indexing of columns in struct
	$obj->{_id_col}					= $args{id_col} || $obj->{_id_col} || "0E0"; # '0 but true';

	unless ( $struct{keys}->[$obj->{_id_col}] == 1) {
		for my $col (0..$#{$struct{keys}}) {
			if ($col == $obj->{_id_col}) {
				$struct{keys}->[$col] = 1
			} else {
				$struct{keys}->[$col] ||= '' # allow keys > 1
			}
		}
	}

	$obj->{_id_fld}		= $head[ $obj->{_id_col} ];

	# OK, now that id is settled, make an index pack pattern
 	$obj->{_id_pack}	= join ' ' => $struct{pack_pats}->[$obj->{_id_col}], 'N';

	$obj->{_head}		= [ @head ];
	$obj->{_hd_nums}	= { map { $head[$_] => $_							} @cols };
	$obj->{_labels}		= { map { $head[$_] => $struct{labels}->[$_]		} @cols };
	$obj->{_types}		= { map { $head[$_] => $struct{types}->[$_]			} @cols };
	$obj->{_sizes}		= { map { $head[$_] => $struct{sizes}->[$_]			} @cols };
	$obj->{_col_pats}	= { map { $head[$_] => $struct{col_pats}->[$_]		} @cols };
	$obj->{_pack_pats}	= { map { $head[$_] => $struct{pack_pats}->[$_]		} @cols };
	$obj->{_rec_pats}	= { map { $head[$_] => $struct{record_packs}->[$_]	} @cols };

	$obj->{_defaults}	= { map { $head[$_] => $struct{defaults}->[$_]		} @cols };
	$obj->{_nulls}		= { map { $head[$_] => $struct{nulls}->[$_]			} @cols };
	$obj->{_keys}		= { map { $head[$_] => $struct{keys}->[$_]			} @cols };

	$obj->{FILE}		= $lock->{FILE};
	$obj->{FILE_DIR}	= $lock->{FILE_DIR};
	$obj->{FILE_NAME}	= $lock->{FILE_NAME};
	$obj->{FILE_SUF}	= $lock->{FILE_SUF};
	$obj->{ID_FILE}		= $lock->{ID_FILE};
	$obj->{LOCK_OK}		= $lock->{LOCK_OK};

	$obj->{_struct}		= [ [ 'header',		@head					],
							[ 'labels',		@{$struct{labels}}		],
							[ 'formats',	@{$struct{formats}}		],
							[ 'defaults',	@{$struct{defaults}}	],
							[ 'keys',		@{$struct{keys}}		],
						];

	$obj->{_header}		= { 'fields'	=> [ @head 					],
							'labels'	=> [ @{$struct{labels}} 	],
							'formats'	=> [ @{$struct{formats}} 	],
							'defaults'	=> [ @{$struct{defaults}} 	],
							'keys'		=> [ @{$struct{keys}} 		],
						};

	$obj->{_status}->{saveable}++;

	my $location		= shift || $self->{FILE_DIR};

	# indexes
	my @current_indexes	= glob "${location}$obj->{FILE_NAME}_index*$obj->{_idx_suf}";
 	my %indexes;
	for my $idx (@current_indexes) {
		my ($idx_name,$idx_case,$idx_flds);
		if ($idx =~ /^.*_index$self->{_idx_suf}$/) {
			$idx_flds	= [ $self->{_id_fld} ];
			$idx_case	= 'ncase';
			$idx_name	= 'base';
		} elsif ($idx =~ /^.*_index-((?:low|up)case)$self->{_idx_suf}$/) {
			$idx_flds	= [ $self->{_id_fld} ];
			$idx_case	= $1;
			$idx_name	= "base-$idx_case";
		} elsif ($idx =~ /^.*_index-(.+?)(?:|-((?:low|up)case))?$self->{_idx_suf}$/) {
			$idx_flds	= [ split '-' => $1 ];
			$idx_case	= $2 || 'ncase';
			$idx_name	= join '-' => grep { $_ ne 'ncase' } $1, $idx_case;
		} else {
			next;
		}
		$obj->{_indexes}->{$idx_name}->{case}	= $idx_case;
		$obj->{_indexes}->{$idx_name}->{path}	= $idx;
		$obj->{_indexes}->{$idx_name}->{flds}	= $idx_flds;
		$obj->{_indexes}->{$idx_name}->{name}	= $idx_name;
	}

	# add code_refs	& messages
	$obj->{_msg}->($args{msg} || "");
	$obj->{_err}->($args{err_msg} ? "log: $args{err_msg}" : '');
	$obj->{_procedures}	= { ( %{ $obj->{_procedures} }, %{ $args{procedures} || {} } ) };

    return bless $obj, ref($self) || $self;
}

sub save_connection {
	my $self			= shift();
	unless ($self->{_status}->{saveable}) {
		$self->{_err}->("log: Save Connection failed: Not Saveable.") and return;
	}

	my $location		= shift || $self->{FILE_DIR};

	my $conn_file		= "${location}$self->{FILE_NAME}$self->{_dbx_suf}";

	my %connection		= map { ($_ eq '_procedures' || ref($self->{$_}) =~ /(CODE|GLOB)/) ? () : ($_ => $self->{$_}) } keys %{ $self };

 	$connection{_indexes}	= $self->get_indexes();

	my $save_time		= localtime();

 	use Data::Dumper;
	$Data::Dumper::Purity	= 1;
 	$Data::Dumper::Sortkeys	= 1;
	open my $dbx, ">", $conn_file
		or $self->{_err}->("log: Save Connection failed on Open: $!") and return;

	print $dbx <<"INIT";

# Create database handler for
# $self->{FILE_DIR}$self->{FILE_NAME}$self->{_dbx_suf}
# $save_time

use BVA::XUI::DATA;

INIT

	print $dbx 'my ' . Data::Dumper->Dump([\%connection], [qw/*_DBX/]);

	$Data::Dumper::Purity	= 0;

	print $dbx <<"CONN";

BVA::XUI::DATA->init_from_dbx(\\%_DBX);

CONN

	chmod 0755, $dbx;
	close $dbx
		or $self->{_err}->("log: Save Connection failed on Close: $!") and return;

	$self->{_dbx}	= $conn_file;
}


sub get_idx {
	my $self	= shift;
	my $idx		= shift;
	my %meta	= %{ shift() };

	my (%ixs,%ids,%nums,%bytes,@byte_locs)	= ( (), (), (), () );

	while (<$idx>) {
		chomp;
		my ($id,$num,$byte,@cells);
#		@cells   = unpack $meta{PATTERN}, $_;
		@cells	 = split "\x00" => $_;
		$byte    = $cells[-1];
		$num     = $cells[-2];
		$id      = $cells[-3];
		next unless defined $byte;
		$nums{$num} 	= $byte;
		$ids{$id}   	= $byte;
		$bytes{$byte}	= $id;
		foreach my $f (0..$#{$meta{FIELDS}}) {
			$cells[$f] = '_NO_' . $meta{FIELDS}->[$f] unless length($cells[$f]);
			push @{ $ixs{$meta{FIELDS}->[$f]}->{$cells[$f]}} => $byte;
			push @{ $ixs{$meta{ID_FLD}}->{$id} } => $byte;
		}
		push @byte_locs => $byte;
	}

  	our $INDEX	= @byte_locs ?
		{
			result			=> '__OK__',
			meta			=> \%meta,
			indices			=> \%ixs,
			$meta{ID_FLD}	=> \%ids,
			idxN			=> \%nums,
			byte			=> \%bytes,
		} :
		{
			result		=> '__NADA__',
			meta		=> \%meta,
		};

	return $INDEX;
}


## File metadata
# 	$self->{FILE}		= $lock->{FILE};
# 	$self->{FILE_DIR}	= $lock->{FILE_DIR};
# 	$self->{FILE_NAME}	= $lock->{FILE_NAME};
# 	$self->{FILE_SUF}	= $lock->{FILE_SUF};
# 	$self->{ID_FILE}	= $lock->{ID_FILE};
# 	$self->{LOCK_OK}	= $lock->{LOCK_OK};

sub DB_FILE {
	my $self			= shift();
	$self->{FILE} || '';
}

sub DB_FILE_DIR {
	my $self			= shift();
	$self->{FILE_DIR} || '';
}

sub DB_FILE_NAME {
	my $self			= shift();
	$self->{FILE_NAME} && $self->{FILE_SUF} ?
		"$self->{FILE_NAME}$self->{FILE_SUF}" :
			'';
}

sub DB_BASE_FILE_NAME {
	my $self			= shift();
	$self->{FILE_NAME} || '';
}

sub DB_FILE_SUF {
	my $self			= shift();
	$self->{FILE_SUF} || '';
}

sub DB_ID_FILE {
	my $self			= shift();
	$self->{ID_FILE} || '';
}

sub DB_LOCK_OK {
	my $self			= shift();
	$self->{LOCK_OK} || '';
}


sub prepare {
	my $self			= shift();
	my $sttmt			= shift();
	my $prep_sttmt;

	if (ref($sttmt)) {
		$prep_sttmt	= $sttmt;
	} else {
		my $splitters		=
			'SELECT|UPDATE|INSERT|DELETE|COUNT|BROWSE|INDEX|CREATE|IMPORT|FIELDS|SET|RECORD|VALUES|INTO|TO|FROM|WHERE|ORDER|WITH|LIMIT|PROC|ACTION|WHEN|ALIAS';
		chomp $sttmt;
		my @sttmt_items		= split /\s*\b($splitters)\b\s*/, $sttmt;  # *** 2006-08-30 \s*\b($splitters)\b\s*
		shift @sttmt_items;
		push @sttmt_items, '' if @sttmt_items % 2;
		$prep_sttmt		= { @sttmt_items };
		foreach (keys %{ $prep_sttmt } ) {
			next if ref($prep_sttmt->{$_});
			$prep_sttmt->{$_} ||= '';
			$prep_sttmt->{$_} =~ s/^\s*|\s*$//gs;
		}

		for ( qw/SELECT UPDATE INSERT DELETE COUNT BROWSE INDEX CREATE IMPORT ACTION/ ) {  # MARK DESTROY
			if (exists $prep_sttmt->{$_}) { $prep_sttmt->{ACTION} = $_ =~ /^ACTION$/ ?  $prep_sttmt->{ACTION} : lc( $_ ); last }
		}
	}

	# WITH  in/output_sep, metadata flags (%args)
	if ($prep_sttmt->{WITH}) {
		$prep_sttmt->{WITH}		= { map {
			my ($key, $sep, $val) = split / *([:=]>?) ?/;
			if ($key) {
				if ($sep) {
					$val ||= 0;
				} else {
					$val ||= 1;
				}
				($key, $val)
			} else {
				()
			}
		} ( split / *[,;] */s, $prep_sttmt->{WITH} ) };
 	} else {
 		$prep_sttmt->{WITH}		= {};
 	}

 	$prep_sttmt->{LIMIT}		= $prep_sttmt->{LIMIT} ? [ split / *, */s, $prep_sttmt->{LIMIT} ] : [ ($self->{_line_max}, 0) ];
	$prep_sttmt->{LIMIT}->[0]	= ($prep_sttmt->{WITH}->{first_only} ? 1 : ( $prep_sttmt->{WITH}->{max_count} || $prep_sttmt->{LIMIT}->[0]) );

	my %extra			= @_;

# 	# here's a PROC added to %extra:
# 	my $db	= $sth->prepare(qq{
# 		SELECT * WHERE all TO db PROC zero_pad 12, range;
# 	},
# 		zero_pad => sub {
# 			my $self	= $_[0];
# 			...
# 		},
# 		)->execute()
# 	or die "Database statement execution failure: " . $dbh->messages('err');

	# PROC parse to separate procedures w/ args;
	# Look up procedures in %extras and in the BVA::XUI::DATA::PROC library;
	# Create a closure around each found procedure and its args, and give them to the statement object
	# Preserves the order of PROCs in the statement
	# Added Mar 2013: results tracking
	# Prepending the PROC name with an alpha-only word and a colon registers that word
	# as a selector variable that will be returned with the results() method;
	# Example: $sth	= dbh->prepare(qq{SELECT * WHERE all PROC COUNTS:count_unique,city;});
	# After sth->execute(), sth->results('COUNT') has the values collected (assuming 
	# count_unique cooperates by putting its return values into $self->{COUNTS}).
	# In this example, COUNTS: is actually unnecessary, because COUNTS is tracked by default,
	# in @BVA::XUI::DATA::PROC::tracks; currently these are COUNTS DUPES UNIQUE SUMS RECORDS TOTALS
	$prep_sttmt->{_procs} 		= [ map {
			my ($proc, $args)	= split /\s*[ ,\(]\s*/ => $_ , 2;
			$args				||= '';
			my $process;
			if ($proc) {
				if ($proc =~ s/^([a-zA-Z]+):(.*)$/$2/) {
					push @{ $self->{_tracking} } => $1;
				}
				if (exists $extra{$proc}) {
					$process						= $extra{$proc};
					$self->{_procedures}->{$proc}	= $extra{$proc};
				} elsif (exists $self->{_procedures}->{$proc}) {
					$process						= $self->{_procedures}->{$proc};
				} elsif (defined &{ "BVA::XUI::DATA::PROC::$proc" }) {
					$process						= \&{ "BVA::XUI::DATA::PROC::$proc" };
				}
				if ($process) {
					$args =~ s/^\s*(.*?)\s*[,\)]*$/$1 || ''/e;
					sub { $_[0]->$process( split( / *, */ => $args) ) }
				}
			} else {
				()
			}
		} split( /\s*;\s*/s => $prep_sttmt->{PROC} || '')
	];

	unshift @_, $self, $prep_sttmt;
	goto &{ 'prep_' . $prep_sttmt->{ACTION} };
}

# Deprecated: in old versions takes hash instead of string SQL statement. prepare() now does that.
sub prepstmt {
	goto &prepare;
}


sub execute {
	goto &{'do_' . $_[0]->{ACTION} };
}


# Cursor from SELECT
sub cursor {
	my $self	= shift;
	my $arg		= shift || 'hashref';

	if ($self->has_iterator) {
		return $self->iterator($arg);
	}

	$self->{_err}->("log: No cursor available from " . ($self->{ACTION} || 'unknown action') . "\n");
	return;
}

sub iterator {
	my $self	= shift;
	my $arg		= shift || 'hashref';

	if ($self->{iterator}) {
		return $self->{iterator}->($arg);
	}

	$self->{_err}->("log: No iterator available from " . ($self->{ACTION} || 'unknown action') . "\n");
	return;
}

sub has_iterator {
	my $self	= shift;

	return $self->{iterator} ? 1 : 0;
}

sub results {
	my $self	= shift;
	
	$self->{_results} = { map { exists $self->{$_} ? ($_ => $self->{$_}) : () } $self->result_tracking() };
	
	my $which_result	= shift || '';
	
	return $self->{_results} unless $which_result;
	
	unless (exists $self->{_results}->{$which_result}) {
		$self->{_results}->{$which_result} = {};
	}
	return $self->{_results}->{$which_result};
}

sub result_tracking {
	my $self	= shift;
	
	return @{ $self->{_tracking} };
}


# Cursor Metadata Methods
# 	'hdr'
# 	'hdr_str'
# 	'hdr_list_str'
# 	'hdr_pat_str'
# 	'pat'
# 	'rec_pat'
# 	'msg'
# 	'sel'
# 	'where'
# 	'stmt'

#	[TO_DO]

# Cursor State Methods
#	'num'
# 	'#'
# 	'pos'
# 	'reset'

sub cursor_numrecs {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('num');
}

sub cursor_numleft {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('#');
}

sub cursor_position {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('pos');
}

sub cursor_reset {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('reset');
}




# SELECT
sub prep_select {
	my $self				= shift();
	my (%statement, @flds, @cols, $colpat);
	%statement				= %{ shift() };
	$statement{SELECT}		||= '*';  # * returns all fields
	@flds					= map { $_ eq '*' ? $self->head : $_ } split /\s*,\s*/, $statement{SELECT};
	@cols					= $self->columns(@flds);
	$colpat					= join ' ', $self->col_formats(@flds);

	# FROM	# If more than one, assumes original $self->struct
	$statement{_from}		= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ( ref($statement{FROM}) ? @{ $statement{FROM} } : (split /\s*,\s*/, ($statement{FROM} || 'me')) ) ];

	# ORDER
	$statement{ORDER}		= $statement{ORDER} ? $statement{ORDER} =~ /unsorted/i ? '' : $statement{ORDER} : '';  # sort1, sort2 ..
	my $sort_nums			= [ map { $self->{_hd_nums}->{$_} || () } ( split /\s*,\s*/, $statement{ORDER} ) ];
	my $sort_flds			= [ @{ $self->{_head} }[@{ $sort_nums }] ];
	my $sortpack			= join ' ', (map {$self->{_pack_pats}->{$_} } @{ $sort_flds }), 'N';
	my $idxpack				= join ' ', $sortpack, 'N';
	$statement{ORDER}		= { cols => $sort_nums, flds => $sort_flds, sortpack => $sortpack };

 	# WHERE		            # fld|?(=|>|<|!|==|>>|<<|!!|~|!~|^|`|*|!*)val
	unless ($statement{WHERE} and $statement{WHERE} eq '?') {
  		$statement{selector}	= $self->make_selector($statement{WHERE});
  	}

	# Pack pattern for the tmp file of selected data.
	my $record_pack			= @flds ? join( ' ', map( { $self->{_rec_pats}->{$_} || '' } @flds)) : '';
	$statement{_select}		= { cols => \@cols, flds => \@flds, colpack => $colpat, idxpack => $idxpack, recordpack => $record_pack};

	# TO  - NOTE: TO is deprecated in SELECT statements;
	# Instead, use fetchrow and fetchall variants if possible
	# Still needed for TO file/db*
	if ($statement{TO}) {
		$statement{TO}			=~ s/^\s*(table|file|db[xh]?|cursor)\s*[,:=]?>?\s*(.*)$/$1=>$2/
									or $statement{TO} = 'table=>'.$statement{TO};
		$statement{TO}			= [ (map { split /\s*=>\s*/ } $statement{TO} ) ];
		{
		local $_ = $statement{TO}->[0];
		/(file|db[xh]?)/i	and do {
			my $out_file		= $statement{TO}->[1] || $statement{_from}->[0];
			my $index_file;
			if (-e $out_file and not $statement{WITH}->{file_override}) {
				my ($name,$suf,$nbr)	= ('', '', 00);
				($name,$suf)			= $out_file =~ /^(.*?)(\.\w{1,4})?$/;
				$suf					||= $self->{_db_suf};
				$nbr++					while (-e "$name.$nbr$suf");
				$out_file				= "$name.$nbr$suf";
				$index_file				= "$name.${nbr}.$self->{_idx_suf}";
			}
			$statement{TO}->[1]			= $out_file;
			$statement{TO}->[2]			= $index_file;

		  }
		  or
		/cursor/i		and do {
			$statement{TO}->[1]			||= '';
		  }
		  or
		/table/i		and do {
			$statement{TO}->[1]			||= 'text';
		  }
		}
	}

	bless { %{ $self }, %statement }, ref($self) || $self
}


# UPDATE
sub prep_update {
	my ($self, $sttmt) = @_;
	my %statement = %{ $sttmt };

	$statement{_update}		= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{UPDATE}) ? @{ $statement{UPDATE} } : split( /\s*,\s*/, ($statement{UPDATE} || 'me')) ];

	# SET
	$statement{SET} ||= {};  # fld1 = val1; fld2: val2
	$statement{SET} = $statement{SET} eq '?' ? {} : $statement{SET};
	unless ( ref($statement{SET}) ) {
		$statement{SET} =~ s/("(.*?)")|(\s*[,;]\s*)/$2 || '|'/ge;
		my @SET_items = split /\|/s, $statement{SET};
		$statement{SET} = { map {
			my ($key, $val) = split / *[:=]>? ?/, $_, 2;
			$val ||= '';
			($key, $val)
		} @SET_items };
	}

 	# WHERE				# fld(=|>|<|!|==|>>|<<|!!|~|!~|^|_|`|*|!*)val  (` deprecated)
	unless ($statement{WHERE} eq '?') {
  		$statement{selector}	= $self->make_selector($statement{WHERE});
  	}

	bless { %{ $self }, %statement }, ref($self) || $self
}

# INSERT
sub prep_insert {
	my ($self, $sttmt) = @_;
	my %statement = %{ $sttmt };

	# INTO
	$statement{_into}	= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{INTO}) ? @{ $statement{INTO} } : split( /\s*,\s*/, ($statement{INTO} || 'me')) ];

	# INSERT
	my (%record);
	my @flds = @{ $self->{_head} };

	# Start the new record with NULLS
	for my $fld (@flds) {
		$record{$fld}	= $self->{_nulls}->{$fld}
	}

	# Now add DEFAULTS, if specified in SQL statement (WITH use_defaults: 1)
	if ($statement{WITH}->{use_defaults}) {
		for my $fld (@flds) {
			next unless defined($self->{_defaults}->{$fld});
			next if ($self->{_defaults}->{$fld} =~ /^=/ and $statement{WITH}->{no_calcs});
			my @defs		= split /\s*(?:,|\||-)\s*/ => $self->{_defaults}->{$fld};
			my $def_val		= "";
			if (@defs == 1 or @defs = grep { /^\*/ } @defs ) {
				$def_val	= $defs[0];
				$def_val	=~ s/^\*(.*)$/$1/s; # ** 2010-08-11
			}

			$record{$fld}	= (!$def_val && ~$def_val) ? '0' : $def_val || $record{$fld};
		}
	}

	# Now add a RECORD, if any, from the SQL statement
	if (defined($statement{RECORD})) {
		my @REC;
		$statement{RECORD}	=~ s/(^"|"$)//g;
		if (ref($statement{RECORD})) {
			@REC = @{ $statement{RECORD} };
		} else {
			@REC = split $self->{_input_sep} => $statement{RECORD};
		}

		for my $col (0..$#flds) {
			if ( defined($REC[$col]) ) {
				$record{$flds[$col]} = (!$REC[$col] && ~$REC[$col]) ? '0' :
					$REC[$col] || $record{$flds[$col]}
			}
		}
	}

	# Now add VALUES, if any, from the SQL statement
	if (defined($statement{VALUES})) {
		my %VALS;
		if (ref($statement{VALUES})) {
			%VALS = %{ $statement{VALUES} };
		}
		elsif ($statement{VALUES} eq '?') {
			%VALS = ();
		}
		else {
			%VALS = map {
				my ($key, $val) = split / *[:=]>? ?/;
				defined($val) or $val = '';
				$val = (!$val && ~$val) ? '0' : $val;
				($key, $val)
			} split / *[,;] */s => $statement{VALUES};
		}

		%record = ( %record, %VALS );
	}

	$statement{INSERT} = [ map { /^\s*(.*?)\s*$/os; $1 } @record{ @flds } ];

	bless { %{ $self }, %statement }, ref($self) || $self
}

# IMPORT
sub prep_import {
	my ($self, $sttmt) = @_;
	my %statement = %{ $sttmt };

	# INTO
	$statement{_into}	= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{INTO}) ? @{ $statement{INTO} } : split( /\s*,\s*/, ($statement{INTO} || 'me')) ];


	# FROM
	$statement{_from}	= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{FROM}) ? @{ $statement{FROM} } : split( /\s*,\s*/, ($statement{FROM} || 'me')) ];

	# IMPORT_DEFAULT
	my %record;
	my @flds = @{ $self->{_head} };

	# Start the new record with NULLS
	for my $fld (@flds) {
		$record{$fld}	= $self->{_nulls}->{$fld}
	}

	# Now add DEFAULTS, if specified in SQL statement (WITH use_defaults: 1)
	if ($statement{WITH}->{use_defaults}) {
		for my $fld (@flds) {
			next unless defined($self->{_defaults}->{$fld});
			next if ($self->{_defaults}->{$fld} =~ /^=/ and $statement{WITH}->{no_calcs});
			my @defs		= split /\s*(?:,|\||-)\s*/ => $self->{_defaults}->{$fld};
			my $def_val		= "";
			if (@defs == 1 or @defs = grep { /^\*/ } @defs ) {
				$def_val	= $defs[0];
				$def_val	=~ s/^\*(.*)$/$1/s; # ** 2010-08-11
			}

			$record{$fld}	= (!$def_val && ~$def_val) ? 0 : ($def_val || $record{$fld});
		}
	}

	# Now add a RECORD, if any, from the SQL statement
	if (defined($statement{RECORD})) {
		my @REC;
		$statement{RECORD}	=~ s/(^"|"$)//g;
		if (ref($statement{RECORD})) {
			@REC = @{ $statement{RECORD} };
		} else {
			@REC = split $self->{_input_sep} => $statement{RECORD};
		}

		for my $col (0..$#flds) {
			if ( defined($REC[$col]) ) {
				$record{$flds[$col]} = (!$REC[$col] && ~$REC[$col]) ? '0' :
					$REC[$col] || $record{$flds[$col]}
			}
		}
	}

	# Now add VALUES, if any, from the SQL statement
	if (defined($statement{VALUES})) {
		my %VALS;
		if (ref($statement{VALUES})) {
			%VALS = %{ $statement{VALUES} };
		} else {
			%VALS = map {
				my ($key, $val) = split / *[:=]>? ?/;
				defined($val) or $val = '';
				$val = (!$val && ~$val) ? '0' : $val;
				($key, $val)
			} split / *[,;] */s => $statement{VALUES};
		}

		%record = ( %record, %VALS );
	}

	$statement{_import}{SELECT}		= $statement{IMPORT} || '*';

	$statement{_import}{WHERE}		= $statement{WHERE} || 'all';
	$statement{_import}{WHEN}		= $statement{WHEN}	|| sub { return 1 };
 	$statement{_import}{ORDER}		= $statement{ORDER}	|| '';
 	$statement{_import}{PROC}		= $statement{PROC}	|| '';

	$statement{_import}{DEFAULT} 	= { %record };
	$statement{_import}{FIELDS}		= [ @flds ];

	# ALIAS [otherDBfld_A]:[thisDBfld_X], [otherDBfld_B]=[thisDBfld_Y], [otherDBfld_C]=>[thisDBfld_Z]
	# ALIAS firstname:first, street => address, zip = zipcode
	my %fld_aliases					= map { ($_ => $_) } @flds;

	if ( $statement{ALIAS} ) {
		for ( split /\s*,\s*/ => $statement{ALIAS} ) {
			my ($there_name,$here_name)	= split / *[:=]>? ?/;
			if ($there_name && $here_name) {
				$fld_aliases{$there_name} = $here_name;
			}
		}
	}

	$statement{_import}{ALIAS}		= { %fld_aliases };

	bless { %{ $self }, %statement }, ref($self) || $self
}

# DELETE
sub prep_delete {
	my ($self, $sttmt) = @_;
	my %statement = %{ $sttmt };

	# FROM
	$statement{_from}	= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{FROM}) ? @{ $statement{FROM} } : split( /\s*,\s*/, ($statement{FROM} || 'me')) ];

	# WHERE				# fld(=|>|<|!|==|>>|<<|!!|~|!~|^|_|`|*|!*)val
#	$self->make_selector($statement{WHERE});
  	$statement{selector}	= $self->make_selector($statement{WHERE});

	bless { %{ $self }, %statement }, ref($self) || $self
}

# COUNT
sub prep_count {
	my ($self, $sttmt)	= @_;
	my %statement		= %{ $sttmt };

	# FROM	# If more than one, assumes original $self->struct
	$statement{_from}	= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{FROM}) ? @{ $statement{FROM} } : split( /\s*,\s*/, ($statement{FROM} || 'me')) ];

	# WHERE				# fld(=|>|<|!|==|>>|<<|!!|~|!~|^|_|`|*|!*)val
#	$self->make_selector($statement{WHERE});
  	$statement{selector}	= $self->make_selector($statement{WHERE});

	bless { %{ $self }, %statement }, ref($self) || $self
}

# BROWSE
sub prep_browse {
	my ($self, $sttmt)	= @_;
	my %statement		= %{ $sttmt };

	# BROWSE
	$statement{BROWSE}	||= '*';
	my @ids				=  $statement{BROWSE} =~ /\*/ ?
								('_ALL_') :
									split /\s*,\s*/, $statement{BROWSE};
	$statement{BROWSE}	= { ids => \@ids,  msg => ''};

	# FROM	# If more than one, assumes original $self->struct
	$statement{_from}	= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{FROM}) ? @{ $statement{FROM} } : split( /\s*,\s*/, ($statement{FROM} || 'me')) ];

	# WHERE		            # fld|?(=|>|<|!|==|>>|<<|!!|~|!~|^|_|`|*|!*)val
#	$self->make_selector($statement{WHERE});
  	$statement{selector}	= $self->make_selector($statement{WHERE});

	bless { %{ $self }, %statement }, ref($self) || $self
}


# MARK
#	sub prep_mark {
#	my ($self, $sttmt) = @_;
#	my %statement = %{ $sttmt };

#	}

# DESTROY
#	sub prep_destroy {
#	my ($self, $sttmt) = @_;
#	my %statement = %{ $sttmt };

#	}

# INDEX
sub prep_index {
	my ($self, $sttmt) = @_;
	my %statement = %{ $sttmt };
	my (@cols, @flds, $idxpat);

	# INDEX
	$statement{INDEX}	||= '#';
	@flds				=  $statement{INDEX} =~ /\#/ ?
								( ) :
								$statement{INDEX} =~ /\*/ ?
								( $self->{_id_fld} ) :
									split /\s*,\s*/, $statement{INDEX};

	# $self->id_fld is auto-included, so remove it from the fld list
	@flds					= grep { !/$self->id_fld/ } @flds;
	@cols					= @flds ? $self->columns(@flds) : ();
	@flds					= @cols ? $self->fields(@cols) : ();
	$idxpat					= join ' ', (@cols ? $self->pack_patterns(@flds,$self->id_fld) : $self->pack_patterns($self->id_fld)), "Z*", "Z*";

	$statement{INDEX}		= { cols => \@cols, flds => \@flds, id_fld => $self->id_fld, idxpat => $idxpat, msg => ''};


	# FROM	# If more than one, assumes original $self->struct
	$statement{_from}	= [ map {
		/^\s*me\s*$/		?	$self->{FILE} :
		-e && /^([^`]+)/	?	$1 :
								()
	} ref($statement{FROM}) ? @{ $statement{FROM} } : split( /\s*,\s*/, ($statement{FROM} || 'me')) ];

	# WHERE		            # fld|?(=|>|<|!|==|>>|<<|!!|~|!~|^|_|`|*|!*)val
  	$statement{selector}	= $self->make_selector($statement{WHERE});

	my $fld_str				= join( ' ' => @{ $statement{INDEX}->{flds}} ? @{ $statement{INDEX}->{flds}} : '' );
	my $index_date			= BVA::XUI::DATA::CALC::exec_date_time(); # localtime();
	my $fileName			= $self->DB_FILE;
	my $table_date			= BVA::XUI::DATETIME::tell_time('iso',(stat($fileName))[9]);
	
	my $wherePhrase			= $statement{selector}->{where};
	
	my $index_case	= '';
	if ($statement{WITH}->{idx_upcase}) {
		$statement{INDEX}->{case}	= $index_case	=  'upcase';
		push @{ $statement{_procs} } => \&BVA::XUI::DATA::PROC::upcase;
	} elsif ($statement{WITH}->{idx_lowcase}) {
		push @{ $statement{_procs} } => \&BVA::XUI::DATA::PROC::lowcase;
		$statement{INDEX}->{case}	= $index_case	=  'lowcase';
	} else {
		$statement{INDEX}->{case}	= '';
		$index_case	= 'nativecase';
	}
	
	$statement{INDEX}->{preamble} = <<"IDX";
BVA::XUI::DATA->get_idx(\\\*DATA,{
  	TABLE		=> \"$fileName\",
  	TABLEDATE	=> \"$table_date\",
  	WHERE		=> \"$wherePhrase\",
  	FIELDS		=> \[ qw/$fld_str/ \],
  	ID_FLD		=> \"$statement{INDEX}->{id_fld}\",
  	LOCATORS	=> \[ qw/indexNum byteNum/ \],
  	PATTERN		=> \"$statement{INDEX}->{idxpat}\",
  	CASE		=> \"$index_case\",
  	INDEXDATE	=> \"$index_date\",
  });

__DATA__
IDX

	bless { %{ $self }, %statement }, ref($self) || $self
}

# CREATE
sub prep_create {
	my ($self, $sttmt)	= @_;

	my %statement = %{ $sttmt };
	my ($out_file,@flds);

	if ($statement{CREATE} =~ m/^\s*(file|dbh?)?\s*([,:=]?>?)?\s*(.*)$/) {
		$statement{_create}->{type}	= $1 || 'db';
		$statement{_create}->{attr}	= $3 || $self->{FILE};
		($out_file,@flds)			= split /\s*,\s*/ => $statement{_create}->{attr};
	} else {
		$statement{_create}->{type}	= 'db';
		$out_file					= $self->{FILE};
	}

	if (-e $out_file and not $statement{WITH}->{file_override}) {
			my ($name,$suf,$nbr)	= ('', '', 00);
			($name,$suf)			= $out_file =~ /^(.*?)(\.\w{1,4})?$/;
			$suf					||= $self->{_db_suf};
			$nbr++					while (-e "$name.$nbr$suf");
			$out_file				= "$name.$nbr$suf";
	}
	$statement{_create}->{file}		= $out_file;

	if (!@flds) {
		@flds						= split /\s*,\s*/ => $statement{FIELDS} if $statement{FIELDS};
		@flds						= $self->fields() unless @flds;
	}
	@flds							= grep { $_ !~ /=x=/ } @flds;
	@flds							= map { $_ eq '*' ? $self->fields() : $_ } @flds;
	for my $i (0..$#flds) {
		next if $flds[$i] =~ /^\#/;
		my $col	= ( $flds[$i] =~ /^\*(.*)$/ ? $self->column($1) : $self->column($flds[$i]) );	
		if ($col) {
			unless ($statement{WITH}->{ignore_keys}) {		
				if ($self->{_struct}[4]->[$col+1] == 1) {
					$flds[$i] = "#$flds[$i]";
				}
			}
		}
		if (defined $statement{WITH}->{id_col} and $i == $statement{WITH}->{id_col}) {
			$flds[$i] = "#$flds[$i]";
		}
	}

	$statement{_create}->{flds}		= [ @flds, '=x=' ];
	
	$statement{_create}->{id_col}	= (grep{ $flds[$_] =~ /^\#/ } ('0 but true',1..$#flds))[0]
										|| $statement{WITH}->{id_col}
											|| $self->{_id_fld} && (grep{ $flds[$_] eq $self->{_id_fld} } ('0 but true',1..$#flds))[0]
												|| $self->{_id_col}
													|| '0 but true';
	$statement{_create}->{id_fld}	= $flds[$statement{_create}->{id_col}];
	$statement{_create}->{id_fld}	=~ s/\#?\*?(.*)$/$1/;

	bless { %{ $self }, %statement }, ref($self) || $self
}

# CREATE
sub do_create {
	my $self			= shift;
	my %args			= %{ $self->{WITH} };

	my $id_fld			= $self->{_create}->{id_fld};

	my $last_lock;

	if ($args{first_id}) {
		$args{first_id}		=~ s{^\s*(.*?)(continue)?\s*$}
								{
									$1
									. ($2 ? do {
										$last_lock	= $self->get_lock($self->{_file});
										my $last_id	= $last_lock->{LAST_ID};
										$last_id 	=~ s{^(.*?)([^.]+)$}{$2};
										$last_id
										} : "")
								}ex;
 		$self->{_defaults}->{$id_fld} = $args{first_id}
	}

	if ($args{formats}) {
		for (@{ $self->{_create}->{flds} }) {

		}
	}

	my $lock	= $self->get_lock($self->{_create}->{file}) or return;

	unless (sysopen(OUT, $lock->{FILE}, O_WRONLY | O_TRUNC | O_CREAT, oct(777))) {
		$self->{_err}->("log: Can't create database file $self->{_create}->{file}: $@, \n$!\n");
		return
	}

	$self->{_msg}->("log: New database fields:\n" . join( '|' => @{ $self->{_create}->{flds} }) );

	if ($args{use_formats}) {
# 		print OUT $self->db_struct(@{ $self->{_create}->{flds} }), $self->{_record_sep}, $self->{_record_sep};
		print OUT $self->db_struct(@{ $self->{_create}->{flds} }), $self->{_record_out_sep}, $self->{_record_out_sep};
	} else {
#  		print OUT $self->db_mod_struct(@{ $self->{_create}->{flds} }), $self->{_record_sep}, $self->{_record_sep};
 		print OUT $self->db_mod_struct(@{ $self->{_create}->{flds} }), $self->{_record_out_sep}, $self->{_record_out_sep};
	}

	$self->{_msg}->("log: OK New database file created $self->{_create}->{file}");

	close OUT;
	truncate $lock->{LOCK_FH}, 0;
	$lock->{RELEASE}->() or return;
	if ($last_lock) {
		$last_lock->{RELEASE}->() or return;
	}

	if ($self->{_create}->{type} eq 'dbh') {
		return BVA::XUI::DATA->init(input_sep => $self->{_input_sep},output_sep => $self->{_output_sep},record_sep => $self->{_record_out_sep})->connect(db => $lock->{FILE});
	} else {
		return $lock->{FILE};
	}
}

# UPDATE
sub do_update {
	my $self			= shift;

	my %args			= %{ $self->{WITH} };
	my %exec_params;

	if (ref($_[0]) eq 'HASH') {
		%exec_params	= %{ $_[0] };
	} elsif ($_[0]) {
		push @_ => '' if @_ % 2;
		%exec_params	= @_;
	}

	if ( $exec_params{WHERE} ) {
		$self->{selector} = $self->make_selector($exec_params{WHERE});
	}
	if ( $exec_params{SET} ) {
		$self->{SET} =  {( %{ $self->{SET} }, %{ $exec_params{SET} } )};
	}

	%args				= (%args, %exec_params);

	my $selector		= $self->{selector}->{test};

	my @flds			= @{ $self->{_head} };

	if ($args{insert_mute}) {
		$self->{_msg}->("mute: Insertion");
	}

	# File ops
	# only this file for now
	$self->{CURRENT}	= $self->get_lock($self->{_update}->[0]) or return;

	local $^I 			= $args{backup} ? ($args{backup}=~/^\./ ? '' : '.') . $args{backup} : '';
	local @ARGV			= ($self->{CURRENT}{FILE});
	local $/			= $self->{_record_sep};

	LINE: while ($self->{CURRENT}{LINE} = <>) {
		unless (
			$self->{selector}->{count} < $self->{LIMIT}->[0]
				and
			$. > $self->{_line0}
				and
			$self->{CURRENT}{LINE} !~ /^\s*$/
				and
			$self->{CURRENT}{LINE} !~ /$self->{_line_skip}/
				and
			$self->$selector($self->{CURRENT}{LINE})
		)
		{
			print $self->{CURRENT}{LINE};
			next LINE
		}

		chomp $self->{CURRENT}{LINE};

		# parse this record's data into expected fields
		@{ $self->{CURRENT}{ROW} }{@flds}	= split $self->{_input_sep} => $self->{CURRENT}{LINE};

		# Sanify field values with nulls, protecting zero values by testing with defined()
		# Trim leading and trailing whitespace
		for my $fld (@flds) {
			if (!defined($self->{CURRENT}{ROW}->{$fld})) {
				$self->{CURRENT}{ROW}->{$fld} = $self->{_nulls}->{$fld};
			}
			$self->{CURRENT}{ROW}->{$fld} =~ s/^\s*|\s*$//gs;
		}

		# Optional row processing before updating file
		for my $process ( @{ $self->{_procs} } ) { $self->$process() }

		my %rec		= (%{ $self->{CURRENT}{ROW} },  %{ $self->{SET} });

		## Calculations, if any (from input -- NOT from defaults)
		CALC: for my $col (0..$#flds) {
			next unless $rec{$flds[$col]};
			next unless $rec{$flds[$col]} =~ /^\s*=(.*)\s*$/s;
			my $calc	= $1;
			my $calc_sub;
			my $which_calc	= 0;
			if (exists $self->{_calcs}->{$calc} ) {
				$calc_sub	= $self->{_calcs}->{$calc};
			} elsif (defined( &{"BVA::XUI::DATA::CALC::$calc"} ) ) {
				$calc_sub	=  $self->{_calcs}->{$calc}	= \&{"BVA::XUI::DATA::CALC::$calc"};
			} else {
 				# $calc_sub	=  \&BVA::XUI::DATA::CALC::_default;
				next CALC;
			}
			$rec{$flds[$col]}	= $self->$calc_sub([@rec{@flds}], $col);
		}

		print join( $self->{_input_sep} => @rec{@flds} ), $self->{_record_sep};

	}

	$self->{_msg}->("log: " . join( ' ' =>
			qq/Update: $self->{selector}->{count} record/ . ($self->{selector}->{count} == 1 ? '' : 's') . qq/ updated/,
			qq/out of $self->{selector}->{tried} tr/ . ($self->{selector}->{tried} == 1 ? 'y' : 'ies'),
			qq/WHERE: $self->{selector}->{phrase}/));

	$self->{CURRENT}{RELEASE}->();

	$self->{selector}->{count};
}

# INSERT
sub do_insert {
	my $self			= shift;
	my %args			= %{ $self->{WITH} };

	if ($args{insert_mute}) {
		$self->{_msg}->("mute: Insertion");
	}

	my %exec_params;

	if (ref($_[0]) eq 'HASH') {
		%exec_params	= %{ $_[0] };
	} elsif ($_[0]) {
		push @_ => '' if @_ % 2;
		%exec_params	= @_;
	}

	# Now add VALUES, if any, from the execute call
	if ( $exec_params{VALUES} ) {
		my %VALS;
		if (ref($exec_params{VALUES}) eq 'HASH') {
			%VALS = %{ $exec_params{VALUES} };
		} else {
			%VALS = map {
				my ($key, $val) = split / *[:=]>? ?/;
				defined($val) or $val = '';
				$val = (!$val && ~$val) ? '0' : $val;
				($key, $val)
			} split / *[,;] */s => $exec_params{VALUES};
		}
		my @flds	= @{ $self->{_head} };
		my %record	= map { ($flds[$_] => $self->{INSERT}->[$_]) } (0..$#flds);
		%record = ( %record, %VALS );
		$self->{INSERT} = [ map { /^\s*(.*?)\s*$/os; $1 } @record{ @flds } ];
	}

	%args					= (%args, %exec_params);

	# File ops
	# only this file for now
	$self->{CURRENT}	= $self->get_lock($self->{_into}->[0]) or return;

	my ($id_base,$incr)	= $self->{CURRENT}{LAST_ID} =~ m{^(.*?)([^.]+)$};

	if ($args{id_base}) {
		$id_base = $args{id_base}
	}
	$id_base			||= '';
	$id_base			&&= "$id_base.";

	++$incr;

	my @record			= @{ $self->{INSERT} };

	my $new_id			= "$id_base$incr";

	if ($args{id_override} || $self->{_id_override} ) {
		$new_id	= $record[ $self->{_id_col} ] ||= $new_id
	}

	if ($args{unique_id} || $self->{unique_id} ) {
		my $selector	= $self->make_selector(  "$self->{_id_fld}=$new_id" )->{test};
		if (open DB, "$self->{CURRENT}{FILE}") {
			local $/ 	= $self->{_record_sep};
			COUNT: while (my $LINE = <DB>) {
				last COUNT if $LINE =~ /^\Q$self->{_table_sep}\E/;
				next COUNT unless $. > $self->{_line0};
				next COUNT if $LINE =~ /$self->{_line_skip}/;
				next COUNT unless $self->$selector($LINE);
			}
			close DB;
			if ($self->{selector}->{count}) {
				$self->{_err}->("Proposed ID $new_id is not unique for insertion into this table.\n$!\n");
				$self->{CURRENT}{RELEASE}->();
				return
			}
		}
	}

	$record[ $self->{_id_col} ] = $new_id;

	## Use Previous Record's Values, if specified in SQL statement (WITH use_prev)
	## NOTE: A previous "true" value will persist against a newer "false" value
	if ($args{use_prev} and open DB, "$self->{CURRENT}{FILE}") {
		local $/ 	= $self->{_record_sep};
		my $LINE;
		LINE: while (<DB>) {
			next LINE unless (
				$. > $self->{_line0}
					and
				$_ !~ /^\s*$/
					and
				$_ !~ /$self->{_line_skip}/
			);
			$LINE = $_;
		}
		chomp $LINE;
		close DB;
		my @prev	=  map { /^\s*(.*?)\s*$/os; $1 . '' } split $self->{_input_sep} => $LINE;
		@record	= map { $record[$_] || $prev[$_] || '' } 0..$#record; # * See NOTE above.
	}

	## Calculations, if any (from defaults and ??)
	CALC: for my $col (0..$#record) {
		next unless $record[$col] =~ /^\s*=(.*)\s*$/s;
		my $calc	= $1;
		my $calc_sub;
		my $which_calc	= 0;
		if (exists $self->{_calcs}->{$calc} ) {
 			$calc_sub	= $self->{_calcs}->{$calc};
		} elsif (defined( &{"BVA::XUI::DATA::CALC::$calc"} ) ) {
			$calc_sub	=  $self->{_calcs}->{$calc}	= \&{"BVA::XUI::DATA::CALC::$calc"};
		} else {
 			# $calc_sub	=  \&BVA::XUI::DATA::CALC::_default;
 			next CALC;
		}
		$record[$col]	= $self->$calc_sub([@record], $col);
	}

	## Now open the data file, in append mode
	if (open DB, ">>$self->{CURRENT}{FILE}") {
		local *LOCK	= $self->{CURRENT}{LOCK_FH};

		## Print the new data record at the end of the data file
		local $" = $self->{_input_sep};
		print DB "@record", $self->{_record_sep};	## ** 2006-01-01

		## Close the data file
		close DB;

		## Overwrite the semaphore with the new ID
		seek (LOCK, 0, 0) or die "Can't seek lock. $!";
		print LOCK $new_id, "\n";
		truncate LOCK, tell LOCK;

	} else {
		$self->{_err}->("log: Can't access $self->{CURRENT}{FILE} for Insert: \n$!\n");
		$self->{CURRENT}{RELEASE}->();
		return
	}

	$self->{_msg}->("log: Insert: Record Inserted with ID $new_id");

	# Now release the lock
	$self->{CURRENT}{RELEASE}->();

	$self->{_msg}->("unmute");

	return $new_id;
}

sub do_import {
	my $self			= shift;
	my %args			= %{ $self->{WITH} };

	if ($args{insert_mute}) {
		$self->{_msg}->("mute: Insertion");
	}

	my %default_record	= %{  $self->{_import}{DEFAULT} };
	my @import_flds		= @{  $self->{_import}{FIELDS} };
	my $condition		= \&{ $self->{_import}{WHEN} };
	my %aliases			= %{  $self->{_import}{ALIAS} };
	$self->{_import}{COUNT} = 0;
	my @src_sths;

	my $input_sep	= $args{input_sep} || $self->{_input_sep};
	my $record_sep	= $args{record_sep} || $self->{_record_sep};

	FROM: for my $src (@{ $self->{_from} }) {
 		my $import_dbh		= BVA::XUI::DATA->init(input_sep => $input_sep, record_sep => $record_sep)->connect(db=>$src)
								or next FROM;
		my $importer		= $import_dbh->prepare(qq{SELECT $self->{_import}{SELECT} WHERE $self->{_import}{WHERE}  ORDER $self->{_import}{ORDER} PROC $self->{_import}{PROC};}, %{ $self->{_procedures} })
								or next FROM;
		$importer->execute();

		push @src_sths => $importer;
	}

	# File ops
	# Import to only this file for now
	$self->{CURRENT}	= $self->get_lock($self->{_into}->[0]) or return;

	my $last_id			= $self->max_id() || $self->{CURRENT}{LAST_ID} || 'XX00000';

	my ($id_base,$incr)	= $last_id =~ m{^(.*?)([^.]+)$};

	if ($args{id_base}) {
		$id_base = $args{id_base}
	}
	$id_base			||= '';
	$id_base			&&= "$id_base.";

	my $id_col			= exists $args{id_col} ? $args{id_col} : $self->{_id_col};
	my $latest_id		= '';

	IMPORT: {
		# Now open the data file, in append mode
		my $DB;
		if (open $DB, ">>", "$self->{CURRENT}{FILE}") {
			local *LOCK	= $self->{CURRENT}{LOCK_FH};
		} else {
			$self->{_err}->("log: Can't access $self->{CURRENT}{FILE} for Import: \n$!\n");
			$self->{CURRENT}{RELEASE}->();
			return
		}

		for my $src (@src_sths) {
			my @flds	= @{ $src->cursor('hdr') };
			while (my $s = $src->fetchrow_hashref) {

				my %import_data;
				for my $K (@flds) {
					next unless ($s->{$K} || ~$s->{$K});
					$import_data{ $K } 				= $s->{$K} || $default_record{$K};
					next unless $aliases{$K};
					$import_data{ $aliases{$K} } 	= $s->{$K} || $default_record{$K};
				}

				# Make the new record
				%import_data	= ( %default_record, %import_data );
				my @record		= map { $_ && /^\s*(.*?)\s*$/os; $1 } @import_data{ @import_flds };

				# Set the record ID
				++$incr;
				my $new_id			= "$id_base$incr";
				if ($args{id_override} || $self->{_id_override} ) {
					$new_id	= $record[ $id_col ] ||= $new_id;
				}
				$record[ $id_col ] = $new_id;

				## Calculations, if any (from defaults and ??)
				unless ($args{no_calcs}) {
					CALC: for my $col (0..$#record) {
						next unless $record[$col] =~ /^\s*=(.*)\s*$/s;
						my $calc	= $1;
						my $calc_sub;
						my $which_calc	= 0;
						if (exists $self->{_calcs}->{$calc} ) {
							$calc_sub	= $self->{_calcs}->{$calc};
						} elsif (defined( &{"BVA::XUI::DATA::CALC::$calc"} ) ) {
							$calc_sub	=  $self->{_calcs}->{$calc}	= \&{"BVA::XUI::DATA::CALC::$calc"};
						} elsif ( $self->column($calc) ) {
							$calc_sub	= sub { $_[1]->[$self->column($calc)] };
						} else {
							# $calc_sub	=  \&BVA::XUI::DATA::CALC::_default;
							next CALC;
						}
						$record[$col]	= $self->$calc_sub([@record], $col);
					}
				}

				## Print the new data record at the end of the data file
				{
					local $" = $self->{_input_sep};
					print $DB "@record", $self->{_record_sep}
						and $self->{_import}{COUNT}++;
				}

				## Overwrite the semaphore with the new ID
				seek (LOCK, 0, 0) or die "Can't seek lock. $!";
				print LOCK $new_id, "\n";
				truncate LOCK, tell LOCK;
			}
		}

		## Close the data file
		close $DB;
	}

	# Now release the lock
	$self->{CURRENT}{RELEASE}->();

	$self->{_msg}->("log: Records Imported: $self->{_import}{COUNT}");

	$self->{_msg}->("unmute");

	return BVA::XUI::DATA->init(input_sep => $self->{_input_sep})->connect(db => $self->{CURRENT}{FILE});
}

# DELETE
sub do_delete {
	my $self			= shift;
	my $selector		= $self->{selector}->{test} || return 0;
	my %args			= %{ $self->{WITH} };

	# only this file for now
	$self->{CURRENT}	= $self->get_lock($self->{_from}->[0]) or return;

	{
		local $^I 			= $args{backup} ? '.' . $args{backup} : '';
		local @ARGV			= ($self->{CURRENT}{FILE});
		local $/			= $self->{_record_sep};

		LINE: while (my $LINE = <>) {
			unless (
				$self->{selector}->{count} < $self->{LIMIT}->[0]
					and
				$. > $self->{_line0}
					and
				$LINE !~ /^\s*$/
					and
				$LINE !~ /$self->{_line_skip}/
					and
				$self->$selector($LINE)
			)
			{
				print $LINE;
			}
		}
	}

	$self->{_msg}->("log: " . join( ' ' =>
			qq/Delete: $self->{selector}->{count} record/ . ($self->{selector}->{count} == 1 ? '' : 's') . qq/ deleted/,
			qq/out of $self->{selector}->{tried} tr/ . ($self->{selector}->{tried} == 1 ? 'y' : 'ies'),
			qq/WHERE: $self->{selector}->{phrase}/));

	$self->{CURRENT}{RELEASE}->();

	$self->{selector}->{count};
}

# COUNT
sub do_count {
	my $self					= shift;
	my $selector				= $self->{selector}->{test} || return 0;
	my %args					= %{ $self->{WITH} };

	# File ops
	for my $src (@{ $self->{_from} }) {
  		open my $in, "<", $src or next;
  		$self->{CURRENT}{FH}	= $in; # {CURRENT}{FH} can be used by procs

		local $/	= $self->{_record_sep};

		COUNT: while (my $LINE = <$in>) {
			last COUNT if $LINE =~ /^\Q$self->{_table_sep}\E/;
			next COUNT unless $. > $self->{_line0};
			next COUNT if $LINE =~ /$self->{_line_skip}/;
			next COUNT unless $self->$selector($LINE);
			for my $process ( @{ $self->{_procs} } ) { $self->$process() }
		}
		close $in;
	}

	$self->{_msg}->("log: " . join( ' ' =>
			qq/Count: $self->{selector}->{count} record/ . ($self->{selector}->{count} == 1 ? '' : 's') . qq/ found/,
			qq/out of $self->{selector}->{tried} tr/ . ($self->{selector}->{tried} == 1 ? 'y' : 'ies'),
			qq/WHERE: $self->{selector}->{phrase}/));

	return $self->{selector}->{count};
}

# BROWSE
sub do_browse {
	my $self					= shift;
	my @flds					= @{ $self->{_head} };
	my $selector				= $self->{selector}->{test};
	$self->{MARK}				||= 0; # $self->{_line0_byte};
	my @index					= ();

	BROWSE: for my $src (@{ $self->{_from} }) {
		$self->{CURRENT}	 	= $self->get_lock($src) or next BROWSE;

		if ($self->{BROWSE}{ids}->[0] ne '_ALL_') {
			$self->{CURRENT}{_table_idx}	= ($self->{CURRENT}{FILE} =~ /^(.*?)\.(\w{1,4})$/)[0] . $self->{_idx_suf};
		}

		## Now open the table file
  		open IN, $self->{CURRENT}{FILE} or next BROWSE;

		$self->{CURRENT}{FH}	= \*IN;

  		local $/				= $self->{_record_sep};

  		## Get to first record or marked record
		if ($self->{MARK}) {
			seek IN, $self->{MARK}, 0;
		} else {
			HDR: while (<IN>) {
				last HDR if $. >= $self->{_line0};
			}
		}

		LINE: while ($self->{CURRENT}{LINE} = <IN>) {

			last LINE if $self->{CURRENT}{LINE} =~ /^\Q$self->{_table_sep}\E/;

			next LINE if $self->{CURRENT}{LINE} =~ /$self->{_line_skip}/;

			for my $break ( @{ $self->{BREAK} } ) {
				if ($self->$break($self->{CURRENT}{LINE})) {
					$self->{break_table}++;
					last LINE
				}
			}

			next LINE unless $self->$selector($self->{CURRENT}{LINE});

			# parse this record's data into expected fields
			@{ $self->{CURRENT}{ROW} }{@flds}	= split /(?:$self->{_input_sep}|$self->{_record_sep})/ => $self->{CURRENT}{LINE};

			# Sanify field values with nulls, protecting zero values by testing with defined()
			# Trim leading and trailing whitespace
			for my $fld (@flds) {
				if (!defined($self->{CURRENT}{ROW}->{$fld})) {
					$self->{CURRENT}{ROW}->{$fld} = $self->{_nulls}->{$fld};
				}
				$self->{CURRENT}{ROW}->{$fld} =~ s/^\s*|\s*$//gs;
			}

 			# optional line processing while browsing; does not change data in file
			for my $process ( @{ $self->{_procs} } ) { next unless $process; $self->$process() }

			# stop reading all files if limit is reached
			if ($self->{selector}->{count} >= $self->{LIMIT}->[0]) {
				close $self->{CURRENT}{FH};
				$self->{CURRENT}{RELEASE}->();
				last BROWSE;
			}

		}

		close $self->{CURRENT}{FH};
		$self->{CURRENT}{RELEASE}->();

		# stop reading all tables if $self->{break_table}
		last BROWSE if $self->{break_table};
	}

	$self->{BROWSE}->{msg}		= join( ' ' =>
			qq/Browse: $self->{selector}->{count} record/ . ($self->{selector}->{count} == 1 ? '' : 's') . qq/ browsed/,
			qq/WHERE: $self->{selector}->{phrase}/);

	$self->{_msg}->("log: $self->{BROWSE}->{msg}");

	return bless $self, ref($self) || $self;
}

# INDEX
sub do_index {
	my $self						= shift;
	my %args						= %{ $self->{WITH} };
	
	my @flds						= @{ $self->{_head} };
	my $selector					= $self->{selector}->{test};
	$self->{_select}->{start_byte}	= $self->{MARK} ||= $self->{_line0_byte};
	my @index_files						= ();

	INDEX: for my $src (@{ $self->{_from} }) {
		$self->{CURRENT}	 			= $self->get_lock($src) or next INDEX;

		my $index_name					= join '-' => '_index', grep { $_ && !/$self->{_id_fld}/ } sort( @{ $self->{INDEX}->{flds} } ), $self->{INDEX}->{case};
		$self->{CURRENT}{_table_idx}	= ($self->{CURRENT}{FILE} =~ /^(.*?)\.(\w{1,4})$/)[0] . $index_name . $self->{_idx_suf};

		## Open a new index file, deleting previous
		$self->{NEW}	 		= $self->get_lock($self->{CURRENT}{_table_idx}) or next INDEX;
		my $idx_fh;
		open $idx_fh, ">", $self->{NEW}{FILE}
			and push @index_files => $self->{CURRENT}{_table_idx}
				or do {$self->{_err}->("log: Index can't access index $self->{CURRENT}{_table_idx}: $@, \n$!\n"); return};

		$self->{NEW}{FH}		= $idx_fh;

		print $idx_fh $self->{INDEX}->{preamble};

		## Now open the table file
  		open IN, "<", $self->{CURRENT}{FILE} or next INDEX;

		$self->{CURRENT}{FH}	= \*IN;

  		## Get to first record or marked record
		if ($self->{MARK}) {
			seek $self->{CURRENT}{FH}, $self->{MARK}, 0;
		} else {
 			HDR: while (<IN>) {
				last HDR if $. >= $self->{_line0};
			}
		}

		LINE: while ($self->{CURRENT}{LINE} = readline($self->{CURRENT}{FH})) {

			last LINE if $self->{CURRENT}{LINE} =~ /^\Q$self->{_table_sep}\E/;

			next LINE if $self->{CURRENT}{LINE} =~ /$self->{_line_skip}/;

			for my $break ( @{ $self->{BREAK} } ) {
				if ($self->$break($self->{CURRENT}{LINE})) {
					$self->{break_table}++;
					last LINE
				}
			}

			next LINE unless $self->$selector($self->{CURRENT}{LINE});

			# parse this record's data into expected fields
			@{ $self->{CURRENT}{ROW} }{@flds}	= split $self->{_input_sep} => $self->{CURRENT}{LINE};

			# Sanify field values with nulls, protecting zero values by testing with defined()
			# Trim leading and trailing whitespace
			for my $fld (@flds) {
				if (!defined($self->{CURRENT}{ROW}->{$fld})) {
# 					$self->{CURRENT}{ROW}->{$fld} = $self->{_nulls}->{$fld};
					$self->{CURRENT}{ROW}->{$fld} = '-' x $self->{_sizes}->{$fld};
				}
				$self->{CURRENT}{ROW}->{$fld} =~ s/^\s*|\s*$//gs;
			}

 			# optional line processing while indexing; does not change data in file
			for my $process ( @{ $self->{_procs} } ) { $self->$process() }

			# create an index entry, with field values, running count, and byte number
	  		$self->{CURRENT}{ENTRY}	= pack $self->{INDEX}->{idxpat},
				@{ $self->{CURRENT}{ROW} }{ @{ $self->{INDEX}->{flds} }, $self->{INDEX}->{id_fld} },
				$self->{selector}->{count},
				$self->{_select}->{start_byte};

			print $idx_fh $self->{CURRENT}{ENTRY}, "\n";

			# stop reading all files if limit is reached
			if ($self->{selector}->{count} >= $self->{LIMIT}->[0]) {
				close $self->{CURRENT}{FH};
				$self->{CURRENT}{RELEASE}->();
				print $idx_fh "\n";
				close $idx_fh;
				$self->{NEW}{RELEASE}->();
				last INDEX;
			}
		} continue {
			$self->{_select}->{start_byte} = tell $self->{CURRENT}{FH};
		}
		close $self->{CURRENT}{FH};
		$self->{CURRENT}{RELEASE}->();
		print $idx_fh "\n";
		close $idx_fh;
		$self->{NEW}{RELEASE}->();

		$self->{_select}->{start_byte}	= $self->{MARK};

		# stop reading all tables if $self->{break_table}
		last INDEX if $self->{break_table};
	}

	$self->{INDEX}->{msg}		= join( ' ' =>
			qq/Index: $self->{selector}->{count} record/ . ($self->{selector}->{count} == 1 ? '' : 's') . qq/ indexed/,
			qq/WHERE: $self->{selector}->{phrase}/);

	$self->{_msg}->("log: $self->{INDEX}->{msg}");

	$self->{INDEX}->{index_files}	= \@index_files;
	
	$self->get_indexes();

	return bless $self, ref($self) || $self;
}

# SELECT
sub do_select {
	my $self				= $_[0];
	my %args				= %{ $self->{WITH} };

	if (ref($_[1]) eq 'HASH') {
		my %exec_params			= %{ $_[1] };
		if ($exec_params{MARKS} ) {
			$self->{selector} = $self->make_selector('all')
		}
		if ( $exec_params{WHERE} ) {
			$self->{selector} = $self->make_selector($exec_params{WHERE})
		}
		if ( $exec_params{ORDER} ) {
			$exec_params{ORDER}	= $exec_params{ORDER} =~ /unsorted/i ? '' : $exec_params{ORDER};
			my $sort_nums	= [ map { $self->{_hd_nums}->{$_} || () } ( split /\s*,\s*/, $exec_params{ORDER} ) ];
			my $sort_flds	= [ @{ $self->{_head} }[@{ $sort_nums }] ];
			my $sortpack	= join ' ', (map {$self->{_pack_pats}->{$_} } @{ $sort_flds }), 'N';
			my $idxpack		= join ' ', $sortpack, 'N';
			$self->{ORDER}	= $exec_params{ORDER}	= { cols => $sort_nums, flds => $sort_flds, sortpack => $sortpack };
			$self->{_select}->{idxpack} = $idxpack;
		}
		%args					= (%args, %exec_params) ;
	}

	# TO  *Last Minute* 	# [table|file|db|cursor: path/filename|...]
	$self->{TO}				= $args{TO} || $self->{TO};

	# BYTE MARKS - used mostly with byte marks obtained from an index
	$self->{BYTE_MARKS}		= $args{MARKS} ? $args{MARKS} : $args{MARK} ? [$args{MARK}] : [];
	if (@{ $self->{BYTE_MARKS} } > 0) { 
		$self->{MARKED_ONLY}++; 
		$self->{MARK}			= shift @{ $self->{BYTE_MARKS} };
	} else {
 		$self->{MARK}			= $self->{_line0_byte};
 	}

	## Data variables
	require IO::File;
	my $tbl_fh	= IO::File->new_tmpfile
		or return;

	my @sort_list;

	SELECT: {

		my $selector			= $self->{selector}->{test};

		TABLE: for my $src (@{ $self->{_from} }) {
			$self->{CURRENT}		= $self->get_lock($src) or next TABLE;

			## Now open the table file
			open IN, "<", $self->{CURRENT}{FILE} or next TABLE;

			$self->{CURRENT}{FH}	= \*IN;
			$self->{break_table}	= 0;

			local $/				= $self->{_record_sep};

			my @table_sort_list;

			## Get to first record or marked record
			if ($self->{MARK}) {
				seek $self->{CURRENT}{FH}, $self->{MARK}, 0;
			} else {
				HDR: while (<IN>) {
					last HDR if $. >= $self->{_line0};
				}
			}

			LINE: while ($self->{CURRENT}{LINE} = <IN>) {

				if ($self->{CURRENT}{LINE} =~ /^\Q$self->{_table_sep}\E/) {
					$self->{break_table}++;
					last LINE;
				}

				# stop now if any defined break procedure returns true
				# usually only useful with sorted records in source file
				for my $break ( @{ $self->{BREAK} } ) {
					if ($self->$break($self->{CURRENT}{LINE})) {
						$self->{break_table}++;
						last LINE
					}
				}

				# skip line if any defined next procedure returns true
				# usually only useful with sorted records in source file
				# But PROC unique [fld] works on unsorted records
				for my $next ( @{ $self->{NEXT} } ) {
					if ($self->$next($self->{CURRENT}{LINE})) {
						next LINE
					}
				}

				next LINE unless $self->$selector($self->{CURRENT}{LINE});

				chomp $self->{CURRENT}{LINE};

				# parse this record's data into expected fields
				@{ $self->{CURRENT}{ROW} }{@{ $self->{_head} }}	= split $self->{_input_sep} => $self->{CURRENT}{LINE};

				# Sanify field values with nulls, protecting zero values by testing with defined()
				# Trim leading and trailing whitespace
				for my $fld (@{ $self->{_head} }) {
					if (!defined($self->{CURRENT}{ROW}->{$fld})) {
						$self->{CURRENT}{ROW}->{$fld} = $self->{_nulls}->{$fld};
					}
					$self->{CURRENT}{ROW}->{$fld} =~ s/^\s*|\s*$//gs;
				}

				## Meta-data
				$self->{CURRENT}{ID}	= $self->{CURRENT}{ROW}->{ $self->{_id_fld} };

				# Optional row processing; does not change data in source file
				for my $process ( @{ $self->{_procs} } ) { $self->$process() }

				# Store a sort key, with count number added for stable sorts
				# and with row byte number attached for lookups in the temp db.
				# If no ORDER is specified, the sort key is the count number (with the row byte number attached).
				push @table_sort_list, pack $self->{_select}->{idxpack},
										map { lc( $_ ) }
											@{ $self->{CURRENT}{ROW} }{ @{ $self->{ORDER}->{flds} } },
											$self->{selector}->{count}-1,
											tell $tbl_fh; #

				# Now print the desired columns of the record to the temp db #***
				print $tbl_fh pack( $self->{_select}->{recordpack}, @{ $self->{CURRENT}{ROW} }{ @{ $self->{_select}->{flds} } }), $self->{_record_sep};

				# Stop reading table if max is reached
				last LINE if $self->{selector}->{count} >= $self->{LIMIT}->[0];

				# Go to next table if table_first
				last LINE if $args{table_first};

				if (@{ $self->{BYTE_MARKS} }) {
					seek $self->{CURRENT}{FH}, shift @{ $self->{BYTE_MARKS} }, 0;
				} else {
					last LINE if $self->{MARKED_ONLY};
				}

			}

			close $self->{CURRENT}{FH};
			$self->{CURRENT}{RELEASE}->();

			# Keep only the last record from the table if table_last
			if ($args{table_last}) {
				@table_sort_list	= $table_sort_list[-1];
			}

			push @sort_list => @table_sort_list;

			# stop reading all files if max is reached
			last TABLE if $self->{selector}->{count} >= $self->{LIMIT}->[0];

			# stop reading all tables if $self->{break_table}
			last TABLE if $self->{break_table};
		}
	}

	if (@{ $self->{ORDER}->{cols} }) {
		@sort_list = sort @sort_list;
	}

	my $stmt			= $self->statement;
	my $where			= $self->{selector}->{where};
	my $phrase			= $self->{selector}->{phrase};
	my $num				= $self->{selector}->{count};
	my @get_flds		= @{ $self->{_select}->{flds} };
	my $colpat			= $self->{_select}->{colpack};
	my $recordpat		= $self->{_select}->{recordpack};
	my $fld_sep			= $self->{_output_sep};
	my $tmptable_sep	= $self->{_select}->{_input_sep};
	my $rec_sep			= $self->{_record_sep};
	my $rec_out_sep		= $self->{_record_out_sep};
	my $idx_sep			= $self->{_idx_sep};
	my $crsr_pos		= 0;


	if ($args{indexasfile}) {
		my $temp_idx	= "$self->{FILE_DIR}${$}." . time() . "$self->{_idx_suf}";
		my $idx_fh;

		open($idx_fh, "+>$temp_idx")
			and unlink $temp_idx
				or return;
		{
			local $\	= $self->{_idx_sep};
			for (@sort_list) {
				my $byte	= (unpack $self->{_select}->{idxpack}, $_)[-1];
				print $idx_fh $byte
			}
		}
		seek $idx_fh, 0, 0;

		$self->{iterator}	= sub {
			my $flag = $_[0] || 'array';

			$flag eq 'num'			and return $num;
			$flag eq '#'			and return $num - $crsr_pos;
			$flag eq 'hdr'			and return [ @get_flds ];
			$flag eq 'hdr_hash'		and return { map { $_ => 1 } @get_flds };
			$flag eq 'hdr_str'		and return join ' ' => @get_flds;
			$flag eq 'hdr_list_str'	and return join ',' => @get_flds;
			$flag eq 'hdr_pat_str'	and return sprintf $colpat => @get_flds;
			$flag eq 'pat'			and return $colpat;
			$flag eq 'rec_pat'		and return $recordpat;
			$flag eq 'msg'			and return $phrase;
			$flag eq 'sel'			and return $phrase;
			$flag eq 'where'		and return $where;
			$flag eq 'stmt'			and return $stmt;
			$flag eq 'pos'			and return ($crsr_pos < $num ? $crsr_pos : '--' );
			$flag eq 'reset'		and do { $crsr_pos = 0; return $num };
			return if $crsr_pos == $num;

			my $byte;
			{
				local $/	= $idx_sep;
				$byte	= <$idx_fh>;
				return unless $byte;
				chomp $byte;
				$crsr_pos++;
			}

			my $row;
			seek $tbl_fh, $byte, 0;
			{
				local $/		= $rec_sep;
				$row			= <$tbl_fh>;
				return unless $row;
				chomp $row;
			}
			my @rec			= unpack $recordpat, $row;

			$flag eq 'array'	and return wantarray ? @rec : $rec[0];
			$flag eq 'arrayref'	and return [ @rec ];
			$flag eq 'hash'		and return map { $get_flds[$_] => $rec[$_] } (0..$#get_flds);
			$flag eq 'hashref'	and return { map { $get_flds[$_] => $rec[$_] } (0..$#get_flds) };
# 			$flag eq 'row'		and return join( $fld_sep, @rec ) . $rec_sep;
			$flag eq 'row'		and return join( $fld_sep, @rec ) . $rec_out_sep;
# 			$flag eq 'str'		and return sprintf $colpat, map { s{\n}{\\}g; $_ } @rec; # Fudging \n within data for sprintf
			$flag eq 'str'		and return sprintf $colpat, map { my $r = $_; $r =~ s{\n}{\\}g; $r } @rec; # Fudging \n within data for sprintf

			return
		};

	} else {
		my @index;

		for (@sort_list) {
			my $i		= (unpack $self->{_select}->{idxpack}, $_)[-1];
			push @index	=> $i;
		}

		$self->{iterator}	= sub {
			my $flag = $_[0] || 'array';

			$flag eq 'num'			and return $num;
			$flag eq '#'			and return $num - $crsr_pos;
			$flag eq 'hdr'			and return [ @get_flds ];
			$flag eq 'hdr_hash'		and return { map { $_ => 1 } @get_flds };
			$flag eq 'hdr_str'		and return join ' ' => @get_flds;
			$flag eq 'hdr_list_str'	and return join ',' => @get_flds;
			$flag eq 'hdr_pat_str'	and return sprintf $colpat => @get_flds;
			$flag eq 'pat'			and return $colpat;
			$flag eq 'rec_pat'		and return $recordpat;
			$flag eq 'msg'			and return $phrase;
			$flag eq 'sel'			and return $phrase;
			$flag eq 'where'		and return $where;
			$flag eq 'stmt'			and return $stmt;
			$flag eq 'pos'			and return ($crsr_pos < $num ? $crsr_pos : '--' );
			$flag eq 'reset'		and do { $crsr_pos = 0; return $num };
			return if $crsr_pos == $num;

			my $byte		= $index[$crsr_pos++];

			my $row;
			seek $tbl_fh, $byte, 0;
			{
				local $/		= $rec_sep;
				$row			= <$tbl_fh>;
				return unless $row;
				chomp $row;
			}
			my @rec			= unpack $recordpat, $row;

			$flag eq 'array'	and return wantarray ? @rec : $rec[0];
			$flag eq 'arrayref'	and return [ @rec ];
			$flag eq 'hash'		and return map { $get_flds[$_] => $rec[$_] } (0..$#get_flds);
			$flag eq 'hashref'	and return { map { $get_flds[$_] => $rec[$_] } (0..$#get_flds) };
# 			$flag eq 'row'		and return join( $fld_sep, @rec ) . $rec_sep;
			$flag eq 'row'		and return join( $fld_sep, @rec ) . $rec_out_sep;
			$flag eq 'str'		and return sprintf $colpat, map { my $r = $_; $r =~ s{\n}{\\}g; $r } @rec; # Fudging \n within data for sprintf

			return
		};
	}

	my $where_msg	= join( ' ' =>
		qq/Select Summary: $self->{selector}->{count} record/ . ($self->{selector}->{count} == 1 ? '' : 's') . qq/ selected/,
		qq/out of $self->{selector}->{tried} tr/ . ($self->{selector}->{tried} == 1 ? 'y' : 'ies'),
		qq/and $self->{selector}->{skipped} skipped/,
		qq/WHERE: $self->{selector}->{phrase}/);
	$where_msg		.= qq/ ORDER: / . join(', ' => (@{ $self->{ORDER}->{cols}} ? @{ $self->{ORDER}->{flds}} : 'source order') );
	$self->{selector}->{report}	= $where_msg;
	$self->{_msg}->("log: $where_msg");

	if ($self->{TO}->[0]) {
		return $self->to(\%args);
	}

	return bless $self, ref($self) || $self;

	1;
}

# handoff for "TO" clauses in SELECT statement
# deprecated after addition of fetchrow and fetchall methods
sub to {
	my $self	= shift;
	my %args	= %{ shift() };

	$self->{TO}	= $args{TO} || $self->{TO};

	if ($self->{TO}->[0] eq 'file') {
		open ( OUT, ">$self->{TO}->[1]" )
			or die qq/$0: Select failed on write to file $self->{TO}->[1]: \n$!/;#
		select((select(OUT), $|=1)[0]);

# 		$args{commnt} and print OUT qq/# SOURCE: $self->{FILE}$self->{_record_sep}# / . $self->{selector}->{report} . $self->{_record_sep};
		$args{commnt} and print OUT qq/# SOURCE: $self->{FILE}$self->{_record_out_sep}# / . $self->{selector}->{report} . $self->{_record_out_sep};

		my @prelim;
		$args{return_head}		and push @prelim, $self->iterator('hdr');
		$args{return_titles}	and push @prelim, [ $self->labels( @{ $self->iterator('hdr') } ) ];

		if ($args{use_cols}) {
			foreach (@prelim) {
# 				print OUT sprintf $self->iterator('pat') . $self->{_record_sep}, @$_
				print OUT sprintf $self->iterator('pat') . $self->{_record_out_sep}, @$_
			}
			while ( my $str = $self->iterator('str') ) {
# 				print OUT $str, $self->{_record_sep}
				print OUT $str, $self->{_record_out_sep}
			}
		} else {
			foreach (@prelim) {
# 				print OUT join( $self->{_output_sep} => @$_ ), $self->{_record_sep}
				print OUT join( $self->{_output_sep} => @$_ ), $self->{_record_out_sep}
			}
			while ( my $row = $self->iterator('row') ) {
				print OUT $row
			}
		}

		close OUT;

		return $self->{TO}->[1];

	} elsif ($self->{TO}->[0] =~ /^db/) {
		return unless $self->iterator('#');
		my $lock	= $self->get_lock($self->{TO}->[1]) or return;

		if ($args{append}) { # CAREFUL!! ONLY DO THIS WITH SELECT * (all flds)
			open ( OUT, ">>$lock->{FILE}" )
				or die qq/$0: Select failed on append to db $self->{TO}->[1]: \n$!/;
		} else {
			if ($args{use_struct}) {
				$self->{TO}->[3] = '';
				open ( STR, "$args{use_struct}" )
					or die qq/$0: Select failed on opening struct file: $args{use_struct}\n$!/;
				local $/	= $self->{_record_sep};
				while (<STR>) {
					next if /^\s*$/;
					last if /\#-\#-\#/;
					$self->{TO}->[3] .= $_
				}
				close STR;
# 				$self->{TO}->[3]	.= "$self->{_record_sep}";
				$self->{TO}->[3]	.= "$self->{_record_out_sep}";
			} else {
# 				$self->{TO}->[3]	= $self->db_struct(@{ $self->iterator('hdr') }) . "$self->{_record_sep}$self->{_record_sep}"
				$self->{TO}->[3]	= $self->db_struct(@{ $self->iterator('hdr') }) . "$self->{_record_out_sep}$self->{_record_out_sep}"
			}

			open ( OUT, ">$lock->{FILE}" )
				or die qq/$0: Select failed on write to db $self->{TO}->[1]: \n$!/;

			print OUT $self->{TO}->[3];
		}

		select((select(OUT), $|=1)[0]);
		while (my $row = $self->iterator('row')) {
			print OUT $row
		}
		close OUT;
		truncate $lock->{LOCK_FH}, 0;
		$lock->{RELEASE}->();

		if ($self->{TO}->[0] eq 'dbh') {
			return BVA::XUI::DATA->init(input_sep => "\t")->connect(db => $self->{TO}->[1]);
		} elsif ($self->{TO}->[0] eq 'dbx') {
			return BVA::XUI::DATA->init(input_sep => "\t")->connect(db => $self->{TO}->[1])->save_connection;
		} else {
			return $self->{TO}->[1];
		}

	} elsif ($self->{TO}->[0] eq 'table') {
		if ($self->{TO}->[1] eq 'array') {
			return $self->fetchall_arrayref;
		} elsif ($self->{TO}->[1] eq 'hash') {
			return $self->fetchall_hashref;
		} elsif ($self->{TO}->[1] eq 'text') {
			if ($args{use_cols}) {
				return $self->fetchall_string(\%args);
			} else {
				return $self->fetchall_record(\%args);
 			}
		}

	} elsif ($self->{TO}->[0] eq 'cursor') {
		my $main = $self->{TO}->[1] || 'hashref';
	 	return sub { $self->{iterator}->($_[0] || $main) }
	}

	1;
}

# Fetching data
# 	'array'
# 	'arrayref'
# 	'hash'
# 	'hashref'
# 	'row'
# 	'str'

# FETCHROW variations
# --------
# List fld names as params to make slices

# fetchrow (same as fetchrow_arrayref)
sub fetchrow {
	goto &fetchrow_arrayref;
}

sub fetchrow_arrayref {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('arrayref') unless @_;

	my @flds	= grep { $self->columns($_) } @_;

	my $d		= $self->iterator('hashref');

	\@{ [ map { exists $d->{$_} ? $d->{$_} : '' } @flds ] };
#	[ map { exists $d->{$_} ? $d->{$_} : '' } @flds ];
}

sub fetchrow_array {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('array') unless @_;

	my $d		= $self->iterator('hashref');

	map { exists $d->{$_} ? $d->{$_} : '' } grep { $self->columns($_) } @_;
}

sub fetchrow_hashref {
	my $self	= shift;

	return unless $self->has_iterator;

	my $d		= $self->iterator('hashref');

	return $d unless @_;

	\%{ { map { $_ => $d->{$_} } grep { $self->columns($_) } @_ } };
}

sub fetchrow_hash {
	my $self	= shift;

	return unless $self->has_iterator;

	my %d		= $self->iterator('hash');

	return %d unless @_;

	map { $_ => $d{$_} } grep { $self->columns($_) } @_;
}

sub fetchrow_record {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('row') unless @_;

	my $d		= $self->iterator('hashref');
	
	return unless $d;

	my @flds	= grep { $self->columns($_) } @_;

# 	join( $self->{_input_sep} => map { exists $d->{$_} ? $d->{$_} : '' } @flds ) . $self->{_record_sep};
	join( $self->{_input_sep} => map { exists $d->{$_} ? $d->{$_} : '' } @flds ) . $self->{_record_out_sep};

}

sub fetchrow_string {
	my $self	= shift;

	return unless $self->has_iterator;

	return $self->iterator('str') unless @_;

	my $d		= $self->iterator('hashref');

	return unless $d;

	my @flds	= grep { $self->columns($_) } @_;

	my $colpat	= join ' ', $self->col_formats(@flds);

# 	sprintf $colpat, map { s{\n}{\\}g; $_ } map { exists $d->{$_} ? $d->{$_} : '' } @flds;
	sprintf $colpat, map { my $f = $_; $f =~ s{\n}{\\}g; $f } map { exists $d->{$_} ? $d->{$_} : '' } @flds;
}

# FETCHALL variations
# --------
# List fld names as params to make slices
# First arg may be a hashref of switches
# Switches so far: return_head, return_titles,

# fetchall_arrayref returns a ref to an array of arrayrefs,
# in order from SELECT result set
sub fetchall_arrayref {
	my $self	= shift;

	return unless $self->has_iterator;

	my $args	= shift;
	my %args;
	if (ref($args) =~ /HASH/) {
		%args = %{ $args }
	} elsif ($args) {
		unshift @_, $args;
	}

	my @table;
	$self->iterator('reset');

	if (@_) {
		my @flds	= grep { $self->columns($_) } @_;
		$args{return_head}		and push @table, [ @flds ];
		$args{return_titles}	and push @table, [ $self->labels(@flds) ];

		while (my $href = $self->iterator('hashref')) {
			push @table, \@{ [ map { exists $href->{$_} ? $href->{$_} : '' } @flds ] };
		}
	} else {
		$args{return_head}		and push @table, $self->iterator('hdr');
		$args{return_titles}	and push @table, [ $self->labels( @{ $self->iterator('hdr') } ) ];

		while (my $aref = $self->iterator('arrayref')) {
			push @table, $aref
		}
	}
	return \@table
}

# fetchall_array returns an array of arrayrefs,
# in order from SELECT result set
sub fetchall_array {
	my $self	= shift;

	return unless $self->has_iterator;

	my $args	= shift;
	my %args;
	if (ref($args) =~ /HASH/) {
		%args = %{ $args }
	} elsif ($args) {
		unshift @_, $args;
	}
	my @table;
	$self->iterator('reset');

	if (@_) {
		my @flds	= grep { $self->columns($_) } @_;
		$args{return_head}		and push @table, [ @flds ];
		$args{return_titles}	and push @table, [ $self->labels(@flds) ];

		while (my $href = $self->iterator('hashref')) {
			push @table, \@{ [ map { exists $href->{$_} ? $href->{$_} : '' } @flds ] };
		}
	} else {
		$args{return_head}		and push @table, $self->iterator('hdr');
		$args{return_titles}	and push @table, [ $self->labels( @{ $self->iterator('hdr') } ) ];

		while (my $aref = $self->iterator('arrayref')) {
			push @table, $aref
		}
	}
	return @table
}


# fetchall_hashref returns a ref to a hash of hashrefs
# outer hash is keyed on the record's ID fld/primary key
# if available from the SELECT iterator;
# otherwise the hash is keyed on the first column of SELECT statement
# (watch out if first col doesn't have unique values!!)
sub fetchall_hashref {
	my $self	= shift;

	return unless $self->has_iterator;

	my %table;
	$self->iterator('reset');

	if (@_) {
		while (my $href = $self->iterator('hashref')) {
			my $key 		= $href->{$self->{_id_fld}} || $href->{$self->iterator('hdr')->[0]}; # or something
			$table{$key}	= \%{ { map { $_ => $href->{$_} } grep { $self->columns($_) } @_ } }
		}
	} else {
		while (my $href = $self->iterator('hashref')) {
			my $key 		= $href->{$self->{_id_fld}} || $href->{$self->iterator('hdr')->[0]}; # or something
			$table{$key}	= $href;
		}
	}
	return \%table
}

# fetchall_keyed_hashref returns a ref to a hash of hashrefs;
# the outer hash is keyed on first optional arg; if no first arg,
# or the first arg isn't a field recognized by the iterator,
# the outer hash is keyed on the record's ID fld/primary key
# if available from the SELECT iterator;
# otherwise the hash is keyed on the first column of SELECT statement
# (watch out if first col doesn't have unique values!!)
sub fetchall_keyed_hashref {
	my $self	= shift;

	return unless $self->has_iterator;

	my %table;
	$self->iterator('reset');

	my $hdr_lookup	= $self->iterator('hdr_hash');

	if (@_ and $hdr_lookup->{$_[0]}) {
		my $key 			= shift;
		if (@_) {
			while (my $href = $self->iterator('hashref')) {
				$table{$href->{$key}}	= \%{ { map { $_ => $href->{$_} } grep { $self->columns($_) } $key, @_ } }
			}
		} else {
			while (my $href = $self->iterator('hashref')) {
				$table{$href->{$key}}	= $href;
			}
		}
	} else {
		while (my $href = $self->iterator('hashref')) {
			my $key 		= $href->{$self->{_id_fld}} || $href->{$self->iterator('hdr')->[0]};
			$table{$key}	= $href;
		}
	}
	return \%table
}

# fetchall_hasharrayref returns a ref to an array of hashrefs,
# in order from SELECT result set
sub fetchall_hasharrayref {
	my $self	= shift;

	return unless $self->has_iterator;

	my @table;
	$self->iterator('reset');

	if (@_) {
		while (my $href = $self->iterator('hashref')) {
			push @table	=> \%{ { map { $_ => $href->{$_} } grep { $self->columns($_) } @_ } }
		}
	} else {
		while (my $href = $self->iterator('hashref')) {
			push @table	=> $href;
		}
	}
	return \@table
}


# fetchall_record returns a table with lines for each result set record
# lines are in columns separated by the separator of the database (input_sep)
sub fetchall_record {
	my $self	= shift;

	return unless $self->has_iterator;

	my $args	= shift;
	my %args;
	if (ref($args) =~ /HASH/) {
		%args = %{ $args }
	} elsif ($args) {
		unshift @_, $args;
	}
	my @table;
	$self->iterator('reset');

	if (@_) {
		my @flds	= grep { $self->columns($_) } @_;

# 		$args{return_head}		and push @table, join( $self->{_input_sep} => @flds ) . $self->{_record_sep};
		$args{return_head}		and push @table, join( $self->{_input_sep} => @flds ) . $self->{_record_out_sep};
# 		$args{return_titles}	and push @table, join( $self->{_input_sep} => $self->labels(@flds) ) . $self->{_record_sep};
		$args{return_titles}	and push @table, join( $self->{_input_sep} => $self->labels(@flds) ) . $self->{_record_out_sep};

		while (my $href = $self->iterator('hashref')) {
# 			push @table, join( $self->{_input_sep} => map { exists $href->{$_} ? $href->{$_} : '' } @flds ) . $self->{_record_sep};
			push @table, join( $self->{_input_sep} => map { exists $href->{$_} ? $href->{$_} : '' } @flds ) . $self->{_record_out_sep};
		}
	} else {
# 		$args{return_head}		and push @table, join( $self->{_input_sep} => @{ $self->iterator('hdr') } ) . $self->{_record_sep};
		$args{return_head}		and push @table, join( $self->{_input_sep} => @{ $self->iterator('hdr') } ) . $self->{_record_out_sep};
# 		$args{return_titles}	and push @table, join( $self->{_input_sep} => $self->labels( @{ $self->iterator('hdr') } )) . $self->{_record_sep};
		$args{return_titles}	and push @table, join( $self->{_input_sep} => $self->labels( @{ $self->iterator('hdr') } )) . $self->{_record_out_sep};

		while (my $row = $self->iterator('row')) {
			push @table, $row;
		}
	}
	return join '' => "\n", @table, "\n";
}

# fetchall_string returns a table with lines for each result set record
# lines are in columns defined by metadata type & size for each column
sub fetchall_string {
	my $self	= shift;

	return unless $self->has_iterator;

	my $args	= shift;
	my %args;
	if (ref($args) =~ /HASH/) {
		%args = %{ $args }
	} elsif ($args) {
		unshift @_, $args;
	}
	my @table;
	$self->iterator('reset');

	if (@_) {
		my @flds	= grep { $self->columns($_) } @_;
		my $colpat	= join(' ', $self->col_formats(@flds));

		$args{return_titles}	and push @table, sprintf $colpat => $self->labels(@flds);
		$args{return_head}		and push @table, sprintf $colpat => @flds;

		while (my $href = $self->iterator('hashref')) {
			push @table, sprintf($colpat => @{ [ map { exists $href->{$_} ? $href->{$_} : '' } @flds ] } );
		}
	} else {
		$args{return_titles}	and push @table, sprintf($self->iterator('pat') => $self->labels( @{ $self->iterator('hdr') } ));
		$args{return_head}		and push @table, $self->iterator('hdr_str');

		while (my $str = $self->iterator('str')) {
			push @table, $str;
		}
	}
# 	return join $self->{_record_sep} => "", @table, "";
	return join $self->{_record_out_sep} => "", @table, "";
}

# Selector
#   'test' => sub { "DUMMY" },
#   'count' => 131,
#   'skipped' => 0,
#   'where' => 'appeal @~ bbash',
#   'tried' => 205,
#   'report' => 'Select Summary: 131 records selected out of 205 tries and 0 skipped WHERE: Event or Appeal has an item exactly matching \'bbash\' ORDER: contrib_lastname, log_id',
#   'COUNT' => 131,
#   'phrase' => 'Event or Appeal has an item exactly matching \'bbash\''

sub selector {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector};
}

sub selector_test {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector}->{'test'};
}

sub selector_count {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector}->{'count'} || "0E0";
}

sub selector_skipped {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector}->{'skipped'} || "0E0";
}

sub selector_tried {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector}->{'tried'} || "0E0";
}

sub selector_where {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector}->{'where'} || '';
}

sub selector_report {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector}->{'report'} || 'Data not selected yet.';
}

sub selector_phrase {
	my $self	= shift;

	return unless $self->{selector};

	$self->{selector}->{'phrase'} || '';
}




## Check file locking on semaphore file, creating it if necessary
sub check_lock {
	my $self	= shift;

	my $args	= shift;

	my %lock	= ref($args) ? %{ $args } : ( _TRYFILE => $args || '' );

	$lock{_TRYFILE} ||= $self->{_file} || '';

	unless ($lock{_TRYFILE}) {
		$self->{_err}->("log: No database specified for locking: $@, \n");
		return
	}

	unless ($lock{_TRYFILE} =~ m/^([_\w.\/\\: -]+)$/) {
		$self->{_err}->("log: $0: Bad characters in filename.\n");
		return
	}
	$lock{FILE}	= $1;

	@lock{qw/FILE_DIR FILE_NAME FILE_SUF/}	= $lock{FILE} =~ /^(.*?\/)?([^\/]*?)(\.\w{1,4})?$/;

	unless ("$lock{FILE}$self->{_sem_suf}" =~ m/^([_\w.\/\\: -]+)$/) {
		$self->{_err}->("log: $0: Bad characters in semaphore filename.\n");
		return
	}
	$lock{ID_FILE}	= $1;

	## Try to open the semaphore file
	unless (sysopen(LOCK, $lock{ID_FILE}, O_RDWR | O_CREAT, oct(777))) {
		$self->{_err}->("log: Can't open semaphore $lock{ID_FILE}: $@, \n$!\n");
		return
	}

	## Try to lock the semaphore file
	unless ($lock{LOCK_OK}		= eval { flock(LOCK, LOCK_EX) and 'OK' } || '') {
		close LOCK;
		$self->{_err}->("log: Can't lock semaphore $lock{ID_FILE}: $@, \n$!\n");
		return
	}

	## Get the ID of the last record added, if any, from the first line of the semaphore.
	seek LOCK, 0, 0;
	$lock{LAST_ID} = <LOCK>;
	$lock{LAST_ID} ||= ''; chomp $lock{LAST_ID};

	$lock{LOCK_FH}	= \*LOCK;

	$lock{RELEASE}	= sub {
		close delete $lock{LOCK_FH}
			and
		$self->{_msg}->("log: OK Released lock on $lock{FILE}");
	};

	$self->{_msg}->("log: OK Confirmed lock on $lock{FILE}");

	\%lock;  # qw{ FILE FILE_DIR FILE_NAME FILE_SUF ID_FILE LOCK_FH LOCK_OK LAST_ID RELEASE }
}


sub get_lock {
	my $self	= shift;

	my $args	= shift;

	my %lock	= ref($args) ? %{ $args } : ( _TRYFILE => $args || '' );

	$lock{_TRYFILE} ||= $self->{_file} || '';

	unless ($lock{_TRYFILE}) {
		$self->{_err}->("log: No database specified for locking: $@, \n");
		return
	}

	unless ($lock{_TRYFILE} =~ m/^([_\w.\/\\: -]+)$/) {
		$self->{_err}->("log: $0: Bad characters in filename.\n");
		return
	}
	$lock{FILE}	= $1;

	@lock{qw/FILE_DIR FILE_NAME FILE_SUF/}	= $lock{FILE} =~ /^(.*?\/)?([^\/]*?)(\.\w{1,4})?$/;

	unless ("$lock{FILE}$self->{_sem_suf}" =~ m/^([_\w.\/\\: -]+)$/) {
		$self->{_err}->("log: $0: Bad characters in semaphore filename.\n");
		return
	}
	$lock{ID_FILE}	= $1;

	## Try to open the semaphore file
	unless (sysopen(LOCK, $lock{ID_FILE}, O_RDWR | O_CREAT, oct(777))) {
		$self->{_err}->("log: Can't open semaphore $lock{ID_FILE}: $@, \n$!\n");
		return
	}

	## Try to lock the semaphore file
	unless ($lock{LOCK_OK}		= eval { flock(LOCK, LOCK_EX) and 'OK' } || '') {
		close LOCK;
		$self->{_err}->("log: Can't lock semaphore $lock{ID_FILE}: $@, \n$!\n");
		return
	}

	## Establish the ID of the last record added, if any, or the starting ID.
	## Try the first line of the semaphore.
	## If it's blank, check the database file and then the default ID
	## If the database file doesn't exist, politely create it.
	$lock{INIT_ID}	= $self->default($self->{_id_fld}) || 0;
	seek LOCK, 0, 0;
	$lock{LAST_ID} = <LOCK>;
	$lock{LAST_ID} ||= ''; chomp $lock{LAST_ID};
	unless ($lock{LAST_ID}) {
		unless (sysopen( DB, $lock{FILE}, O_RDWR | O_CREAT, oct(777))) {
			close LOCK;
			$self->{_err}->("log: Can't open database file $lock{FILE}: $@, \n$!\n");
			return
		}

		my $last_line	= '';
		{
			local $/	= $self->{_record_sep};
			while (<DB>) {
				next unless $. > $self->{_line0};
				next if (/^\s*$/ or /$self->{_line_skip}/);
				last if /^\Q$self->{_table_sep}\E/;
				$last_line	= $_;
			}
			close DB;
		}
		chomp $last_line;
		$lock{LAST_ID}	= (split $self->{_input_sep} => $last_line)[$self->{_id_col}];
		$lock{LAST_ID}	||=	$lock{INIT_ID} || '0';
		seek LOCK, 0, 0;
		print LOCK $lock{LAST_ID}, "\n";
		truncate LOCK, tell LOCK; # Obliterate anything else in the semaphore
		seek LOCK, 0, 0;

	}
	$lock{LAST_ID} ||=	'0';
	$lock{LOCK_FH}	= \*LOCK;

	## Are we journaling? If so, make sure we're in synch.
	if ($lock{journal} ||= $self->{WITH}->{journal} || '') {
		# Last ID must be at the starting ID ...
		if ($lock{journal} =~ /new/i) {
			if ($lock{LAST_ID} ne $lock{INIT_ID} ) {
				close LOCK;
				$self->{_err}->("log: Journal new declined at ID $lock{LAST_ID} for table $lock{FILE}");
				return
			}
		# Or the requested journal ID must be a string or pattern exactly matching the last ID
		} elsif ($lock{LAST_ID} !~ /^$lock{journal}$/) {
			close LOCK;
			$self->{_err}->("log: Unexpected last ID $lock{LAST_ID} for table $lock{FILE}");
			return
		}
	}

	$self->{_msg}->("log: OK Obtained lock on $lock{FILE}");

	$lock{RELEASE}	= sub {
		close delete $lock{LOCK_FH}
			and
		$self->{_msg}->("log: OK Released lock on $lock{FILE}");
	};

	\%lock;  # qw{ FILE FILE_DIR FILE_NAME FILE_SUF ID_FILE LOCK_FH LOCK_OK LAST_ID INIT_ID RELEASE }
}

sub statement {
	my $self		 = shift;
	my $pat = " %-6.6s    %s\n";
	my $sel_info = "";

	my $action	= $self->{ACTION} ||= 'NOOP';
	$sel_info .= sprintf $pat, 'ACTION',
		join(', ', $action);

	if ($action eq 'select') {
		$sel_info .= sprintf $pat, 'SELECT',
			join(', ', @{ $self->{_select}->{flds} });
		$sel_info .= sprintf $pat, 'FROM',
			join(', ', @{ $self->{_from} } );
		$sel_info .= sprintf $pat, 'WHERE',
			 $self->{selector}->{where};
		$sel_info .= sprintf $pat, 'ORDER',
			join(', ', @{ $self->{ORDER}->{flds} }) || 'unsorted' ;
		$sel_info .= sprintf $pat, 'TO',
			ref($self->{TO}) ? $self->{TO}->[1] ? join(':' => @{ $self->{TO} }[0,1]) : 'cursor' : ($self->{TO} || 'cursor');

	} elsif ($action eq 'browse') {
		$sel_info .= sprintf $pat, 'BROWSE',
			join(', ', @{ $self->{BROWSE}->{ids} });
		$sel_info .= sprintf $pat, 'FROM',
			join(', ', @{ $self->{_from} } );
	} elsif ($action eq 'index') {
		$sel_info .= sprintf $pat, 'INDEX',
			ref($self->{INDEX}) ? join(', ', @{ $self->{INDEX}->{flds} }) : $self->{INDEX};
		$sel_info .= sprintf $pat, 'FROM',
			join(', ', @{ $self->{_from} } );
		$sel_info .= sprintf $pat, 'WHERE',
			$self->{selector}->{where};
		$sel_info .= sprintf $pat, 'ORDER',
			ref($self->{ORDER}) ? join(', ', @{ $self->{ORDER}->{flds} }) : $self->{ORDER} || 'unsorted' ;
		$sel_info .= sprintf $pat, 'TO',
			$self->{TO}->[0];

	} elsif ($action eq 'update') {
		$sel_info .= sprintf $pat, 'UPDATE',
			join( ', ', @{ $self->{_update} });
		$sel_info .= sprintf $pat, 'WHERE',
			$self->{selector}->{where};
		$sel_info .= sprintf $pat, 'SET',
			join(', ', map { "$_: $self->{SET}->{$_}" } keys %{ $self->{SET} });

	} elsif ($action eq 'insert') {
		$sel_info .= sprintf $pat, 'INSERT',
			join( ', ', @{ $self->{INSERT} || [] });
		$sel_info .= sprintf $pat, 'VALUES',
			$self->{VALUES};
		$sel_info .= sprintf $pat, 'INTO',
			join( ', ', @{ $self->{_into} });

	} elsif ($action eq 'delete') {
		$sel_info .= sprintf $pat, 'DELETE','';
		$sel_info .= sprintf $pat, 'WHERE',
			$self->{selector}->{where};
		$sel_info .= sprintf $pat, 'FROM',
			join(', ', @{ $self->{_from} } || [] );

	} elsif ($action eq 'count') {
		$sel_info .= sprintf $pat, 'COUNT','';
		$sel_info .= sprintf $pat, 'WHERE',
			$self->{selector}->{where};
		$sel_info .= sprintf $pat, 'FROM',
			join(', ', @{ $self->{_from} } || [] );

	} elsif ($action eq 'create') {
		$sel_info .= sprintf $pat, 'CREATE',
			"$self->{_create}->{type}: $self->{_create}->{attr}";

	}

	$sel_info .= sprintf $pat, 'WITH',
		(keys %{ $self->{WITH} }?
		join(', ', map { $_ . ": " . $self->{WITH}->{$_} }
			keys %{ $self->{WITH} }) : '');

	$sel_info .= sprintf $pat, 'PROC',
		$self->{PROC} || '';

	$sel_info .= "";
	return $sel_info
}

sub reveal {
	goto &struct

}

sub struct {
	my $self = $_[0];
	my $file = $self->{FILE};

	my $db_info		= sprintf "Connect:\n %-16s%s\n", "Filename:", $file;
	my $fh = open (IN, $file)
		and $db_info .= sprintf " %-16s%s\n", "Connection:", "OK"
			and close(IN)
				or $db_info .= sprintf "%-16s%s\n", "Connection:", "Failed: $!"
					and return $db_info;

	$db_info .= sprintf " %-16s%s\n", "File Locking:", $self->{LOCK_OK} || '';

	$db_info .= sprintf " %-16s%s kB\n", "File size:", $self->{_fsize_note};

	$db_info .= sprintf " %-16s%s (%s)\n", "Records:", $self->{_num_recs}, $self->{_num_recs_note};

	$db_info .= ' ---' . "\n\n";

	my @head = @{ $self->{_head} };
	my $num_flds = @head;
	$db_info .= sprintf "%-16s%s field%s, as follows:\n", "Structure:",
		$num_flds, $num_flds == 1 ? "" : "s";
	my ($fld_max, $lbl_max, $i) = (5,5,0);
	for (@head) {
		$fld_max = length($_) if $fld_max < length($_);
		$lbl_max = length($self->{_labels}->{$_}) if
			$lbl_max < length($self->{_labels}->{$_});
	}

	$db_info .= sprintf "%4s  %-${fld_max}s  %-${lbl_max}s  %4s  %6s  %4s    %s\n",
		qw/Col Field Label Type Size Key Default/;
	$db_info .= sprintf "%4s  %-${fld_max}s  %-${lbl_max}s  %4s  %6s  %4s    %4s\n",
		qw/--- ----- ----- ---- ---- --- -------/;
	for (@head) {
		$db_info .= sprintf "%4d  %-${fld_max}s  %-${lbl_max}s  %4s  %6d  %4s    %s\n",
		$i++, $_, 	$self->{_labels}->{$_},	$self->{_types}->{$_},	$self->{_sizes}->{$_},
					$self->{_keys}->{$_},	$self->{_defaults}->{$_};
	}

	$db_info .= <<"PARMS";
 ---

 line_zero       => $self->{_line0}
 line_zero_byte  => $self->{_line0_byte}
 id_col          => $self->{_id_col}
 id_fld          => $self->{_id_fld}
 id_pack_pat     => $self->{_pack_pats}->{$self->{_id_fld}}
 id_pack         => $self->{_id_pack}

PARMS

	$db_info .= ' ---' . "\n\n";

	return $db_info
}

## File subs

sub db_base_name {
	my $self	= shift;
	$self->{FILE_NAME} || undef;
}

sub db_file_name {
	my $self	= shift;
	$self->{FILE} || undef;
}

sub num_recs {
	my $self	= shift;
	$self->{_num_recs} || undef;
}

sub get_indexes {
	my $self	= shift;

	my $location		= shift || $self->{FILE_DIR};
	
	my @current_indexes	= glob "${location}$self->{FILE_NAME}_index*$self->{_idx_suf}";

 	my %indexes;

	for my $idx (@current_indexes) {
		my ($idx_name,$idx_case,$idx_flds);
		if ($idx =~ /^.*_index$self->{_idx_suf}$/) {
			$idx_flds	= [ $self->{_id_fld} ];
			$idx_case	= 'ncase';
			$idx_name	= 'base';
		} elsif ($idx =~ /^.*_index-((?:low|up)case)$self->{_idx_suf}$/) {
			$idx_flds	= [ $self->{_id_fld} ];
			$idx_case	= $1;
			$idx_name	= "base-$idx_case";
		} elsif ($idx =~ /^.*_index-(.+?)(?:|-((?:low|up)case))?$self->{_idx_suf}$/) {
			$idx_flds	= [ split '-' => $1 ];
			$idx_case	= $2 || 'ncase';
			$idx_name	= join '-' => grep { $_ ne 'ncase' } $1, $idx_case;
		} else {
			next;
		}
		$indexes{$idx_name}->{case} = $idx_case;
		$indexes{$idx_name}->{path}	= $idx;
		$indexes{$idx_name}->{flds}	= $idx_flds;
		$indexes{$idx_name}->{name}	= $idx_name;
	}

	$self->{_indexes}	= \%indexes;
	return \%indexes;
}

sub indexes {
	my $self	= shift;
	
	return $self->{_indexes} || $self->get_indexes();
}

## Header and other struct subroutines

sub head { @{ $_[0]->{_head} } }

sub fields {
	my $self = shift;
	@_ ? map { $self->{_head}->[$_] || () } @_ : @{ $self->{_head} }
}

sub matched_fields {
	my $self = shift;
	my $fldpat	= shift;
	map { /$fldpat/ ? $_ : () } @{ $self->{_head} }
}

sub field {
	my $self	= shift;
	$self->{_head}->[ $_[0] ] || ''
}

sub columns {
	my $self = shift;
	map { $self->{_hd_nums}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
}

sub column {
	my $self	= shift;
	$self->{_hd_nums}->{ $_[0] } || ''
}

sub id_col {
	my $self	= shift;
	$self->{_id_col};
}

sub id_fld {
	my $self	= shift;
	$self->{_id_fld};
}

sub types {
	my $self = shift;
	map { $self->{_types}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
}

sub type {
	my $self = shift;
	$self->{_types}->{ $_[0] } || ''
}

sub sizes {
	my $self = shift;
	map { $self->{_sizes}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
}

sub size {
	my $self = shift;
	$self->{_sizes}->{ $_[0] } || ''
}

sub labels {
	my $self = shift;
	map { $self->{_labels}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
}

sub label {
	my $self = shift;
	$self->{_labels}->{ $_[0] } || ''
}

sub defaults {
	my $self = shift;
	map { $self->{_defaults}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
}

sub default {
	my $self = shift;
	$self->{_defaults}->{ $_[0] } || ''
}

sub pack_patterns {
	my $self = shift;
# 	map { $self->{_pack_pats}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
 	map { $self->{_rec_pats}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
#	map { 'Z*' } ( @_ ? @_ : @{ $self->{_head} } )
}

sub pack_pattern {
	my $self = shift;
	$self->{_pack_pats}->{ $_[0] } || ''
}

sub col_formats {
	my $self = shift;
	map { $self->{_col_pats}->{$_} || () } ( @_ ? @_ : @{ $self->{_head} } )
}

sub col_format {
	my $self = shift;
	$self->{_col_pats}->{ $_[0] } || ''
}

sub db_struct {
	my $self = shift;
	my %hd_nums	= %{ $self->{_hd_nums} };
	my @flds;

	if (@_) {
		for (@_) {
			my ($fld,$col,$id_mark,@add);
			$fld	= $_;
			if ($fld eq '*') {
				push @add => @{ $self->{_head} };
			} elsif ($fld =~ /^(\#?)\*(.+)$/) {
				$id_mark	= $1 ? 1 : 0;
				$fld		= $2;
				$col		= $hd_nums{$fld}	= scalar keys %hd_nums;

				$self->{_struct}[0]->[$col+1] = $fld;
				$self->{_struct}[1]->[$col+1] = join(' ' => map { ucfirst($_) } split / |_/ => $fld);
				$self->{_struct}[2]->[$col+1] = "$self->{_def_type}:$self->{_def_size}";
				$self->{_struct}[3]->[$col+1] = $self->{_defaults}{$fld} || '';
				$self->{_struct}[4]->[$col+1] = $id_mark;

				push @add => $fld;
			} elsif ($fld =~ /^(\#?)(.+)$/) {
				$id_mark	= $1 ? 1 : 0;
				$fld		= $2;
				$col		= $hd_nums{$fld};
				if (exists $hd_nums{$fld}) {
					$self->{_struct}[4]->[$col+1] = $id_mark;
					push @add => $fld;
				}
			}
			push @flds => @add;
		}
	} else {
		@flds	= $self->{_select} ?
					@{ $self->{_select}->{flds} } :
						@{ $self->{_head} }
	}

	my @cols = map { $hd_nums{$_} || () } @flds;

# 	join $self->{_record_sep} => map {
	join $self->{_record_out_sep} => map {
		my @elements	= @{$_};
		my $line_label	= shift @elements;
		$line_label . ':' . join($self->{_input_sep} => @elements[@cols]);
	} @{$self->{_struct}};
}

sub db_mod_struct {
	my $self = shift;
	my %hd_nums	= %{ $self->{_hd_nums} };
	my @flds;

	if (@_) {
		for (@_) {
			my ($fld,$fmt,$type,$size,$def,$col,$id_mark,@add);
			$fld	= $_;
			if ($fld eq '*') {
				push @add => grep { !/=x=/ } @{ $self->{_head} };
			} elsif ($fld =~ /^(\#?)\*(.+?)(?:\:([^:]+?))?(?:\:([^:]+?))?(?:\:(.*?))?$/) {
				$id_mark	= $1 ? 1 : 0;
				$fld		= $2;
				$type		= $3 || $self->{_types}->{$fld} || $self->{_def_type};
				$size		= $4 || $self->{_sizes}->{$fld} || $self->{_def_size};
				$def		= $5 || (defined($5) && !$5 && ~$5 ? 0 : '');
				$fmt		= "$type:$size";
				$col		= $hd_nums{$fld}	= scalar keys %hd_nums;

				$self->{_struct}[0]->[$col+1] = $fld;
				$self->{_struct}[1]->[$col+1] = join(' ' => map { ucfirst($_) } split / |_/ => $fld);
				$self->{_struct}[2]->[$col+1] = $fmt;
				$self->{_struct}[3]->[$col+1] = $def;
				$self->{_struct}[4]->[$col+1] = $id_mark;

				push @add => $fld;
			} elsif ($fld =~ /^(\#?)([^*].+?)(?:\:([^:]+?))?(?:\:([^:]+?))?(?:\:(.*?))?$/) {
				$id_mark	= $1 ? 1 : 0;
				$fld		= $2;
				$type		= $3 || $self->{_types}->{$fld} || $self->{_def_type};
				$size		= $4 || $self->{_sizes}->{$fld} || $self->{_def_size};
				$def		= $5 || (defined($5) && !$5 && ~$5 ? 0 : '');
				$fmt		= "$type:$size";
				$col		= $hd_nums{$fld};
				if (exists $hd_nums{$fld}) {
					$self->{_struct}[4]->[$col+1] = $id_mark;
					$self->{_struct}[2]->[$col+1] = $fmt ? $fmt : $self->{_struct}[2]->[$col+1];
					$self->{_struct}[3]->[$col+1] = $def;
					push @add => $fld;
				}
			}
			push @flds => @add;
		}
	} else {
		@flds	= $self->{_select} ?
					@{ $self->{_select}->{flds} } :
						@{ $self->{_head} }
	}

	my @cols = map { $hd_nums{$_} || () } @flds;

# 	join $self->{_record_sep} => map {
	join $self->{_record_out_sep} => map {
		my @elements	= @{$_};
		my $line_label	= shift @elements;
		$line_label . ':' . join($self->{_input_sep} => @elements[@cols])
	} @{$self->{_struct}};
}

sub db_thin_struct {
	my $self	= shift;
	my %hd_nums	= %{ $self->{_hd_nums} };
	my @flds;
	if (@_) {
		@flds	= map {
						my $fld	= $_;
						if ($fld =~ /^\*(.+)$/) {
							$fld	= $1;
							push @{$self->{_struct}[0]} => $fld;
							push @{$self->{_struct}[1]} => join(' ' => map { ucfirst($_) } split / |_/ => $fld);
							push @{$self->{_struct}[2]} => 't:24';
							$hd_nums{$fld}				= scalar keys %hd_nums;
						}
						$fld } @_
	} else {
		@flds	= $self->{_select} ?
					@{ $self->{_select}->{flds} } :
						@{ $self->{_head} }
	}

	my @cols = map { $hd_nums{$_} || () } @flds;

# 	join $self->{_record_sep} => map {
	join $self->{_record_out_sep} => map {
		my @elements	= @{$_};
		my $line_label	= shift @elements;
		$line_label . ':' . join($self->{_input_sep} => @elements[@cols])
	} @{$self->{_struct}}[0,1,2];
}

sub meta {
	my $self = shift;
	my @flds = ( @_ ? @_ : @{ $self->{_head} } );
	return { map {
		!$self->{_hd_nums}->{$_} and ()
		  or
		$_ => {
		fld		=> $_,
		label	=> $self->{_labels}->{$_},
		type	=> $self->{_types}->{$_},
		size	=> $self->{_sizes}->{$_},
		col		=> $self->{_hd_nums}->{$_},
		packpat	=> $self->{_pack_pats}->{$_},
		colfmt	=> $self->{_col_pats}->{$_},
		default	=> $self->{_defaults}->{$_},
		key		=> $self->{_keys}->{$_},
		null	=> $self->{_nulls}->{$_},
		}
	} @flds }
}

sub type_RE {
	my $self = shift;
	my %typenames;
	for ( @{ $self->{_head} }) { $typenames{ $self->{_types}->{$_} }++ }
	return q/[/ . join( '' => keys %typenames) . q/]/;
}

sub first_id {
	my $self	= shift;
	my $first	= shift || '00000';

	$self->{_file}
		or do {$self->{_err}->("log: No database file specified for First ID: \n$!\n"); return};

	## Open the semaphore file and set the last ID
	sysopen(ID, $self->{_id_file}, O_RDWR | O_CREAT, oct(777))
		and ($self->{_lock_ok} and flock(ID, LOCK_EX) or 0)
			or do {$self->{_err}->("_first_id can't access semaphore $self->{_id_file}: $@ \n$!\n"); return};

	print ID $first, "\n";
	truncate ID, tell ID;
	close ID or return;
	$first
}


sub last_id {
	my $self = shift;

	$self->{FILE}
		or do {
				$self->{_err}->("log: No database file specified for Last ID: \n$!\n");
				return
			};

	unless ($self->{LAST_ID}) {
		unless (sysopen(ID, $self->{ID_FILE}, O_RDWR | O_CREAT, oct(777))
			and ($self->{LOCK_OK}	= eval { flock(LOCK, LOCK_EX) and 'OK' })) {
				$self->{_err}->("log: _last_id can't access semaphore $self->{_id_file}: $@ \n$!\n");
				return
		}
		unless (open DB,  $self->{FILE}) {
			close LOCK;
			$self->{_err}->("log: Can't open database file $self->{FILE}: $@, \n$!\n");
			return
		}

		my $last_line		= '';
		{
			local $/	= $self->{_record_sep};
			while (<DB>) {
				next unless $. > $self->{_line0};
				next if (/^\s*$/ or /$self->{_line_skip}/);
				last if /^\Q$self->{_table_sep}\E/;
				$last_line		= $_;
			}
			close DB;
		}
		chomp $last_line;
		$self->{LAST_ID}	= (split $self->{_input_sep} => $last_line)[$self->{_id_col}];
		$self->{LAST_ID}	||= $self->default($self->{_id_fld}) || '0';
		seek LOCK, 0, 0;
		print LOCK $self->{LAST_ID}, "\n";
		truncate LOCK, tell LOCK; # Obliterate anything else in the semaphore
		seek LOCK, 0, 0;
	}
	$self->{LAST_ID}
}

sub max_id {
	my $self = shift;

	$self->{FILE}
		or do {
				$self->{_err}->("log: No database file specified for Last ID: \n$!\n");
				return
			};

	unless (sysopen(ID, $self->{ID_FILE}, O_RDWR | O_CREAT, oct(777))
		and ($self->{LOCK_OK}	= eval { flock(LOCK, LOCK_EX) and 'OK' })) {
			$self->{_err}->("log: _last_id can't access semaphore $self->{_id_file}: $@ \n$!\n");
			return
	}
	unless (open DB,  $self->{FILE}) {
		close LOCK;
		$self->{_err}->("log: Can't open database file $self->{FILE}: $@, \n$!\n");
		return
	}

	my @IDS;

	my $last_line		= '';
	{
		local $/	= $self->{_record_sep};
		while (<DB>) {
			next unless $. > $self->{_line0};
			next if (/^\s*$/ or /$self->{_line_skip}/);
			last if /^\Q$self->{_table_sep}\E/;
			$last_line		= $_;
			chomp $last_line;
			push @IDS => (split $self->{_input_sep} => $last_line)[$self->{_id_col}];
		}
	}
	close DB;

	return (sort @IDS)[-1];
}

## Make Collector

sub make_collector {
	my $self				= shift;

	# Build the default collector object
	my $collector = {
		test		=> sub { ++$_[0]->{tried}; ++$_[0]->{kept} },
		kept		=> 0,
		tried		=> 0,
		select		=> 'all',
		phrase		=> 'all',
	};

	my $select				= shift;

	unless ($select) {
		return bless $collector, ref($self) || $self;
	}

	# schema -- this could be from args instead of $self
	$collector->{sep}		= $self->{_input_sep};
	$collector->{hdr}		= $self->{_head};
	$collector->{hd_nums}	= { map { $collector->{hdr}->[$_] => $_ } "0E0", 1..$#{ $collector->{hdr} } };
	$collector->{lbls}		= $self->{_labels} ?
								{ map { $_ => $self->{_labels}->{$_} || ucfirst($_) } @{ $collector->{hdr} } } :
									{ map { $_ => ucfirst($_) } @{ $collector->{hdr} } };

	return bless $collector, ref($self) || $self;

}


## Make Selector
## make_selector()
## HAS BEEN MOVED to DATA::WHERE


sub messages {
	my $self = shift;
	local $_ = shift || 'all';
	/msg/i and return $self->{_msg}->('print:')
	  or
	/err/i and return $self->{_err}->('print:')
	  or
	return $self->{_msg}->('print:') . "\n" . $self->{_err}->('print:');
}

## Generate message closures
## for placing dynamic text into layouts and templates
## $msg = make_db_msg("Record $id");
## $msg->("add: Updated"); # appends ' Updated' to msg
## $msg->("print: ...");  #outputs whole msg plus '...'
## $msg->("log: ...");  #outputs time-stamped msg
sub make_db_msg {
	my $default_msg		= shift || '';
	my $init_time		= localtime();
	$default_msg		=~ s/<time>/$init_time/ge;
	my $msg_holder		= ' ';
	my $mute_lock		= 0;

	return sub {
		my $in = shift;
		my $print_flush		= shift || 0;
		my $time = localtime();
		return $msg_holder unless $in;
		my ($command, $add_msg) = split ': ',  $in, 2;
		$add_msg and $add_msg =~ s/\<time\>/$time/ge;
		$add_msg ||= '';
		if ($command =~ /^mute/i) {
			$mute_lock++;
			return "Mute: " . ($add_msg || $time);
		} elsif ($command =~ /^unmute/i) {
			$mute_lock	= 0;
			return "Mute off: " . ($add_msg || $time);
		} elsif ($command =~ /^print/i) {
			$msg_holder .=  $add_msg || '' unless $mute_lock;
			my $print_out	= qq|$default_msg $msg_holder|;
			$msg_holder = ' ' if $print_flush;
			return $print_out;
		} elsif ($command =~ /^add/i) {
			$msg_holder .=  $add_msg unless $mute_lock;
			return $add_msg;
		} elsif ($command =~ /^log/i) {
			$msg_holder .=  qq|\n$time: $add_msg| unless $mute_lock;
			return qq|\n$time $default_msg $add_msg|;
		} else {
			$msg_holder = $in unless $mute_lock;
			return $in;
		}
	}
}

sub make_counter ($;@) {
	my $self	= shift;

	my ($label,$start,$progress,$increment,$column,$eol)	= @_;

	$label 				||= 'Count:';
	$start				||= 0;
	my $count			= $start - 1;

	$progress			||= '.';
	$increment			||= 200;
	$column				||= 50;
	$eol				||= "\t#\n";

	my $first_time		= 1;
	return sub {
		my $flag	= shift() || '';
		return $start if $flag eq 'first';
		return $count if $flag eq 'last';

		if ($flag eq 'progress') {
			return '' if $count == 0;
			my $progress_marker	= '';
			$progress_marker	.= $progress if ($count % $increment == 0);
			if ($count % ($column * $increment) == 0) {
				$progress_marker	.= $eol;
				$progress_marker	=~ s/\#/$count/g;
			}
			return $progress_marker;
		}

		return "$label  " . $count if ($flag eq 'labeled' or $flag eq 'last_labeled');

		if ($first_time) {
			$first_time = 0;
			return "$label  " . ++$count ;
		}

		return "\b" x length($count) . ++$count;
	}
}


sub get_excel_month_nums {
	my $self	= $_[0];
	return { qw/Jan 01 Feb 02 Mar 03 Apr 04 May 05 Jun 06 Jul 07 Aug 08 Sep 09 Oct 10 Nov 11 Dec 12/ };
}


sub DESTROY {
#	my ($pm,$me) = split '=', $_[0];
#	print "\n<!--[ Goodbye from $pm $VERSION ]-->\n";
}

1;


## Calculations
## Used in SELECT, BROWSE, INSERT
package BVA::XUI::DATA::CALC;

sub _default {
	my $self	= $_[0];

	my $record	= $_[1];

	my $col		= $_[2];

	return $record->[$col] . '**';
}

sub required {
	my $self	= $_[0];

	my $record	= $_[1];

	my $col		= $_[2];

	my ($fld)	= $self->labels($self->fields($col));

	my $id		= $_[1]->[$self->{_id_col}];

	$self->{_err}->("add: Missing Data for $fld in record ID $id of $self->{FILE_NAME}.\n");

	return "";
}

sub dated {
	my $self	= $_[0];

	my $record	= $_[1];

	my $col		= $_[2];

	# Derive standard local date values/indices
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();

	$year			+= 1900;
	my $mil_hour	= $hour;
	my $AP			= $hour > 11 ? 'PM' : 'AM' ;
	$hour			= $hour % 12 || 12;
	my $season		= $isdst ? 'DST' : 'STD' ; # local savings/standard time

	my $output_time	= sprintf "%4.4d-%2.2d-%2.2d",
	$year, $mon + 1, $mday;

	return $record->[$col] . " ($output_time)";
}

sub exec_date {
	my $self	= $_[0];

	my $rec		= $_[1];

	# Derive standard local date values/indices
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();

	$year			+= 1900;
	my $mil_hour	= $hour;
	my $AP			= $hour > 11 ? 'PM' : 'AM' ;
	$hour			= $hour % 12 || 12;
	my $season		= $isdst ? 'DST' : 'STD' ; # local savings/standard time

	my $output_time	= sprintf "%4.4d-%2.2d-%2.2d",
	$year, $mon + 1, $mday;

	return $output_time;

}

sub exec_date_time {
	my $self	= $_[0];

	my $rec		= $_[1];

	# Derive standard local date values/indices
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();

	$year			+= 1900;
	my $mil_hour	= $hour;
	my $AP			= $hour > 11 ? 'PM' : 'AM' ;
	$hour			= $hour % 12 || 12;
	my $season		= $isdst ? 'DST' : 'STD' ; # local savings/standard time

	my $output_time	= sprintf "%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d",
	$year, $mon + 1, $mday, $mil_hour, $min, $sec;

	return $output_time;

}

sub exec_time {
	my $self	= $_[0];

	# Derive standard local date values/indices
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();

	$year			+= 1900;
	my $mil_hour	= $hour;
	my $AP			= $hour > 11 ? 'PM' : 'AM' ;
	$hour			= $hour % 12 || 12;
	my $season		= $isdst ? 'DST' : 'STD' ; # local savings/standard time

	my $output_time	= sprintf "%2.2d:%2.2d:%2.2d",
	$mil_hour, $min, $sec;

	return $output_time;
}


1;

## Procedures
## Used in SELECT to modify return values with NO change to db record
## Used in SELECT, BROWSE, INDEX, COUNT to collect, summarize, & count data with NO change to db record
## Used in UPDATE to modify db record as it is updated
package BVA::XUI::DATA::PROC;

@BVA::XUI::DATA::PROC::tracks	= qw/COUNTS DUPES UNIQUE SUMS RECORDS TOTALS/;

sub printout {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	my $line	= sprintf "%8d: " . join(' ' =>
					map { ($self->col_formats($_))[0] || '%24s' } @flds),
					($self->{monitor_count}++ || 0) + 1,
					map {
						exists $self->{CURRENT}{ROW}->{$_} ? $self->{CURRENT}{ROW}->{$_} : ''
							|| ($_ =~ /^#(.*)$/ ? $self->{CURRENT}{$1} || '' : '')
								|| $_
					} @flds;
	print STDOUT $line, "\n";
}

sub monitor {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? grep( { $self->{_hd_nums}->{$_} } @_[1..$#_] ) : @{ $self->{_head} }[$self->{id_col} || 0];

	my $line	= sprintf "%8d Record%s OK: " . join(' ', $self->col_formats(@flds)) . "\r",
					($self->{monitor_count}++ || 0) + 1,
					($self->{monitor_count} == 1 ? ' ' : 's'),
					@{ $self->{CURRENT}{ROW} }{@flds};
	print STDOUT $line;
}

sub trim0 {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	for my $K (@flds) {
		$self->{CURRENT}{ROW}->{$K} =~ s/^0+(.*)/$1/
	}
}

sub trunc_right {
	my $self	= $_[0];
	my @flds	= @_ > 2 ? @_[2..$#_] : @{ $self->{_head} };
	my $width	= $_[1] =~ /^\d+$/ ? $_[1] : 0;
	for my $K (@flds) {
		my $w	= $width || $self->size($K);
		$_[0]->{CURRENT}{ROW}->{$K} =~ s/^(.{$w}).*$/$1/e;
	}
}

sub zero_pad {
	my $self	= $_[0];
	my @flds	= @_ > 2 ? @_[2..$#_] : @{ $self->{_head} };
	my $width	= $_[1] =~ /^\d+$/ ? $_[1] : 0;
	for my $K (@flds) {
		my $w	= $width || $self->size($K);
		$_[0]->{CURRENT}{ROW}->{$K} =~ s/^(\d+)(.*)$/ $w -= length($2);sprintf(qq{%0.${w}d%s}, $1, $2)/e;
	}
}			#

sub upcase {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	for my $K (@flds) {
		$_[0]->{CURRENT}{ROW}->{$K} = uc ${ $self->{CURRENT}{ROW} }{$K}
	}
}

sub lowcase {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	for my $K (@flds) {
		$_[0]->{CURRENT}{ROW}->{$K} = lc ${ $self->{CURRENT}{ROW} }{$K}
	}
}

sub track {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{TRACK}->{$fld}	= $self->{CURRENT}{ROW}->{$fld};
	}
}

sub break {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	my @fld_cols	= sort { $a <=> $b } $self->columns(@flds);
	my %pat_cols	= map { $_ => 1 } @flds;
	my $sep			= $self->{_input_sep};
	my $end			= $self->{_record_sep};
	my $break_pat	= join '' => map {	$pat_cols{$_} ?
							qr{\s*$self->{CURRENT}{ROW}->{$_}\s*(?:$sep.*|)$end?} :
							qr{[^$sep]*$sep}
					} @{ $self->{_head} }[ 0..$fld_cols[-1] ];
	push @{ $_[0]->{BREAK} } => sub { $_[1] !~ /^$break_pat/ };
}

sub next {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	my @fld_cols	= sort { $a <=> $b } $self->columns(@flds);
	my %pat_cols	= map { $_ => 1 } @flds;
	my $sep			= $self->{_input_sep};
	my $end			= $self->{_record_sep};
	my $next_pat	= join qr{$sep} => map {	$pat_cols{$_} ?
										qr{ *$self->{CURRENT}{ROW}->{$_} *} :
										qr{[^$sep]*}
 								} @{ $self->{_head} }[ 0..$fld_cols[-1] ];
	$_[0]->{NEXT}->[0] = sub { $_[1] =~ /^$next_pat/ };
}

sub unique {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? @_[1..$#_] : ($self->{id_fld});
	my @fld_cols	= sort { $a <=> $b } $self->columns(@flds);
	unless (@fld_cols) {
		$_[0]->{NEXT}->[0] = sub { 0 };
		return;
	}
	my @sorted_flds	= $self->fields(@fld_cols);
	my $key			= join '-' => @sorted_flds;
	$_[0]->{UNIQUE}->{$key}->{ join '-' => map { $self->{CURRENT}{ROW}->{$_} } @sorted_flds }++;
	my $sep			= $self->{_input_sep};
	my $end			= $self->{_record_sep};
	my %pat_cols	= map { $_ => 1 } @flds;
	my $umulti_pat	= join '' => map {
		$pat_cols{$_} ? qr{ *([^$sep]*) *(?:$sep|$end)} : qr{[^$sep]*$sep}
	} $self->fields(0..$fld_cols[-1]);

	$_[0]->{NEXT}->[0] = sub {
		my @matches = $_[1] =~ /^$umulti_pat/;
		@matches && $_[0]->{UNIQUE}->{$key}->{ join '-' => @matches };
	};
}

sub count_true {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{COUNTS}->{$fld}++ if $self->{CURRENT}{ROW}->{$fld}
	}
}

sub count_binary {
	my $self		= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'b' } @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{COUNTS}->{$fld}++ if $self->{CURRENT}{ROW}->{$fld}
	}
}

sub count_binary_bc {
	my $self		= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} =~ /b|c/ } @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{COUNTS}->{$fld}++ if $self->{CURRENT}{ROW}->{$fld}
	}
}

sub count_unique {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? grep( { $self->{_hd_nums}->{$_} } @_[1..$#_] ) : @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{COUNTS}->{$fld}->{ $self->{CURRENT}{ROW}->{$fld} }++
	}
}

sub count_unique_multi {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	my $isep		= $self->{_item_sep};
	for my $fld (@flds) {
		my @actuals	= split /\s*(?:$isep)\s*/ => $self->{CURRENT}{ROW}->{$fld};
		for (@actuals) {
			$_[0]->{COUNTS}->{$fld}->{ $_ }++
		}
	}
}

sub cross_tab {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };

	# Binary Fields
	my @binary_flds	= grep { $self->{_types}{$_} eq 'b' } @flds;

	# Value Fields (for now, just recognized fields not type 'b')
	my @value_flds	= grep { $self->{_types}{$_} and $self->{_types}{$_} ne 'b' } @flds;

	for my $fld (@binary_flds) {
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{COUNTS}->{$fld}++;

		for my $cross_fld (@binary_flds) {
			next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{COUNTS}->{CROSS}->{ $cross_fld }->{ $fld }++;
		}

		for my $cross_fld (@value_flds) {
			# next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{COUNTS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $fld }++;
		}
	}

	for my $fld (@value_flds) {
		$_[0]->{COUNTS}->{$self->{CURRENT}{ROW}->{$fld} || "*No $fld"}++;
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{COUNTS}->{$fld}++;
		$_[0]->{COUNTS}->{"$fld:$self->{CURRENT}{ROW}->{$fld}"}++;


		for my $cross_fld (@binary_flds) {
			next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{COUNTS}->{CROSS}->{ $cross_fld }->{ $self->{CURRENT}{ROW}->{$fld} }++;
			#$_[0]->{COUNTS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $self->{CURRENT}{ROW}->{$fld} }++;
		}

		for my $cross_fld (@value_flds) {
			#next if $fld eq $cross_fld;
			$_[0]->{COUNTS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $self->{CURRENT}{ROW}->{$fld} }++;
		}
	}
}

sub cross_total {
	my $self		= $_[0];
	my $sum_fld		= $_[1];
	my @flds		= @_ > 2 ? @_[2..$#_] : @{ $self->{_head} };

	# Binary Fields
	my @binary_flds	= grep { $self->{_types}{$_} eq 'b' } @flds;

	# Value Fields (for now, just recognized fields not type 'b')
	my @value_flds	= grep { $self->{_types}{$_} and $self->{_types}{$_} ne 'b' } @flds;

	# Grand total of values in $sum_fld
	$_[0]->{SUMS}->{TOTAL}->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
	$_[0]->{SUMS}->{TOTAL}->{count}++;

	for my $fld (@binary_flds) {
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{SUMS}->{$fld}->{count}++;
		$_[0]->{SUMS}->{$fld}->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};

		for my $cross_fld (@binary_flds) {
			next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{SUMS}->{CROSS}->{ $cross_fld }->{ $fld }->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
			$_[0]->{SUMS}->{CROSS}->{ $cross_fld }->{ $fld }->{count}++;
		}

		for my $cross_fld (@value_flds) {
			# next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{SUMS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $fld }->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
			$_[0]->{SUMS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $fld }->{count}++;
		}
	}

	for my $fld (@value_flds) {
		$_[0]->{SUMS}->{$self->{CURRENT}{ROW}->{$fld} || "*No $fld"}->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
		$_[0]->{SUMS}->{$self->{CURRENT}{ROW}->{$fld} || "*No $fld"}->{count}++;
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{SUMS}->{$fld}->{count}++;
		$_[0]->{SUMS}->{$fld}->{sum}								+= $self->{CURRENT}{ROW}->{$sum_fld};
		$_[0]->{SUMS}->{$fld}->{ $self->{CURRENT}{ROW}->{$fld} }	+= $self->{CURRENT}{ROW}->{$sum_fld};


		for my $cross_fld (@binary_flds) {
			next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{SUMS}->{CROSS}->{ $cross_fld }->{ $self->{CURRENT}{ROW}->{$fld} }->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
			$_[0]->{SUMS}->{CROSS}->{ $cross_fld }->{ $self->{CURRENT}{ROW}->{$fld} }->{count}++;
		}

		for my $cross_fld (@value_flds) {
			#next if $fld eq $cross_fld;
			$_[0]->{SUMS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $self->{CURRENT}{ROW}->{$fld} }->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
			$_[0]->{SUMS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $self->{CURRENT}{ROW}->{$fld} }->{count}++;
		}
	}
}

sub sum_unique {
	my $self		= $_[0];
	my $sum_fld		= $_[1];
	my @flds		= @_ > 2 ? @_[2..$#_] : @{ $self->{_head} };

	# Binary Fields
	my @binary_flds	= grep { $self->{_types}{$_} eq 'b' } @flds;

	# Value Fields (for now, just recognized fields not type 'b')
	my @value_flds	= grep { $self->{_types}{$_} and $self->{_types}{$_} ne 'b' } @flds;

	# Grand total of values in $sum_fld
	$_[0]->{SUMS}->{TOTAL}->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
	$_[0]->{SUMS}->{TOTAL}->{count}++;

	for my $fld (@binary_flds) {
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{SUMS}->{$fld}->{count}++;
		$_[0]->{SUMS}->{$fld}->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};

	}

	for my $fld (@value_flds) {
		$_[0]->{SUMS}->{$self->{CURRENT}{ROW}->{$fld} || "*No $fld"}->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
		$_[0]->{SUMS}->{$self->{CURRENT}{ROW}->{$fld} || "*No $fld"}->{count}++;
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{SUMS}->{$fld}->{count}++;
		$_[0]->{SUMS}->{$fld}->{sum}								+= $self->{CURRENT}{ROW}->{$sum_fld};
		$_[0]->{SUMS}->{$fld}->{ $self->{CURRENT}{ROW}->{$fld} }	+= $self->{CURRENT}{ROW}->{$sum_fld};

		for my $cross_fld (@value_flds) {
			#next if $fld eq $cross_fld;
			$_[0]->{SUMS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $self->{CURRENT}{ROW}->{$fld} }->{sum}	+= $self->{CURRENT}{ROW}->{$sum_fld};
			$_[0]->{SUMS}->{CROSS}->{ $self->{CURRENT}{ROW}->{$cross_fld} || "*No $cross_fld" }->{ $self->{CURRENT}{ROW}->{$fld} }->{count}++;
		}

	}
}

sub cross_tab_cube {
	my $self		= $_[0];
	my @flds		= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };

	# Binary Fields
	my @binary_flds	= grep { $self->{_types}{$_} eq 'b' } @flds;

	# Value Fields (for now, just recognized fields not type 'b')
	my @value_flds	= grep { $self->{_types}{$_} and $self->{_types}{$_} ne 'b' } @flds;

	for my $fld (@binary_flds) {
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{COUNTS}->{"$fld Subtotal"}++;

		for my $cross_fld (@binary_flds) {
			next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{COUNTS}->{CROSS}->{ $fld }->{ $cross_fld }++;
		}

		for my $cross_fld (@value_flds) {
			# next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{COUNTS}->{CROSS}->{ $fld }->{ $cross_fld }->{ $self->{CURRENT}{ROW}->{$cross_fld} || "No $cross_fld" }++;
		}
	}

	for my $fld (@value_flds) {
		next unless $self->{CURRENT}{ROW}->{$fld};
		$_[0]->{COUNTS}->{$fld}->{"$fld Subtotal"}++;
		$_[0]->{COUNTS}->{$fld}->{$self->{CURRENT}{ROW}->{$fld}}++;

		for my $cross_fld (@binary_flds) {
			next unless $self->{CURRENT}{ROW}->{$cross_fld};
			$_[0]->{COUNTS}->{CROSS}->{ $fld }->{ $self->{CURRENT}{ROW}->{$fld} }->{ $cross_fld }++;
		}

		for my $cross_fld (@value_flds) {
			# next unless ($self->{CURRENT}{ROW}->{$cross_fld});
			next if ($fld eq $cross_fld);
			$_[0]->{COUNTS}->{CROSS}->{ $fld }->{ $self->{CURRENT}{ROW}->{$fld} }->{ $cross_fld }->{ $self->{CURRENT}{ROW}->{$cross_fld} || "No $cross_fld" }++;
		}
	}
}

sub gather_unique {
	my $self			= $_[0];
# 	my ($unique, @flds)	= @_ > 1 ? @_[1..$#_] : ($self->{_head}->[$self->{_id_col}], @{ $self->{_head} });
	my $unique			= @_ > 1 ? $_[1] : $self->{_head}->[$self->{_id_col}];
	my @flds			= @_ > 2 ? @_[2..$#_] : @{ $self->{_head} };
	$_[0]->{COUNTS}->{$unique}->{ $self->{CURRENT}{ROW}->{$unique} }[0]++;
	push @{ $_[0]->{COUNTS}->{$unique}->{ $self->{CURRENT}{ROW}->{$unique} } } => @{ $self->{CURRENT}{ROW} }{@flds}
}

sub gather_unique_records {
	my $self			= $_[0];
	my $unique			= @_ > 1 ? $_[1] : $self->{_head}->[$self->{_id_col}];
	$_[0]->{COUNTS}->{$unique}->{ $self->{CURRENT}{ROW}->{$unique} }[0]++;
	$_[0]->{RECORDS}->{$unique}->{ $self->{CURRENT}{ROW}->{$unique} } = $self->{CURRENT}{ROW};
}

sub catflds {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join '' => map { $self->{CURRENT}{ROW}->{$_} } @flds;
}

sub catflds_replace {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join '' => map { $self->{CURRENT}{ROW}->{$_} } @flds[1..$#flds];
}

sub catflds_hyphen {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join '-' => map { $self->{CURRENT}{ROW}->{$_} } @flds;
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/-+/-/g;
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/^-*(.*?)-*$/$1/g;
}

sub catflds_replace_hyphen {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join '-' => map { $self->{CURRENT}{ROW}->{$_} } @flds[1..$#flds];
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/-+/-/g;
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/^-*(.*?)-*$/$1/g;
}

sub catflds_period {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join '.' => map { $self->{CURRENT}{ROW}->{$_} } @flds;
}


sub catflds_replace_period {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join '.' => map { $self->{CURRENT}{ROW}->{$_} } @flds[1..$#flds];
}


sub catflds_tab {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join "\t" => map { $self->{CURRENT}{ROW}->{$_} } @flds;
}

sub catflds_replace_tab {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join "\t" => map { $self->{CURRENT}{ROW}->{$_} } @flds[1..$#flds];
}

sub catflds_space {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join " " => map { $self->{CURRENT}{ROW}->{$_} } @flds;
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/ +/ /g;
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/^\s*(.*?)\s*$/$1/g;
}

sub catflds_replace_space {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : @{ $self->{_head} };
	$self->{CURRENT}{ROW}->{$flds[0]} = join " " => map { $self->{CURRENT}{ROW}->{$_} } @flds[1..$#flds];
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/ +/ /g;
	$self->{CURRENT}{ROW}->{$flds[0]} =~ s/^\s*(.*?)\s*$/$1/g;
}

sub progress {
	my $self		= $_[0];
	my $incr		= $_[1] || ( $_[0]->{LIMIT}->[0]/1000 > 10 ? $_[0]->{LIMIT}->[0]/1000 : 100 );
	my $width		= $_[2] || 100;
	print STDOUT '.'		if ($_[0]->{selector}->{count} % $incr == 0);
	print STDOUT "\n"		if ($_[0]->{selector}->{count} % ($width * $incr) == 0);
}

sub clean_binaries {
    my $self    = $_[0];
    my @flds    = @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'b' } @{ $self->{_head} };
    for my $fld (@flds) {
       if ($_[0]->{CURRENT}{ROW}{$fld} =~ /^\s*(1|Y|Yes|True)\s*$/i) {
			$_[0]->{CURRENT}{ROW}{$fld} = 1;
       } elsif ($_[0]->{CURRENT}{ROW}{$fld} =~ /^\s*(0|N|No|False)\s*$/i) {
			$_[0]->{CURRENT}{ROW}{$fld} = 0;
       }
    }
}

sub force_binaries {
    my $self    = $_[0];
    my @flds    = @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'b' } @{ $self->{_head} };
    for my $fld (@flds) {
       if ($_[0]->{CURRENT}{ROW}{$fld} =~ /^\s*(1|Y|Yes|True)\s*$/i) {
			$_[0]->{CURRENT}{ROW}{$fld} = 1;
       } elsif ($_[0]->{CURRENT}{ROW}{$fld} =~ /^\s*(0|N|No|False)\s*$/i) {
			$_[0]->{CURRENT}{ROW}{$fld} = 0;
       } elsif ( ! $_[0]->{CURRENT}{ROW}{$fld} ) {
			$_[0]->{CURRENT}{ROW}{$fld} = 0;
       } else {
			$_[0]->{CURRENT}{ROW}{$fld} = 1;
       }
    }
}

sub fix_dates {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'd' } @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)\/(\d\d?)\/(\d\d\d\d)}
										{sprintf qq{%04d-%02d-%02d}, $3,$1,$2}e;
	}
}

sub fix_us_short_dates {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'd' } @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^01\/01\/1900}{1900-01-01};  # mont_co no date
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)\/(\d\d?)\/(18\d\d)(.*)$}
										{sprintf qq{%04d-%02d-%02d}, $3,$1,$2}e;
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)\/(\d\d?)\/(19\d\d)(.*)$}
										{sprintf qq{%04d-%02d-%02d}, $3,$1,$2}e;
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)\/(\d\d?)\/(20\d\d)(.*)$}
										{sprintf qq{%04d-%02d-%02d}, $3,$1,$2}e;

		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)\/(\d\d?)\/([1-9]\d)(.*)$}
										{sprintf qq{19%02d-%02d-%02d}, $3,$1,$2}e;
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)\/(\d\d?)\/(0\d|10)(.*)$}
										{sprintf qq{20%02d-%02d-%02d}, $3,$1,$2}e;
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)-(\d\d?)-([1-9]\d)(.*)$}
										{sprintf qq{19%02d-%02d-%02d}, $3,$1,$2}e;
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)-(\d\d?)-(0\d|10)(.*)$}
										{sprintf qq{20%02d-%02d-%02d}, $3,$1,$2}e;
	}
}

sub fix_excel_dates {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'd' } @{ $self->{_head} };
	my %month_nums = %{ $self->{COUNTS}->{'_month_nums'} ||= $self->get_excel_month_nums() };
	for my $fld (@flds) {
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)-(\w\w\w)-(\d\d\d\d)}
										{sprintf qq{%04d-%02d-%02d}, $3,$month_nums{$2},$1}e;
	}
}

sub fix_excel_short_dates {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'd' } @{ $self->{_head} };
	my %month_nums = %{ $self->{COUNTS}->{'_month_nums'} ||= $self->get_excel_month_nums() };
	for my $fld (@flds) {
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)-(\w\w\w)-(\d\d)}
										{sprintf qq{20%02d-%02d-%02d}, $3,$month_nums{$2},$1}e;
	}
}

sub fix_date_times {
	my $self	= $_[0];
	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'd' } @{ $self->{_head} };
	for my $fld (@flds) {
		$_[0]->{CURRENT}{ROW}{$fld} =~ s{^(\d\d?)\/(\d\d?)\/(\d\d\d\d)([ T](\d{1,2}):(\d{2})(:(\d{2}))?)?}
										{sprintf qq{%04d-%02d-%02d}, $3,$1,$2}e;
	}
}

sub set_date {
	my $self	= $_[0];

	my @flds	= @_ > 1 ? @_[1..$#_] : grep { $self->{_types}{$_} eq 'd' } @{ $self->{_head} };

	# Derive standard local date values/indices
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();

	$year			+= 1900;
	my $mil_hour	= $hour;
	my $AP			= $hour > 11 ? 'PM' : 'AM' ;
	$hour			= $hour % 12 || 12;
	my $season		= $isdst ? 'DST' : 'STD' ; # local savings/standard time

	for my $fld (@flds) {
		$_[0]->{CURRENT}{ROW}{$fld} = sprintf "%4.4d-%2.2d-%2.2d",
			$year, $mon + 1, $mday;
	}
}




1;



__DATA__




