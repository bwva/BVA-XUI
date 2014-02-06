package BVA::XUI;

$BVA::XUI::VERSION			= 3.6010;
$BVA::XUI::VERSION_DATE		= '2013-10-31';

use strict;
use warnings qw/all/;

=head1 NAME

BVA::XUI - Extensible User Interface!

=head1 VERSION

Version 3.6010

=cut

use vars qw/ *KEY *OUTPUT *INPUT $hdr_prntd/;

use parent qw{ BVA::XUI::IN BVA::XUI::DB BVA::XUI::DATA BVA::XUI::OUT BVA::XUI::UTILS BVA::XUI::DATETIME };

=head1 SYNOPSIS

XUI provides methods and structures for creating dynamic user interfaces.

    use BVA::XUI;

    my $ui			= BVA::XUI->init();
    my $students	= $ui->new( _mark => 'ST', _start => '{*', _end => '*}");
    my $tmpl		= "{*ST:DATA=number*}\t{*ST:DATA=firstname*}\t{*ST:DATA=firstname*}\t{*ST:DATA=birthdate*}\t\n};
    $students->form($tmpl);
    my $counter		= 0;
    while (my $st = $sth->fetchrow_hashref()) {  # data from somewhere
    	$students->charge($st);
    	$students->charge(number => $counter++);
    	$students->buffer;
    }    
    my $list	= join '' $students->flush();

=head1 EXPORT

No functions exported.

=head1 SUBROUTINES/METHODS

=head2 init

=cut


## Initialization #################
## Creates and returns the master UI object generator
sub init {
	my $class	= shift;
	my $args	= shift;
	{
		local $_	= ref($args);
		/HASH/i		and	*KEY						= $args
	 	  or
		/ARRAY/		and	@KEY{qw/_env _start _end/}	= @$args
		  or
						@KEY{qw/_env _start _end/}	= ($args, @_);
	}

	$KEY{_mark}				||= 'KEY';
	$KEY{_sub}				||= \&OUTPUT;
	
	$KEY{_init_time}		= BVA::XUI::DATETIME::tell_time('iso_store',time);
	$KEY{_env}				||= exists $ENV{SERVER_PROTOCOL}	? 'htm'
								: exists $ENV{TERM_PROGRAM}		? 'term'
                                : 'edit';

	$KEY{_start}			||= $KEY{_env} =~ /^htm/i	? '<!--'	: '%';
	$KEY{_end}				||= $KEY{_env} =~ /^htm/i	? '-->'		: '%';

  	$KEY{_match_str}		= qr{(?s:((\Q$KEY{_start}$KEY{_mark}\E)[: -]((.(?!\2))*?)\Q$KEY{_end}\E))};

	$KEY{_default_tmpl}		= qq{$KEY{_start}$KEY{_mark}:DATA=_mark$KEY{_end}};
	$KEY{_ui_mark}			= $KEY{_mark};

	$KEY{_in_lib}			||= '';
	$KEY{_in_max}			||= 1000;
	$KEY{_home_dir}			||= './';
	$KEY{_display_dir}		||= "$KEY{_home_dir}/displays";
	$KEY{_list_dir}			||= "$KEY{_home_dir}/lists";
	$KEY{_message_dir}		||= "$KEY{_home_dir}/messages";
	$KEY{_image_dir}		||= "$KEY{_home_dir}/images";
	$KEY{_message_file}		||= '';
	$KEY{_list_file}		||= '';
	$KEY{_dbi}				||= undef;
	$KEY{_dbh}				||= undef;

	## Start with the typeglob based on the %KEY hash
	*KEY = \%KEY;
	
	## If a master output sub was specified, assign it to the typeglob
	unless (defined(&KEY) ) {
		*KEY = $KEY{_sub} || \&{"KEY{_mark}"};
	}

	## _objects: Store a reference to the generator's buffer
	$KEY{_objects}			= \@KEY;

	## generator: Create the ui object generator
	## Access it with class or instance method ->generator()
 	$KEY{_generator}		=  bless \*KEY, $class;

 	## Store the generator in @KEY; its ui objects will each be stored there, too.
 	## Access them with class or instance method ->objects().
 	push @{ *KEY } 			=> $KEY{_generator};

	$KEY{_generator}->INPUT;

	$KEY{_generator}->get_marked_input;

	return $KEY{_generator}
}

# Instantiate a UI object around a filehandle or a typeglob
sub new {
	my $self					= shift();
	unless (ref($self) eq __PACKAGE__ and exists ${ $self }->{_mark}) {
		$self = __PACKAGE__->init();
	}

	my $args					= shift || {};
	my %properties				= %{ *$self };
	$properties{_ui_mark}		= $properties{_mark};
	{
		local $_	= ref($args);
		/HASH/i		and %properties										= (%properties, %$args)
	 	 or
		/ARRAY/		and @properties{qw/_mark _file _start _end _sub/}	= @$args
		  or
					@properties{qw/_mark _file _start _end _sub/}		= ($args, @_);
	}

	$properties{_mark}			||= '';
	$properties{_sub}			||= \&OUTPUT;

	$properties{_init_time}		= BVA::XUI::DATETIME::tell_time('iso',time);
	$properties{_env}			||= exists $ENV{SERVER_PROTOCOL}	? 'htm'
									: exists $ENV{TERM_PROGRAM}		? 'term'
                              		: 'edit';

	$properties{_start}			||= $properties{_env} =~ /^htm/i ? '<!--' : '#';
	$properties{_end}			||= $properties{_env} =~ /^htm/i ?  '-->' : '#';

  	$properties{_match_str}		= $properties{'_is_direct'} ? 
  									$properties{_match_str} :
  									qr{(?s:((\Q$properties{_start}$properties{_mark}\E)[: -]((.(?!\2))*?)\Q$properties{_end}\E))};

	## Default template
	$properties{_default_tmpl}	||= qq{$properties{_start}$properties{_mark}:DATA=_mark$properties{_end}};

	$properties{_in_lib}		||= '';
	$properties{_in_max}		||= 1000;

	$properties{_display_dir}	||= "$properties{_home_dir}/displays";
	$properties{_list_dir}		||= "$properties{_home_dir}/lists";
	$properties{_message_dir}	||= "$properties{_home_dir}/messages";
	$properties{_image_dir}		||= "$properties{_home_dir}/images";

	$properties{_message_file}	||= '';
	$properties{_list_file}		||= '';
	$properties{_dbi}			||= undef;
	$properties{_dbh}			||= undef;
	$properties{_req_prefix}	||= '*';

	$properties{_file}			||= '-';
	$properties{_file}			= $properties{_file} =~ m{^\s*([^`<>]+)\s*$} ? $1 : '-';

	## Object instantiation
	my $new_obj;
	
	if ($properties{_mark}) {
		if ($properties{_file} ne '-') {
			no strict 'refs';
			open (\*{"main::$properties{_mark}"}, "+>>", $properties{_file}) or die "Can't open file $properties{_file}: $!";
			select((select(*{"main::$properties{_mark}"}{IO}), $|=1)[0]);
			chmod 0777, $properties{_file};
		}
		no strict 'refs';
		*{"main::$properties{_mark}"} = \%properties;
		unless (defined(&{"main::$properties{_mark}"}) ) {
			*{"main::$properties{_mark}"} = $properties{_sub} || \&{"main::$properties{_mark}"};
		}
		$new_obj = bless \*{"main::$properties{_mark}"},  ref($self) || $self;
	} else {
		no strict 'refs';
		*{"main::OUTPUT"} = \%properties;
		$new_obj = bless \*{"main::OUTPUT"},  ref($self) || $self;
	}
	
	# Don't store or collect input for direct() objects
	return $new_obj if $new_obj->data('_is_direct');
	
	## Store the instance ref for use by render() and objects()
	push @{ *$self } => $new_obj;

	## Collect any input marked for this object
	$new_obj->get_marked_input;

	return $new_obj
}

## INPUT
# Capture, clean, and untaint any input
# Filter Input by field and source type
sub INPUT {
	goto &BVA::XUI::IN::IN
}

## OUTPUT
## Main Display Dispatcher
sub OUTPUT {
    goto &BVA::XUI::OUT::OUT
}

############################################


sub generator ($;@) {
 	local *KEY		= *{ shift() };

	return $KEY{_generator};
}

## Invert: Turns the UI object into its internal typeglob (*KEY)
sub invert ($) {
	\*{ $_[0] }
}

## Objects: returns a list of all current UI objects, or
## a list of the objects specified in the args with
## valid KEY marks ($KEY{_mark} -- 'KEY').
sub objects ($;@) {
 	local *KEY		= *{ shift() };

	my @objs		= grep { UNIVERSAL::isa($_, 'GLOB') }  @{ $KEY{_objects} };

	if (@_) {
 		my %objs	= map {  ${ *$_ }{_mark} => $_ } @objs;
  		return map { $objs{$_} ? $objs{$_} : () } @_;
 	}

	@objs;
}

## direct: instantiate a ui object with an atomic output sub.
## Accessible as class or instance method ->direct()
## Use in place of ->new(), with no args.
## Inherits meta_data from the object or generator that creates it.
## Memoizes the ui object (for its parent);
## NOTE: direct()'s _match_str allows \b to separate mark from EXPR, while
## the standard _match_str REQUIRES a colon ':' or space ' ' following the mark;
## this allows direct()'s templates to use a colon AS the mark: 
## E.g, with _start => '[', _end => ']', _mark => ':',
## can now use  [:fieldname] instead of [::fieldname] or [: fieldname].
## 2013-02-13 This match_str also allows expressions of the type MARK:fieldname, with
## the parent ui object's mark, a colon, underscore or period (: _ .), and then the fieldname,
## no brackets needed. E.g., if _mark is 'SUP', SUP:address, SUP_address, or SUP.address
sub direct ($;@) {
 	local *KEY	= *{ shift() };
 	my $owner	= $KEY{_mark};
 	my $mark	= $KEY{_direct_mark}	|| ':';
 	my $start	= $KEY{_direct_start}	|| '[';
 	my $end		= $KEY{_direct_end}		|| ']';
	
   	return $KEY{_generator}->new({
 		_is_direct	=> 1,
 		_mark		=> $mark, 
		_start		=> $start, 
		_end		=> $end,
		_owner		=> $owner,
    	_match_str	=> qr{(?sx:
			((\Q${start}${mark}\E)	(?:\:|\s|\b)	((.(?!\2))+?)	\Q${end}\E)    		
			|
    		((\Q${owner}\E[:_.])					((\S(?!\6))+?)	(?:\b))
    	)},
		_sub	=> sub {
			local *KEY	= *{ shift() };
			my $arg		= shift();
			return '' unless defined $arg;
			$arg		=~ s/^\s*(.*?)\s*$/$1/s;
			my $out		= defined $KEY{$arg} ? $KEY{$arg} : "";
			$KEY{__DONE__}++ unless $out;
			return $out;
		},
	});
}

## Resolve: returns a string with the UI object's data values in %KEY, including meta-data, 
## atomically substituted for any template tokens used by the object's direct() method.
## The default direct() token uses _start => '[', _end => ']', and _mark => ':'.
## $obj->resolve('[:system_dir]/settings.config'); # '/Volumes/Dev/MyProject/System/settings.config'
## Multiple args are joined with '' because _process_tmpl takes only one template arg.
## NOTE: Resolve does NOT auto-charge or calculate, to avoid traps in deep recursion
sub resolve ($@) {
	my $obj	= shift();
	
	return $obj->direct->_process_tmpl(join '' => @_);
}

## Render: Class method that recursively processes
## all instantiated ui objects against template
## composed of any optional args, or $KEY,
## or the default template.
## Use with the object generator, which stores each
## ui object in @KEY.
## Returns processed output.
sub render ($;@) {
	local *KEY		= *{ shift() };

	my $tmpl		= join('' => @_)
						|| $KEY
							|| $KEY{_default_tmpl};
	my $output		= ' ';

	my @objects		= objects( *KEY );
	LOAD: {
		foreach my $obj ( @objects ) {
			$obj->charge_auto();
 			$obj->calculate();
		}
	}
	# Make sure nesting works across UI objects
	my $start_pat	= '(' . join( '|' => map { "\Q${ *{ $_ } }{_start}\E" } @objects) . ')';
	my $mark_pat	= '(' . join( '|' => map { ${ *{ $_ } }{_mark} } @objects) . ')';

	RENDER: {
		foreach my $obj ( @objects ) {
			my ($start, $end, $mark, $own_match_str)
				= $obj->data('_start','_end','_mark','_match_str');
 			$obj->charge_meta(_match_str
 				=> qr{(?s:((\Q${start}${mark}\E)[: ]((.(?!${start_pat}${mark_pat}))*?)\Q$end\E))});
			$tmpl = $obj->_process_tmpl($tmpl);
			$obj->charge_meta(_match_str	=> $own_match_str);
		}
		last RENDER if $output eq $tmpl;
		$output = $tmpl and redo RENDER
	}

	return $output
}

sub render_qp ($;@) {
	local *KEY		= *{ shift() };

	my $tmpl		= join('' => @_)
						|| $KEY
							|| $KEY{_default_tmpl};
	my $output		= ' ';

	my @objects		= objects( *KEY );

	LOAD: {
		foreach my $obj ( @objects ) {
			$obj->charge_auto();
 			$obj->calculate();
		}
	}

	# Make sure nesting works across UI objects
	my $start_pat	= '(' . join( '|' => map { "\Q${ *{ $_ } }{_start}\E" } @objects) . ')';
	my $mark_pat	= '(' . join( '|' => map { ${ *{ $_ } }{_mark} } @objects) . ')';

	RENDER: {
		foreach my $obj ( @objects ) {
			my ($start, $end, $mark, $own_match_str)
				= $obj->data('_start','_end','_mark','_match_str');
 			$obj->charge_meta(_match_str
 				=> qr{(?s:((\Q${start}${mark}\E)[: ]((.(?!${start_pat}${mark_pat}))*?)\Q$end\E))});
			$tmpl = $obj->_process_tmpl($tmpl);
			$obj->charge_meta(_match_str	=> $own_match_str);
		}
		last RENDER if $output eq $tmpl;
		$output = $tmpl and redo RENDER
	}

	require MIME::QuotedPrint;
	return MIME::QuotedPrint::encode_qp( $output || $KEY );
}

## Replace: Processes the current template with available data;
##			Returns processed output.
sub replace ($;@) {
	my $arg1		= shift;
	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	my $tmpl		= join('' => @_)
						|| $KEY
							|| $KEY{_default_tmpl};

	$tmpl	= _process_tmpl(\*KEY, $tmpl);

 	return $tmpl;
}

## Process: Provides OO access to dispatch sub $KEY{_sub}.
## The arg is the same as what replace() would pass to the dispatcher
## after stripping off the _start, _mark, and _end tokens.
## In list context, returns an array whose elements
## are the processed values of the arguments in order.
## The same array is returned concatenated as a string in scalar context.
## Output may be () or ''.
## Usage: $obj->process(qq{INPUT=$fld,$value,r,,,-1})
## Compare to $obj->replace(qq{<!--KEY:INPUT=$fld,$value,r,,,-1-->})
## where _start is '<!--', _mark is 'KEY', _end is '-->.
sub process {
	my $arg1		= shift;
	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	defined	&KEY
		or	*KEY	= \&OUTPUT;

#	my @products	= map { $KEY{_sub}->(\*KEY, $_) || OUTPUT(\*KEY, $_) || $_ } @_;
	my @products	= map { KEY(\*KEY, $_) || OUTPUT(\*KEY, $_) || $_ } @_;

	return wantarray ? @products : join ' ' => @products
}

## _process_tmpl: the "private" internal sub for processing templates.
## MUST be curried to receive valid \*KEY and a single defined $tmpl string;
## Recursively replaces template markers,
## by using pattern matching [_start][_mark](...)[_end]
## and handing each (...) to the UI object's designated OUTPUT dispatcher.
## The OUTPUT() dispatcher uses available data and subroutines or defaults to
## substitute for (...), and returns the result for another iteration.
## Returns $tmpl after substitutions are exhausted.
sub _process_tmpl ($$) {
 	local *KEY	= shift;
	defined	&KEY
		or	*KEY	= \&OUTPUT;

	my $tmpl	= shift;

	GO: {
		my $str = $tmpl;
		study $str;
		my $result;
		$str =~ s{$KEY{_match_str}}
					{ $result = KEY(\*KEY, $3||$7)
						or !$KEY{__DONE__} and $result = OUTPUT(\*KEY, $3||$7)
							or (delete $KEY{__DONE__} ? $result : $1) }gsex;
		last GO if $str eq $tmpl;
		$tmpl = $str and redo GO;
	}

	return $tmpl;
}

## Attach file: allows a file to be specified after instantiation of the object
sub attach_file {
	local *KEY		= *{ shift() };
	my $file		= shift;
	if (UNIVERSAL::isa($file, 'GLOB')) {
 		*KEY = $file
	} elsif ($file =~ m{^([^`]+)$}) {
		$file		= $1;
		if (open KEY, "+>>", $file) {
			$KEY{_file}	= $file;
			chmod 0777, $file;
		} else {
			#warnings::warnif( 'io', "Problem with attaching file $file:\n$!");
# 			die "Problem with attaching file $file:\n$!";
			charge_err(\*KEY, "Problem with attaching file $file:\n$!") and return;
		}
	} else {
		#warnings::warnif( 'io', "Proposed attached file is neither a filehandle nor a valid filename.");
		die "Proposed attached file is neither a filehandle nor a valid filename.";
	}
	\*KEY
}

##################

## Buffer: Same as replace, plus all output is accumulated in the array
## @KEY, which may be accessed as @{$mark} in the calling program
## or by using any of the flush or read variants:
## flush(), flush_lifo(), flush_next(), flush_last(), flush_trained
## read_fifo(), read_lifo(), read_unique(), read_except()
sub buffer ($;@) {
	my $arg1		= shift;
	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	my $tmpl		=  join('' => @_)
						|| $KEY
							|| $KEY{_default_tmpl};

	$tmpl	= _process_tmpl(\*KEY, $tmpl);
	push @KEY => $tmpl;

	return $tmpl;
}

## Buffer Trained !!!!
sub buffer_trained {

}

## Flush: returns and clears the @KEY buffer
sub flush ($) {
	local *KEY		= *{ shift() };
	my @buf;
	@KEY			= map { if (UNIVERSAL::isa($_, 'GLOB')) { $_ } else { push @buf => $_; () }  } @KEY;
	return wantarray ? @buf : join '' => @buf;
}

## Flush Next: Iterates through @KEY, returning the first valid value, which is deleted from @KEY
sub flush_next ($) {
	local *KEY		= *{ shift() };
	my @buf;
	@KEY			= map { if (UNIVERSAL::isa($_, 'GLOB')) { $_ } else { if (@buf) { $_ } else { push @buf => $_; () } }  } @KEY;
	return wantarray ? @buf : $buf[0];
}

## Flush Last: Iterates through @KEY, returning the last valid value, which is deleted from @KEY
sub flush_last ($) {
	local *KEY		= *{ shift() };
	my @buf;
	@KEY			= reverse map { if (UNIVERSAL::isa($_, 'GLOB')) { $_ } else { if (@buf) { $_ } else { push @buf => $_; () } }  } reverse @KEY;
	return wantarray ? @buf : $buf[0];
}

## Flush_lifo: returns and clears the @KEY buffer, last in first out
sub flush_lifo ($) {
	local *KEY		= *{ shift() };
	my @buf;
	@KEY			= map { if (UNIVERSAL::isa($_, 'GLOB')) { $_ } else { push @buf => $_; () }  } @KEY;
	return reverse @buf;
}

## Flush Trained: returns and clears the @KEY buffer, in increments defined by first optional arg.
## If the arg is an arrayref, its elements are the defaults and the number of its elements
## set the number of @KEY elements returned.
## If the arg is a string matching the form [number] : [default],
## the defaults are [default], and the number sets the number of @KEY elements returned.
## Defaults are provided when elements fail a simple truth test [i.e., not a test for zero vs ''].
sub flush_trained ($;$) {
	local *KEY		= *{ shift() };
	my $pattern		= shift();

	my $num_elems;
	my @defaults;
	if (ref($pattern) =~ /array/i) {
		@defaults	= @{ $pattern };
		$num_elems	= @defaults;
	} elsif ($pattern =~ /^\s*(\d+)\s*(:(.*))?$/) {
		$num_elems	= $1;
		my $def		= defined($3) ? $3 : '';
		@defaults	= ($def) x $num_elems;
	}

	my @buf;
	@KEY			= map { if (UNIVERSAL::isa($_, 'GLOB')) { $_ } else { push @buf => $_; () }  } @KEY;

	my @out;
	if (@buf and @defaults) {
		my @elems	= splice @buf,0,$num_elems;
		@out		= map { $elems[$_] || $defaults[$_] } (0..$num_elems-1);
	}

	push @KEY => @buf;

	return wantarray ? @out : join '' => @out;
}

## Read_fifo: returns the @KEY buffer, first in first out
sub read_fifo ($) {
	local *KEY		= *{ shift() };
	return grep { ! UNIVERSAL::isa($_, 'GLOB') } @KEY;
}

## Read_lifo: returns the @KEY buffer, last in first out
sub read_lifo ($) {
	local *KEY		= *{ shift() };
	return reverse grep { ! UNIVERSAL::isa($_, 'GLOB') } @KEY;
}

## Read_unique: returns the @KEY buffer, unique values only.
## Optional args are added to @KEY, dereferencing any arg found to be an array ref
sub read_unique ($;@) {
	local *KEY		= *{ shift() };
	my %found;
	return map {
		$found{$_}++ ? () : $_ } grep { ! UNIVERSAL::isa($_, 'GLOB')
	} (@KEY, map { ref($_) =~ /ARRAY/ ? @{ $_ } : $_ } @_);
}

## Read_except: returns the @KEY buffer, excluding any values matching
## any optional args, dereferencing any arg found to be an array ref
sub read_except ($;@) {
	local *KEY		= *{ shift() };
	my %exceptions	= map { ($_ => 1) } map { ref($_) =~ /ARRAY/ ? @{ $_ } : $_ } @_;
	return grep { ! ( $exceptions{$_} || UNIVERSAL::isa($_, 'GLOB') ) } @KEY;
}

## Template: Same as replace, except the first optional arg
## is taken as a template name. If no more args are given,
## the name is looked up, first in $KEY{_displays} and then
## in the display directory $KEY{_display_dir}.
## If additional args are given, they are concatenated as
## the template, and nothing is looked up.
## EXCEPTION: A name prepended with '-' will always be looked up in the
## display directory, without the starting '-'. In this case,
## any additional args are concatenated and appended to the template.
##
## The resulting template might be ''.
## The resulting template is added UNPROCESSED to $KEY{_displays},
## replacing any template already there with that name.
## 		XXX If no optional args are given, the internal template $KEY is used,
## 		XXX or the default generic template qq{$KEY{_start}$KEY{_mark} DATA=_mark $KEY{_end}},
## 		XXX and nothing is added to $KEY{_displays}.

## 		XXX The resulting template is then recursively PROCESSED and output. 2013-10-16
##		*** template() NO LONGER PROCESSES - to prevent premature rendering 
sub template ($;@) {
	my $arg1	= shift;

	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	%KEY and exists $KEY{_display_dir}
		or	return '';

	my $tmpl;
	my $tmpl_name = shift || '_X_X';

	$tmpl_name =~ s/^-([a-zA-Z_][\w -]*)$/$1/
		and $tmpl = do {
					local $/;
					my $d;
					(open $d, "<", "$KEY{_display_dir}/$tmpl_name" . ".tmpl"
						and $KEY{_displays}->{$tmpl_name} = join('' => <$d>, @_) )
							or ""}
	  or
	@_
		and $tmpl = $KEY{_displays}->{$tmpl_name} = join('' => @_)
	  or
	$tmpl_name ne '_X_X'
		and $tmpl = $KEY{_displays}->{$tmpl_name} || do {
					local $/;
					my $d;
					(open $d, "<", "$KEY{_display_dir}/$tmpl_name" . ".tmpl"
						and $KEY{_displays}->{$tmpl_name} = <$d> )
							or ""}
	  or
	return '';

	#$tmpl	= _process_tmpl(\*KEY, $tmpl); # **********

	return $tmpl;
}

## Save Template: Same as template(), except the resulting template
## is also saved as a file in the display directory $KEY{_display_dir},
## writing over any existing template of the same name.
##
## The resulting template might be ''.
## The resulting template is added UNPROCESSED to $KEY{_displays},
## replacing any template already there with that name.
## 		XXX The resulting template is then recursively PROCESSED and output.
## 			save_template() NO LONGER PROCESSES - to prevent premature rendering 
sub save_template ($;@) {
	my $arg1	= shift;

	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	%KEY and exists $KEY{_display_dir}
		or	return '';

	my $tmpl;
	my $tmpl_name = shift || '_X_X';

	$tmpl_name =~ s/^-([a-zA-Z_][\w -]*)$/$1/
		and $tmpl = do {
					local $/;
					my $d;
					(open $d, "<", "$KEY{_display_dir}/$tmpl_name" . ".tmpl"
						and $KEY{_displays}->{$tmpl_name} = join('' => <$d>, @_) )
							or ""}
	  or
	@_
		and $tmpl = $KEY{_displays}->{$tmpl_name} = join('' => @_)
	  or
	$tmpl_name ne '_X_X'
		and $tmpl = $KEY{_displays}->{$tmpl_name} || do {
					local $/;
					my $d;
					(open $d, "<", "$KEY{_display_dir}/$tmpl_name" . ".tmpl"
						and $KEY{_displays}->{$tmpl_name} = <$d> )
							or ""}
	  or
	return '';

	open T, ">", "$KEY{_display_dir}/$tmpl_name" . ".tmpl"
		or return "";
	print T $tmpl, "\n";
	close T;

#	$tmpl	= _process_tmpl(\*KEY, $tmpl);	# **********

	return $tmpl;
}

## Message: Same as template, with the first optional argument taken as a
## message name. If no more args are given,
## the name is looked up, first in $KEY{_messages} and then
## in the message file $KEY{_message_file}.
## If additional args are given, they are concatenated as
## the message, and nothing is looked up.
## EXCEPTION: A name prepended with '-' will always be looked up in the
## message file, without the starting '-'. In this case,
## any additional args are concatenated and appended to the message.
##
## The resulting message might be ''.
## The resulting message is added UNPROCESSED to $KEY{_messages},
## replacing any message already there with that name.
## The resulting message is then recursively PROCESSED and output.

sub message ($;@) {
	my $arg1	= shift;

	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	%KEY or return '';

	my $tmpl_name = shift
			or return '';

	my $tmpl = '';

	$tmpl_name =~ s/^-([a-zA-Z_][\w -]*)$/$1/
		and $tmpl = $KEY{_messages}->{$tmpl_name} = do {
					local $/ = '';
					my $m	 = '';
					if (open M, "<", "$KEY{_message_file}") {
						while (<M>) {
							next unless /^$tmpl_name\n/;
							chomp;
							$m = join('' => (split "\n" => $_, 2)[1], @_);
							last
						}
						close M;
					}
					$m}
	  or
	@_
		and $tmpl = $KEY{_messages}->{$tmpl_name} = join('' => @_)
	  or
	$tmpl_name
		and $tmpl = $KEY{_messages}->{$tmpl_name} ||= do {
					local $/ = '';
					my $m	 = '';
					if (open M, "<", "$KEY{_message_file}") {
						while (<M>) {
							next unless /^$tmpl_name\s*\n/;
							chomp;
							$m = (split "\n" => $_, 2)[1]
						}
						close M;
					}
					$m}
	  or
	return '';

	$tmpl	= _process_tmpl(\*KEY, $tmpl);

	return $tmpl;
}

## Save_Message: Same as template, with the first optional argument taken as a
## message name. If no more args are given,
## the name is looked up, first in $KEY{_messages} and then
## in the message file $KEY{_message_file}.
## If additional args are given, they are concatenated as
## the message, and nothing is looked up.
## EXCEPTION: A name prepended with '-' will always be looked up in the
## message file, without the starting '-'. In this case,
## any additional args are concatenated and appended to the message.
##
## The resulting message might be ''.
## The resulting message is added UNPROCESSED to $KEY{_messages},
## replacing any message already there with that name.
## The resulting message is also saved in the message file $KEY{_message_file},
## replacing any message already there with that name.
## The resulting message is then recursively PROCESSED and output.

sub save_message ($;@) {
	my $arg1	= shift;

	local *KEY	= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	%KEY or	return '';

	my $tmpl_name = shift
			or	return '';

	my $tmpl	= '';

	$tmpl_name	=~ s/^-([a-zA-Z_][\w -]*)$/$1/
		and $tmpl	= $KEY{_messages}->{$tmpl_name} = do {
					local $/ = '';
					my $m	 = '';
					if (open M, "<", "$KEY{_message_file}") {
						while (<M>) {
							next unless /^$tmpl_name\n/;
							chomp;
							$m = join('' => (split "\n" => $_, 2)[1], @_);
							last
						}
						close M;
					}
					$m}
	  or
	@_
		and $tmpl = $KEY{_messages}->{$tmpl_name} = join('' => @_)
	  or
	$tmpl_name
		and $tmpl = $KEY{_messages}->{$tmpl_name} ||= do {
					local $/ = '';
					my $m	 = '';
					if (open M, "<", "$KEY{_message_file}") {
						while (<M>) {
							next unless /^$tmpl_name\s*\n/;
							chomp;
							$m = (split "\n" => $_, 2)[1]
						}
						close M;
					}
					$m}
	  or
	return '';


	my $printed		= 0;
	my $msg			= $tmpl;
	#$msg			=~ s{(\015?\012)|\015}{\n}gs;
	REPLACE: {
		local $^I 		= '';
		local @ARGV		= ($KEY{_message_file});
		local $/		= '';
		MSG: while (<>) {
			unless (/^$tmpl_name\s*\n/) {
				print;
				next MSG;
			}
			print $tmpl_name, "\n", $msg, "\n\n";
			$printed++;
		}
	}
	ADD: {
		unless ($printed) {
			open M, ">>", $KEY{_message_file};
			print M "\n\n", $tmpl_name, "\n", $msg, "\n";
			close M;
 		}
	}

	$tmpl	= _process_tmpl(\*KEY, $tmpl);

	return $tmpl;
}


## List_items: Same as template, with the first optional argument taken as a
## list name. If no more args are given,
## the name is looked up, first in $KEY{_lists} and then
## in the list file $KEY{_list_file}.
## If additional args are given, they are made into
## the list, and nothing is looked up.
## EXCEPTION: A name prepended with '-' will always be looked up in the
## list file, without the starting '-'. In this case,
## any additional args are appended to the list from the file.
##
## The resulting list might be empty ().
## The resulting list is added UNPROCESSED to $KEY{_lists},
## replacing any list already there with that name.
## If no optional args are given, the ordered elements of
## internal buffer @KEY are used as an anonymous list.
## The resulting list items are then output UNPROCESSED
## as an array ref.

sub list_items ($;@) {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	%KEY or	return '';

	my @list;

	my $list_name = shift;
	unless ($list_name) {
		@list	= grep { ! UNIVERSAL::isa($_, 'GLOB') } @KEY;
		return [ @list ];
	}

	my @supplemental_items	= ref($_[0]) ? @{ $_[0] } : @_;

	if ($list_name =~ s/^-([a-zA-Z][-\w ]*)$/$1/) {
		unless (@list	= $obj->_list_from_file($list_name)) {
			unless (@list	= $obj->_list_from_dir($list_name)) {
				return;
			}
		}
		push @list => @supplemental_items;
	} elsif (@supplemental_items) {
		@list	=  @supplemental_items;
	} else {
		if (exists $KEY{_lists}->{$list_name}) {
			@list	= @{ $KEY{_lists}->{$list_name} };
		} else {
			unless (@list	= $obj->_list_from_file($list_name)) {
				unless (@list	= $obj->_list_from_dir($list_name)) {
					return;
				}
			}
		}
	}

	$KEY{_lists}->{$list_name} = [ @list ];

	return wantarray ? @list : [ @list ];
}

## Save_List_items: Same as template, with the first optional argument taken as a
## list name. If no more args are given,
## the name is looked up, first in $KEY{_lists} and then
## in the list file $KEY{_list_file}.
## If additional args are given, they are made into
## the list, and nothing is looked up.
## EXCEPTION: A name prepended with '-' will always be looked up in the
## list file, without the starting '-'. In this case,
## any additional args are appended to the list.
##
## The resulting list might be empty ().
## The resulting list is added UNPROCESSED to $KEY{_lists},
## replacing any list already there with that name.
## The resulting list is also saved in the list file $KEY{_list_file},
## replacing any list already there with that name.
## If no optional args are given, no list is saved
## and the resulting list is empty ().
## The resulting list items are then output UNPROCESSED
## as an array ref.

sub save_list_items_old ($;@) {
	my $arg1	= shift;

	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	%KEY or	return [];

	my $tmpl_name = shift
		or return [];

	my $tmpl = '';

	if ($tmpl_name =~ s/^-([a-zA-Z][-\w ]*)$/$1/) {
		local $/ 	= '';
		my @items	= ();
		if (open M, "<", "$KEY{_list_file}") {
			while (<M>) {
				next unless /^$tmpl_name\s*\n/;
				chomp;
				($tmpl_name, @items) = split "\n" => $_;
				last
			}
			close M;
		}
		$tmpl	= join("\n" =>  @items, @_);
	} elsif (@_) {
		$tmpl	= join("\n" => @_)
	} elsif (exists $KEY{_lists}->{$tmpl_name}) {
		$tmpl	= join("\n" => @{ $KEY{_lists}->{$tmpl_name} } );
	} else {
		local $/ 	= '';
		my @items	= ();
		if (open M, "<", "$KEY{_list_file}") {
			while (<M>) {
				next unless /^$tmpl_name\s*\n/;
				chomp;
				($tmpl_name, @items) = split "\n" => $_;
				last
			}
			close M;
		}
		$tmpl	= join("\n" =>  @items, @_);
	}

	my $printed			= 0;
	REPLACE: {
		local $^I 			= '';
		local @ARGV			= ($KEY{_list_file});
		local $/			= '';
		LIST: while (<>) {
			unless (/^$tmpl_name\s*\n/) {
				print;
				next LIST;
			}
			print $tmpl_name, "\n", $tmpl, "\n\n";
			$printed++;
		}
	}
	ADD: {
		unless ($printed) {
			open M, ">>", $KEY{_list_file};
			print M "\n\n", $tmpl_name, "\n", $tmpl, "\n";
			close M;
		}
	}

	$tmpl	= _process_tmpl(\*KEY, $tmpl);

	$KEY{_lists}->{$tmpl_name} = [ split "\n" => $tmpl ];
}

sub save_list_items ($;@) { # same as save_list; kept for legacy
	my $obj	= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	%KEY or	return [];

	my $list_name = shift
		or return [];

	my @items;
	my @supplemental_items	= ref($_[0]) ? @{ $_[0] } : @_;
	my $use_default_list	= 0;

	if ($list_name =~ s/^-([a-zA-Z][-\w ]*)$/$1/) {
		unless (@items	= $obj->_list_from_file($list_name)) {
			@items	= $obj->_list_from_dir($list_name);
		}
		$use_default_list++ if @items;
		push @items => @supplemental_items;
	}
	unless ($use_default_list) {
		if (@supplemental_items) {
			@items	= @supplemental_items;
		} elsif (exists $KEY{_lists}->{$list_name}) {
			@items	= @{ $KEY{_lists}->{$list_name} };
		} else {
			unless (@items	= $obj->_list_from_dir($list_name)) {
				@items	= $obj->_list_from_file($list_name);
			}
		}
	}

	my $list_pathname	= "$KEY{_list_dir}/$list_name";
	return [] unless open my $lfh, ">", $list_pathname;
	print $lfh join "\n" => @items, "";
	close $lfh;

	$KEY{_lists}->{$list_name} = [ @items ];
}

sub save_list ($;@) {
	my $obj	= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	%KEY or	return [];

	my $list_name = shift
		or return [];

	my @items;
	my @supplemental_items	= ref($_[0]) ? @{ $_[0] } : @_;
	my $use_default_list	= 0;

	if ($list_name =~ s/^-([a-zA-Z][-\w ]*)$/$1/) {
		unless (@items	= $obj->_list_from_file($list_name)) {
			@items	= $obj->_list_from_dir($list_name);
		}
		$use_default_list++ if @items;
		push @items => @supplemental_items;
	}
	unless ($use_default_list) {
		if (@supplemental_items) {
			@items	= @supplemental_items;
		} elsif (exists $KEY{_lists}->{$list_name}) {
			@items	= @{ $KEY{_lists}->{$list_name} };
		} else {
			unless (@items	= $obj->_list_from_dir($list_name)) {
				@items	= $obj->_list_from_file($list_name);
			}
		}
	}

	my $list_pathname	= "$KEY{_list_dir}/$list_name";
	return [] unless open my $lfh, ">", $list_pathname;
	print $lfh join "\n" => @items, "";
	close $lfh;

	$KEY{_lists}->{$list_name} = [ @items ];
}

sub save_sys_list ($;@) {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	%KEY or	return [];

	my $list_name = shift
		or return [];

	my @items;
	my $use_default_list	= 0;

	if ($list_name =~ s/^-([a-zA-Z][-\w ]*)$/$1/) {
		@items	= $obj->_list_from_file($list_name);
		$use_default_list++ if @items;
		push @items => @_;
	}
	unless ($use_default_list) {
		if (@_) {
			@items	= @_;
		} elsif (exists $KEY{_lists}->{$list_name}) {
			@items	= @{ $KEY{_lists}->{$list_name} };
		} else {
			@items	= $obj->_list_from_file($list_name);
			push @items => @_;
		}
	}
	@items	=  map { /\A([^`]+)\z/ ? $1 : ()  } @items;

	my $printed		= 0;
	REPLACE: {
		local $^I 		= '';
		local @ARGV		= ($KEY{_list_file});
		local $/		= '';
		LIST: while (<>) {
			unless (/^$list_name\s*\n/) {
				print;
				next LIST;
			}
			print $list_name, "\n", join("\n" => @items), "\n\n";
			$printed++;
		}
	}
	ADD: {
		unless ($printed) {
			open my $M, ">>", $KEY{_list_file};
			print $M "\n\n", $list_name, "\n", join("\n" => @items), "\n";
			close $M;
		}
	}

	my $list_dir	= $KEY{_list_dir} || '';
	$list_dir		= $list_dir eq "./" ? "./lists" : $list_dir;
	unless (-d $list_dir) {
		mkdir($list_dir, 0777) or die "List dir creation failed for $list_dir: $!";
	}

	my $list_pathname	= "$list_dir/$list_name";
	return [] unless open my $lfh, ">", $list_pathname;
	print $lfh join "\n" => @items, "";
	close $lfh;

	$KEY{_lists}->{$list_name} = [ @items ];
}

sub _list_from_file {
	local *KEY		= shift;
	my $list_name	= shift;
	my %flist;
	if (open my $LF, "<", "$KEY{_list_file}") {
		local $/	= '';
		LIST: while (my $lff = <$LF>) {
			next LIST unless /\A$list_name\s*\n/;
			$flist{$list_name}	= $lff;
			last LIST;
		}
		close $LF;
	}
	return unless exists $flist{$list_name};
	my @items	= split "\n" => $flist{$list_name};
	my @list	= map { defined && /\A\s*([^`]+)\s*\z/ ? $1 : () } splice(@items, 1);
	return wantarray ? @list : [ @list ];
}

sub _list_from_dir {
	local *KEY		= shift;
	my $list_name	= shift;
	my $list		= '';
	my $list_dir	= $KEY{_list_dir} || '';
	$list_dir		= $list_dir eq "./" ? "./lists" : $list_dir;
	return unless -d $list_dir;
	my $list_pathname	= "$list_dir/$list_name";
	return unless -e $list_pathname;
	return unless open my $lfh, "<", $list_pathname;
	my @items = <$lfh>;
	close $lfh;
	chomp @items;
	my @list	= map { defined && /\A\s*([^`]+)\s*\z/ ? $1 : () } @items;
	return wantarray ? @list : [ @list ];
}

## Num_list_items: Same as list_items, with the first optional argument taken as a
## list name. If no more args are given,
## the name is looked up, first in $KEY{_lists} and then
## in the list file $KEY{_list_file}.
## If additional args are given, they are made into
## the list, and nothing is looked up.
## EXCEPTION: A name prepended with '-' will always be looked up in the
## list file, without the starting '-'. In this case,
## any additional args are appended to the list.
##
## The resulting list might be empty ().
## The resulting list is added UNPROCESSED to $KEY{_lists},
## replacing any list already there with that name.
## If no optional args are given, the ordered elements of
## internal buffer @KEY are used as an anonymous list.
## The resulting list items are NOT PROCESSED.
## The NUMBER of items in the list is returned.

sub num_list_items ($;@) {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	%KEY or	return '';

	my @list;

	my $list_name = shift;
	unless ($list_name) {
		@list	= grep { ! UNIVERSAL::isa($_, 'GLOB') } @KEY;
		return scalar @list;
	}

	if ($list_name =~ s/^-([a-zA-Z][-\w ]*)$/$1/) {
		unless (@list	= $obj->_list_from_file($list_name)) {
			@list	= $obj->_list_from_dir($list_name);
		}
		push @list => @_;
	} elsif (@_) {
		@list	=  @_;
	} else {
		if (exists $KEY{_lists}->{$list_name}) {
			@list	= @{ $KEY{_lists}->{$list_name} };
		} else {
			unless (@list	= $obj->_list_from_file($list_name)) {
				@list	= $obj->_list_from_dir($list_name);
			}
		}
	}

	$KEY{_lists}->{$list_name}	= [ @list ];

	return scalar @list;
}


## Store: Same as replace, plus all output is written to the filehandle KEY.
##		If no file was specified in the initialization of the object,
##		or attached with attach_file() after initialization,
##		store() will write to a temporary file if called.
##		retrieve() returns the contents of the file, and empties the file.
##		recall() returns the contents of the file.
##		File may be written to (appending) directly by printing to the filehandle KEY,
##		but it's better to use store().
##		File may be read directly from the filehandle KEY using <KEY>, but
##		it's better to use recall().

sub store ($;@) {
	my $arg1	= shift;
	local *KEY	= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	defined *KEY{IO}
		or $KEY{_file} = "/tmp/$KEY{_mark}.txt"
			and open KEY, "+>>", "$KEY{_file}"
				or return qq{};

	my $tmpl		=  join('' => @_)
						|| $KEY
							|| $KEY{_default_tmpl};

	$tmpl	= _process_tmpl(\*KEY, $tmpl);

	print KEY $tmpl;

	$tmpl;
}

## Store data: Appends data in %KEY to the attached file
## as a stringified hash. If arguments are provided, they are used as
## field names; otherwise all fields are used. If a field name is not
## in the %KEY hash, an empty string is stored. Only stores main data
## with scalar values or array/hash references; skips meta-data (fields starting with '_'), fields that
## contain non-ARRAY/HASH references, and the object's KEY field (holding marked input).
## Does not process data, except to provide '' for empty non-zero field values.
## Returns a hash of the stored values.
sub store_data ($;@) {
	my $arg1	= shift;
	local *KEY	= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	defined *KEY{IO}
		or $KEY{_file} = "/tmp/$KEY{_mark}.txt"
			and open KEY, "+>>", "$KEY{_file}"
				or return qq{};

	return unless %KEY;

	my %_KEY;
	for my $key (@_ ? @_ : sort keys %KEY) {
		next if $key eq $KEY{_mark};
		next if $key =~ /^_/;
		next if ref($KEY{$key}) =~ /(CODE|GLOB)/;

		$_KEY{$key}	= defined($KEY{$key}) ? $KEY{$key} : '';
	}

	require Data::Dumper;
	$Data::Dumper::Purity	= 1;
#	$Data::Dumper::Deparse = 1;
	$Data::Dumper::Sortkeys	= 1;
	print KEY Data::Dumper->Dump([\%_KEY], [qw/*_KEY/]);
	print KEY "\n#=\n";

	wantarray ? %_KEY : \%_KEY;
}

## Store JSON: Appends data in %KEY to the attached file
## as a stringified JSON object. If arguments are provided, they are used as
## field names; otherwise all fields are used. If a field name is not
## in the %KEY hash, an empty string is stored. Only stores main data
## with scalar values or array/hash references; skips meta-data (fields starting with '_'), fields that
## contain non-ARRAY/HASH references, and the object's KEY field (holding marked input).
## Does not process data, except to provide '' for empty/false non-zero field values.
## Returns a hash of the stored values.
sub store_json ($;@) {
	my $arg1	= shift;
	local *KEY	= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	defined *KEY{IO}
		or $KEY{_file} = "/tmp/$KEY{_mark}.txt"
			and open KEY, "+>>", "$KEY{_file}"
				or return qq{};

	return unless %KEY;

	my %_KEY;
	for my $key (@_ ? @_ : sort keys %KEY) {
		next if $key eq $KEY{_mark};
		next if $key =~ /^_/;
		next if ref($KEY{$key}) =~ /(CODE|GLOB)/;

		$_KEY{$key}	= defined($KEY{$key}) ? $KEY{$key} : '';
	}

	require Data::Dumper;
	$Data::Dumper::Purity	= 1;
	$Data::Dumper::Deepcopy	= 1;
	$Data::Dumper::Pair		= ' : ';
	print KEY Data::Dumper->Dump([\%_KEY], [qw/*_KEY/]);
	print KEY "\n#=\n";

	wantarray ? %_KEY : \%_KEY;
}

## Store META: Appends data in %KEY to the attached file
## as a serialized hash object. If arguments are provided, they are used as
## field names; otherwise all fields are used. If a field name is not
## in the %KEY hash, an empty string is stored. Only stores meta data
## (fields starting with '_'); skips fields that contain CODE or GLOB references,
## and the object's KEY field (holding marked input).
## Does not process data, except to provide '' for empty non-zero field values.
## Returns a hash of the stored values.
sub store_meta ($;@) {
	my $arg1	= shift;
	local *KEY	= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	defined *KEY{IO}
		or $KEY{_file} = "/tmp/$KEY{_mark}.txt"
			and open KEY, "+>>", "$KEY{_file}"
				or return qq{};

	return unless %KEY;

	my %_META;
	for my $key (@_ ? @_ : sort keys %KEY) {
		next if $key eq $KEY{_mark};
		next unless $key =~ /^_/;
		next if ref($KEY{$key}) =~ /(CODE|GLOB)/;

		$_META{$key}	= defined($KEY{$key}) ? $KEY{$key} : '';
	}

	require Data::Dumper;
	$Data::Dumper::Purity	= 1;
	$Data::Dumper::Deepcopy	= 1;
	$Data::Dumper::Pair		= ' => ';  # ' : '
	print KEY Data::Dumper->Dump([\%_META], [qw/*_META/]);
	print KEY "\n#=\n";

	wantarray ? %_META : \%_META;
}

## Restore data: Reads back any data stored in the attached file.
## Data read back is eval()-ed on the assumption that it is a
## serialized hash named %_KEY; anything else in the file is ignored, EXCEPT
## that any statement must eval() OK, including 'strict' requirements.
## The hash is then added to the existing %KEY, over-writing fields with
## the same names. Allows multiple serialized hashes to be read in, in the order
## they were stored, resulting in the latest values for fields stored more than once.
## Does not process data; does not alter the file.
## Returns a hash of the restored values.
sub restore_data {
	my $arg1	= shift;
	local *KEY	= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	local $/	= "\n#=\n";

	my $old_tell;
	defined *KEY{IO}
		and $old_tell = tell KEY
			and seek(KEY, 0, 0)
				or return;

	my %_KEY;
	my %_KEYS;

	while (<KEY>) {
		next unless /^([^`]+)$/;
		my $d	= $1;
		next unless eval $d;
		$_KEYS{$_}++ for keys %_KEY;
		*KEY	= { %KEY, %_KEY };
	}
	seek KEY, $old_tell, 0;

	wantarray ? map { $_ => $KEY{$_} || '' } keys %_KEYS : { map { $_ => $KEY{$_} || '' } keys %_KEYS };
}

## retrieve_data, same as restore_data, 
## except it also empties the file, like retrieve()
sub retrieve_data {
	my $arg1	= shift;
	local *KEY	= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };

	local $/	= "\n#=\n";

	defined *KEY{IO}
		and seek(KEY, 0, 0)
			or return;

	my %_KEY;
	my %_KEYS;

	while (<KEY>) {
		next unless /^([^`]+)$/;
		my $d	= $1;
		next unless eval $d;
		$_KEYS{$_}++ for keys %_KEY;
		*KEY	= { %KEY, %_KEY };
	}

	truncate(KEY,0);

	wantarray ? map { $_ => $KEY{$_} || '' } keys %_KEYS : { map { $_ => $KEY{$_} || '' } keys %_KEYS };
}


## Retrieve: Analogous to flush; reads and returns contents of file
## to which store() has written, and empties the file.
sub retrieve ($) {
 	local *KEY		= *{ shift() };

	local	$/;

	defined *KEY{IO}
		and seek(KEY, 0, 0)
			or return qq{};

	my $retr = <KEY>;
	truncate(KEY,0);
	$retr
}

## Recall: reads the file to which store()
## has written, and returns an array of
## its lines in list context, or its first line
## in scalar context, and leaves the file unchanged.
## A non-zero value in the first optional arg reverses the list order
## and causes the LAST line to be returned in scalar context.
## "Lines" are chunks of the file terminated by the
## second optional arg, if any, or Perl's $/, usually \n. The
## second arg may be any string; it's chomped off.
sub recall ($;@) {
	local *KEY		= *{ shift() };

	my $reverse		= shift() ? 1 : 0;

	local $/		= shift() || $/;  #

	my $old_tell;
	defined *KEY{IO}
		and $old_tell = tell KEY
			and seek(KEY, 0, 0)
				or return;

	my @lines = <KEY>;

	seek KEY, $old_tell, 0;

	chomp @lines;

	@lines		= map { /^([^`]+)$/ ? $1 : () } @lines;

	@lines = reverse @lines if $reverse;

	wantarray ? @lines : $lines[0]
}

## Recall_match: reads the file to which store()
## has written line by line, and returns an array of
## the lines matching the first optional arg in list context, or
## the first matching line in scalar context, and leaves the file unchanged.
## The first arg may be any string or regular expression.
## A non-zero value in the second arg reverses the list order
## and causes the LAST matching line to be returned in scalar context.
## "Lines" are chunks of the file terminated by the
## third arg, if any, or Perl's $/, usually \n. The
## third arg may be any string; it's chomped off.
## A non-zero value in the fourth arg causes the byte number of
## each match to be returned; this may be used by recall_seek().
sub recall_match ($$;@) {
	local *KEY		= *{ shift() };

	my $match		= shift() or return wantarray ? () : '';

	my $reverse		= shift() ? 1 : 0;

	local $/		= shift() || $/;

	my $want_byte	= shift() ? 1 : 0;

	my $start_tell;
	defined *KEY{IO}
		and $start_tell = tell KEY
			and seek(KEY, 0, 0)
				or return wantarray ? () : '';

	my %matches;
	my $tell		= 0;
	my $byte_key	= 0;
	while (my $line = <KEY>) {
		$byte_key	= $tell;
		$tell	= tell(KEY);
		next unless $line =~ /$match/;
		chomp $line;
		$matches{$byte_key} = $line;
	}
	seek KEY, $start_tell, 0;

	my @keys		= $reverse ?
						reverse sort keys %matches :
							sort keys %matches;

	if ($want_byte) {
		wantarray ? map { $_ => $matches{$_} } @keys : $keys[0]
	} else {
		wantarray ? @matches{@keys} : $matches{$keys[0]}
	}
}

## Recall_seek: reads the file to which store()
## has written,
sub recall_seek ($$;@) {
	local *KEY		= *{ shift() };

	my $tell		= shift() || 0;

	my $index		= shift() ? -2 : 0;

	local $/		= shift() || $/;

	my $old_tell;

	defined *KEY{IO}
		and $old_tell = tell KEY
			and seek(KEY, 0, 0)
				or return wantarray ? () : '';

	my @matches;
	for my $t ( ref($tell) ? @{ $tell } : ($tell) ) {
		seek KEY, $t, 0;
		my $line		=  <KEY>;
		push @matches => $t, $line;
	}

	chomp @matches;
	seek KEY, $old_tell, 0;

	wantarray ? @matches : $matches[$index]
}

## Form: designates the form (template) against which replace() etc. operate
## and returns the form UNPROCESSED. Accepts '' as a form.
## If no form argument is provided, returns the current form or ''.
## Copies the form to $KEY.
## A form may be assigned directly to $KEY as a string.
## The current form may be read directly from $KEY.
## Note: $KEY is left unchanged when forms are passed as arguments to replace() etc.

sub form ($;@) {
	local *KEY		= *{ shift() };
	return ($KEY || '') unless defined($_[0]);
	{
	local $_ = ref($_[0]);
	/HASH/		and $KEY	= $_[0]->{_form} || $_[0]->{_tmpl} || ''
		or
	/ARRAY/		and $KEY	= join '' => '', @{ $_[0] } || ('')
		or
	/CODE/		and $KEY	= join '' => '', @{ $_[0]->() } || ('')
		or
					$KEY	= join '' => '', @_;
	}
	$KEY
}

## Charge: safely adds data to %KEY, and returns updated %KEY.
## Over-writes existing fields with the same names, but doesn't
## accept any data from fields whose names start with '_'.
sub charge ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%KEY		= ( %KEY, map { /^_/ ? () : ($_ => $NEW{$_}) } keys %NEW );
}


## Charge Chomped: safely adds data to %KEY, and returns updated %KEY.
## Over-writes existing fields with the same names, but doesn't
## accept any data from fields whose names start with '_'.
## Removes system end-of-line
sub charge_chomped ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%KEY		= ( %KEY, map { /^_/ ? () : ( $_ => $NEW{$_}) } map { chomp $NEW{$_}; $_ } keys %NEW );
}


## Charge_meta:  safely adds meta-data to %KEY, and returns updated %KEY.
## Similar to charge, but only accepts data from fields whose names start with '_'.
## Over-writes existing fields with same names.
sub charge_meta ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%KEY		= ( %KEY, map { /^_/ ? ($_ => $NEW{$_}) : () } keys %NEW );
}

## Charge_as_meta:  safely adds meta-data to %KEY, and returns updated %KEY.
## Similar to charge_meta, but accepts data from fields with any names.
## Adds '_' to the start of field names that don't already have it.
## Over-writes existing fields with same names.
sub charge_as_meta ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%KEY		= ( %KEY, map { /^_/ ? ($_ => $NEW{$_}) : ("_$_" => $NEW{$_}) } keys %NEW );
}

## Charge_all: safely adds data and meta-data to %KEY, and returns updated %KEY.
## Over-writes existing fields with the same names.
sub charge_all ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%KEY		= ( %KEY, %NEW );
}

## Charge_these: safely adds data and meta-data to specified fields of %KEY, 
## and returns updated %KEY. The first arg is a comma-separated string or 
## an arrayref listing the fields to be changed.
## The following args are the values, in order of the listed fields.
## Over-writes existing fields with the same names.
sub charge_these ($;@) {
	local *KEY		= *{ shift() };
	my $flds		= shift();
	my @flds		= ref($flds) ? @{ $flds } : split( /\s*,\s*/ => $flds);
	my %NEW			= map { $_ => shift() } @flds;

	%KEY		= ( %KEY, %NEW );
}

## Charge_auto: executes anonymous sub stored in $KEY{_auto}, and returns updated %KEY.
## $KEY{_auto} may hold any series of actions, usually including charge methods.
## Executed for each UI object by render() just before rendering.
sub charge_auto ($;@) {
	local *KEY		= *{ shift() };
	$KEY{_auto}
		and $KEY{_auto}->();

	%KEY
}

## Calculate: executes code references stored in %KEY, and returns updated %KEY.
## Coderefs may be called by name as optional args; if no optional args are provided, 
## all fields in %KEY with names starting with a hyphen '-' will be called, sorted alphabetically.
## Coderefs stored in fields whose names start with a hyphen '-' return their results
## to fields of the same name without the starting hyphen, replacing any value previously there.
## E.g.: A calculation stored in the field -fullname will put its result in the field fullname.
## A calculation stored in a field named without the starting hyphen will put its
## result in a field of the same name with '_out' appended, replacing any value previously there.
## If the value of the field is not a coderef, the value itself is returned.
## Coderefs may not alter values in fields other than the fields to which they return their results.
## The coderef is passed an anonymous UI object charged with data from %KEY.
## Circular references:
## Forced context: 
## Order of execution:
##
## Examples:
# 	$ui->charge(-system_status => sub {
# 		my $obj	= shift;
# 		$obj->charge(test_mode_status => ($obj->data('test_mode') ? "Test Mode On" : "Test Mode Off"));
# 		return $obj->resolve('[:title]: Now at [:state] - [:test_mode_status]');
# 	});
# 	
# 	$ui->charge(-admin_test_checkbox => sub {
# 		my $obj	= shift;
# 		my ($ses_obj)	= $obj->objects('SES');
# 		if ($ses_obj->data('group') eq 'admin' and $obj->data('test_mode')) {
#  			return $ui->process(qq{INPUT=do_test,,c:2,,Test});
# 		} else {
# 			return "";
# 		}	
# 	});
#
## Calculate() is called by render() for each UI object's '-' flds just before rendering, but after 
## charge_auto has been called for that object.
sub calculate ($;@) {
	my $obj		= shift;
	local *KEY	= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my @flds	= @_ ? @_ : sort grep { /^-./ } keys %KEY;
	my $ui_obj	= $obj->direct();
	for my $fld (@flds) {
		# charge with updated COPY of %KEY each time
		$ui_obj->charge( { %KEY } );
		my $result;
		if (ref($KEY{$fld}) =~ /CODE/) {
			$result	= $KEY{$fld}->($ui_obj)
		} else {
			# don't process, but see charge_as_calc()
			$result	= $KEY{$fld} || '';
		}
		if ($fld =~ /^-(.+)$/) {
			$KEY{$1}	= $result;
		} else {
			$KEY{$fld . '_out'}	= $result;
		}
		$ui_obj->clear();
	}
	%KEY;
}

sub calculated_data ($;@) {
	my $obj		= shift;
	local *KEY	= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my @flds	= @_ ? @_ : sort grep { /^-./ } keys %KEY;
	my $ui_obj	= $obj->direct();
	my @calcflds;
	for my $fld (@flds) {
		# charge with updated COPY of %KEY each time
		$ui_obj->charge( { %KEY } );
		my $result;
		if (ref($KEY{$fld}) =~ /CODE/) {
			$result	= $KEY{$fld}->($ui_obj);
		} elsif (ref($KEY{"-$fld"}) =~ /CODE/) {
			$result	= $KEY{"-$fld"}->($ui_obj);
		} else {
			# don't process, but see charge_as_calc()
			$result	= $KEY{$fld} || '';
		}
		if ($fld =~ /^-(.+)$/) {
			$KEY{$1}	= $result;
			push @calcflds => $1;
		} else {
			$KEY{$fld . '_out'}	= $result;
			push @calcflds => $fld . '_out';
		}
		$ui_obj->clear();
	}
	return @KEY{@calcflds};
}

## Charge_as_calc:  safely adds calculating fields to %KEY, and returns updated %KEY.
## Expects subroutine references; if the new value is not a coderef, wraps it in an
## anonymous subroutine that returns the value, dereferencing it and stringifying
## it according to the type of reference.
## Adds '-' to the start of field names that don't already have it.
## Over-writes existing fields with same names (including the hyphen).
## The result of a calculation is put in the corresponding data field
## named without the starting '-'.
sub charge_as_calc ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	my $new_type	= ref($_[0]);
	if ($new_type 		=~ /HASH/) {
		%NEW	= %{ $_[0] };
	} 
	elsif ($new_type	=~ /ARRAY/) {
		%NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
	} 
	elsif ($new_type	=~ /CODE/) {
		%NEW	= %{ $_[0]->() }
	} 
	else {
		%NEW	= @_ % 2 ? (@_, '') : @_
	}
	
	for my $k (keys %NEW) {
		my $c	= $NEW{$k};
		$k		=~ s/^-?(.*)$/$1/;
		if (ref($c) =~ /CODE/) {
			$KEY{"-$k"} = $c;
		} 
		elsif (ref($c) =~ /SCALAR/) {
			$KEY{"-$k"} = sub { return ${ $c } };
		} 
		elsif (ref($c) =~ /ARRAY/) {
			$KEY{"-$k"} = sub { return join " " => @{ $c } };
		} 
		elsif (ref($c) =~ /HASH/) {
			# sort keys here so a given input always comes out the same
			$KEY{"-$k"} = sub { return join ", " => map { uc($_) . ": $c->{$_}" } sort keys %{ $c }; };
		} 
		else {
			$KEY{"-$k"} = sub { return $c; };		
		}
	}
	%KEY
}


## Charge_msg: safely stores a message in the internal subroutine $KEY{_msg},
## creating $KEY{_msg} if necessary with make_msg().
## The first optional arg is a string or a hash reference, following the
## requirements of make_msg
sub charge_msg ($;@) {
	local *KEY		= *{ shift() };
	my $arg			= shift;
	$KEY{_msg}		||= make_msg(\*KEY);

	$KEY{_msg}->($arg);

	%KEY
}

## Charge_err: safely stores a message in the internal subroutine $KEY{_err},
## creating $KEY{_err} if necessary with make_msg().
sub charge_err ($;@) {
	local *KEY		= *{ shift() };
	my $arg			= shift;
	$KEY{_err}		||= make_msg(\*KEY);

	$KEY{_err}->($arg);

	%KEY
}

## Charge_xor: safely adds data and meta-data to %KEY, and returns updated %KEY.
## Over-writes existing fields with the same names only in fields evaluating to false.
sub charge_xor ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%KEY		= ( %KEY, ( map { $KEY{$_} ? () : $_ => $NEW{$_} } keys %NEW ) );
}

## Charge_or: safely adds data and meta-data to %KEY, and returns updated %KEY.
## Does not over-write existing fields with the same names.
sub charge_or ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%KEY		= ( %KEY, ( map { exists $KEY{$_} ? () : $_ => $NEW{$_}  } keys %NEW ) );
}

## Charge_true: safely adds data and meta-data to %KEY, and returns updated %KEY.
## Over-writes existing fields with the same names only if the new data evaluates to 'true'.
sub charge_true ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}
																		# special case for null dates
	%KEY		= ( %KEY, ( map { $_ => $NEW{$_}  } grep { $NEW{$_} and $NEW{$_} !~ /0000-00-00/ } keys %NEW ) );
}

## Charge_marked:  safely adds marked input data to %KEY, and returns updated %KEY.
## Similar to charge, but only accepts data from fields whose
## names start with 'MARK:' or 'MARK_', or end with '_MARK' where MARK is $KEY{_mark} (which is 'KEY').
## If no argument is given, inserts marked input data stored in $KEY{$KEY{_mark}} (same as $KEY{_vals}).
## Over-writes existing fields with same names (with the mark stripped off).
sub charge_marked ($;@) {
	local *KEY		= *{ shift() };
	unless ( @_ ) {
		*KEY	= { %KEY, %{ $KEY{$KEY{_mark}} || {} } };
		return %KEY
	}

	my @flds	= ref($_[0]) ? @{ $_[0] } : @_;
	my %NEW;
	for (@flds) {
		exists $KEY{_input}->{"$KEY{_mark}:$_"}
			and do { $NEW{$_}	= $KEY{_input}->{"$KEY{_mark}:$_"}; 1 }
		or exists $KEY{_input}->{"$KEY{_mark}_$_"}
			and do { $NEW{$_}	= $KEY{_input}->{"$KEY{_mark}_$_"}; 1 }
		or exists $KEY{_input}->{"${_}_$KEY{_mark}"}
			and do { $NEW{$_}	= $KEY{_input}->{"${_}_$KEY{_mark}"}; 1 };
	}

 	%KEY	= ( %KEY, %NEW );
#	*KEY	= { %KEY, %NEW };
}

## Charge from input: safely adds input data to %KEY, and returns updated %KEY.
## Similar to charge_marked, but can charge from any input field.
## Optional args are tried as field names, and must be exact matches to input field names.
## If no input field name matches, args are tried with the UI object's mark added in one
## of these ways: Bare fieldname (first) or Marked fieldname (CON:first, CON_first, or first_CON).
## E.g., suppose input has fields SUP:first => 'A', CON:first => 'B' and 'first' => 'C';
## CON->charge_from_input('first'); # stores 'C' in its 'first' field
## CON->charge_from_input('CON:first'); # stores 'B' in its 'first' field
## CON->charge_from_input('SUP:first'); # stores 'A' in its 'SUP:first' field
## SUP->charge_from_input('first'); # stores 'C' in its 'first' field
## SUP->charge_from_input('SUP:first'); # stores 'A' in its 'first' field
## SUP->charge_from_input('CON:first'); # stores 'B' in its 'CON:first' field
sub charge_from_input ($;@) {
	local *KEY		= *{ shift() };
	unless ( @_ ) {
		return %KEY
	}

	my @flds	= ref($_[0]) ? @{ $_[0] } : @_;
	my %NEW;
	for (@flds) {
		exists $KEY{_input}->{$_}
			and do { $NEW{$_}	= $KEY{_input}->{$_}; 1 }
		or exists $KEY{_input}->{"$KEY{_mark}:$_"}
			and do { $NEW{$_}	= $KEY{_input}->{"$KEY{_mark}:$_"}; 1 }
		or exists $KEY{_input}->{"$KEY{_mark}_$_"}
			and do { $NEW{$_}	= $KEY{_input}->{"$KEY{_mark}_$_"}; 1 }
		or exists $KEY{_input}->{"${_}_$KEY{_mark}"}
			and do { $NEW{$_}	= $KEY{_input}->{"${_}_$KEY{_mark}"}; 1 };
	}

 	%KEY	= ( %KEY, %NEW );
}

## Charge_resolve. Safely adds resolved data to fields in %KEY, and returns updated %KEY.
## See resolve(). Data is resolved by atomic substitutions of the UI object's data in %KEY,
## including any meta-data. Substitutions are made using the direct() method, which
## defaults to using _start => '[', _end => ']', _mark => ':', as in, e.g.,
## $obj->charge_resolve('settings_file' => '[:system_dir]/settings.config');
sub charge_resolve ($;@) {
	local *KEY		= *{ shift() };
	my %NEW;
	{
	local $_ = ref($_[0]);
	/HASH/		and %NEW	= %{ $_[0] }
		or
	/ARRAY/		and %NEW	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	/CODE/		and %NEW	= %{ $_[0]->() }
		or
					%NEW	= @_ % 2 ? (@_, '') : @_
	}

	%NEW	= map { $_ => resolve(\*KEY, $NEW{$_}) } keys %NEW;
	
	%KEY	= ( %KEY, %NEW );
}

## Charge_add. Safely adds numerical data to fields in %KEY, and returns updated %KEY.
## In mixed alpha-numerical strings, operates on the last (right-most) digits found.
## An input value may be any expression that resolves to a number (integer or float).
## Does nothing if an input value is not a number. Starts with zero value.
sub charge_add {
	local *KEY		= *{ shift() };
	my %ADD;
	return %KEY unless @_;

	my $i	= ref($_[0]);
	$i =~ /HASH/	and %ADD	= %{ $_[0] }
		or
	$i =~ /ARRAY/	and %ADD	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	$i =~ /CODE/ 	and %ADD	= %{ $_[0]->() }
		or
						%ADD	= @_ % 2 ? (@_, '') : @_;

	for my $key (keys %ADD) {
		next unless $ADD{$key} =~ /^[+-]?\d+(\.\d*)?$/;
		$KEY{$key} =~ s{^ (.*?)? ([+-]?\d+(?:\.\d*)?|) ([^\d]+)? $}
						{($1 || '') . ($2||0) + $ADD{$key} . ($3 || '')}ex;
	}
	%KEY
}

## Charge_append. Safely appends data to fields in %KEY, and returns updated %KEY.
## Note that NO white space is inserted between the original and appended values.
sub charge_append {
	local *KEY		= *{ shift() };
	my %ADD;
	return %KEY unless @_;

	my $i	= ref($_[0]);
	$i =~ /HASH/	and %ADD	= %{ $_[0] }
		or
	$i =~ /ARRAY/	and %ADD	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	$i =~ /CODE/ 	and %ADD	= %{ $_[0]->() }
		or
						%ADD	= @_ % 2 ? (@_, '') : @_;

	for my $key (keys %ADD) {
		$KEY{$key}	.= $ADD{$key}
	}
	%KEY
}

## Charge_push. Safely appends data to fields in %KEY, and returns updated %KEY.
## Data is appended to fields as an element pushed onto an anonymous array;
## the value of the field is a reference to that array. Any value in the field
## before charge_push becomes the first element of the anonymous array.
sub charge_push {
	local *KEY		= *{ shift() };
	my %ADD;
	return %KEY unless @_;

	my $i	= ref($_[0]);
	$i =~ /HASH/	and %ADD	= %{ $_[0] }
		or
	$i =~ /ARRAY/	and %ADD	= @{ $_[0] } % 2 ? (@{ $_[0] }, '') : @{ $_[0] }
		or
	$i =~ /CODE/ 	and %ADD	= %{ $_[0]->() }
		or
						%ADD	= @_ % 2 ? (@_, '') : @_;

	for my $key (keys %ADD) {
		if (ref($KEY{$key}) =~ /ARRAY/i) {
			push @{$KEY{$key}} => $ADD{$key};
		} elsif (defined $KEY{$key}) {
			$KEY{$key}	= [$KEY{$key}, $ADD{$key}];
		} else {
			$KEY{$key}	= [$ADD{$key}];
		}
	}
	%KEY
}

## Clear: safely deletes data from %KEY, and returns updated %KEY.
## Args are tried as fieldnames; if not present, the field will be added.
## If no args, all non-meta fields are cleared,
## EXCEPT the field named for the object's mark ($KEY{_mark}),
## which holds marked input data.
## Fields whose data is deleted receive the value ''.
## Meta_data (fieldnames starting with '_') is not deleted.
sub clear ($;@) {
	local *KEY		= *{ shift() };
	if (@_) {
		$KEY{$_} = '' for grep { !/^_/ } @_
	} else {
		%KEY = map { (/^_/ or $_ eq $KEY{_mark}) ? ($_ => $KEY{$_}) : ($_ => '') } keys %KEY;
	}
	%KEY
}

## Clear_undef: safely deletes data from %KEY, and returns updated %KEY.
## Args are tried as fieldnames; if not present, the field will be added.
## If no args, all non-meta fields are cleared,
## EXCEPT the field named for the object's mark ($KEY{_mark}),
## which holds marked input data.
## Fields whose data is deleted receive the value undef.
## Meta_data (fieldnames starting with '_') is not deleted.
sub clear_undef ($;@) {
	local *KEY		= *{ shift() };
	if (@_) {
		$KEY{$_} = '' for grep { !/^_/ } @_
	} else {
		%KEY = map { (/^_/ or $_ eq $KEY{_mark}) ? ($_ => $KEY{$_}) : ($_ => undef) } keys %KEY;
	}
	%KEY
}

## Clear_data, clear_input, clear_marked_input
## Similar to clear(), but REMOVES THE FIELD AS WELL AS THE DATA
## If no args, ALL non-meta fields are deleted,
## EXCEPT the field named for the object's mark ($KEY{_mark}),
## which holds marked input data.
## Meta_data (fieldnames starting with '_') is not deleted.
sub clear_data ($;@) {
	local *KEY			= *{ shift() };
	if (@_) {
		delete $KEY{$_} for grep { !/^_/ } @_
	} else {
		%KEY = map { (/^_/ or $_ eq $KEY{_mark}) ? ($_ => $KEY{$_}) : () } keys %KEY;
	}
	%KEY
}

## Removes selected or all input fields & their values
sub clear_input ($;@) {
	local *KEY			= *{ shift() };
	$KEY{_input}->{$_}	= '' for @_ ?  @_ : keys %{ $KEY{_input} };
	delete $KEY{_vals}->{$_} for @_ ?  @_ : keys %{ $KEY{_vals} };
	%KEY
}

## Removes selected or all marked input fields & their values
sub clear_marked_input ($;@) {
	local *KEY			= *{ shift() };
	delete $KEY{_vals}->{$_} for @_ ?  @_ : keys %{ $KEY{_vals} };
	%KEY
}

## Data Access
sub list_keys ($;@) {
	local *KEY		= *{ shift() };
	grep {!/^_/} keys %KEY
}

sub list_values ($;@) {
	local *KEY		= *{ shift() };
	map { $KEY{$_} } grep {!/^_/} keys %KEY
}

sub list_pairs ($;@) {
	local *KEY		= *{ shift() };
	map {"$_:  $KEY{$_}"} grep {!/^_/} keys %KEY
}

sub list_meta_keys ($;@) {
	local *KEY		= *{ shift() };
	grep {/^_/} keys %KEY
}

sub list_meta_values ($;@) {
	local *KEY		= *{ shift() };
	map { $KEY{$_} } grep {/^_/} keys %KEY
}

sub list_meta_pairs ($;@) {
	local *KEY		= *{ shift() };
	map {"$_:  $KEY{$_}"} grep {/^_/} keys %KEY
}

sub data ($;@) {
	local *KEY		= *{ shift() };
	wantarray ? map { $KEY{$_} || '' } @_ : $KEY{$_[0]} || ''
}

sub untainted_data ($;@) {
	local *KEY		= *{ shift() };
	wantarray ? map { $KEY{$_} =~ /^([^`]+)$/ ? $1 : '' } @_ : $KEY{$_[0]} =~ /^([^`]+)$/ ? $1 : ''
}

sub pairs ($;@) {
	local *KEY		= *{ shift() };
	map { $_ => ($KEY{$_} || '') } grep {!/^_/} @_ ? @_ : keys %KEY
}

sub meta_pairs ($;@) {
	local *KEY		= *{ shift() };
	map { $_ => $KEY{$_} } grep {/^_/} @_ ? @_ : keys %KEY
}

sub input_data ($;@) {
	local *KEY		= *{ shift() };
	wantarray ? map { $KEY{_input}->{$_} || '' } @_ : $KEY{_input}->{$_[0]} || ''
}

sub input_keys ($) {
	local *KEY		= *{ shift() };
	return keys %{ $KEY{_input} }
}

sub input_pairs ($;@) {
	local *KEY		= *{ shift() };
	map { $_ => ($KEY{_input}->{$_} || '') } keys %{ $KEY{_input} }
}

sub marked_input_data ($;@) {
	local *KEY		= *{ shift() };
	wantarray ? map { $KEY{_vals}->{$_} || '' } @_ : $KEY{_vals}->{$_[0]} || ''
}

sub marked_input_keys ($) {
	local *KEY		= *{ shift() };
	return keys %{ $KEY{_vals} || () }
}

sub marked_input_pairs ($) {
	local *KEY		= *{ shift() };
	@_ ? map { $_ => (defined($KEY{_vals}->{$_}) ? $KEY{_vals}->{$_} : '') } @_ : %{ $KEY{_vals} || () }
}

sub input ($) {
	local *KEY		= *{ shift() };
	$KEY{_input}
}

sub marked_input ($) {
	local *KEY		= *{ shift() };
	$KEY{_vals}
}

##################

# Load system messages and warnings
sub get_messages {
	my $arg1		= shift;
	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };
	local $/		= '';

	$KEY{_messages}	||= {};
	my $msg_file	= shift || $KEY{_message_file};
	open my $msgs, "<", "$msg_file" or return $KEY{_messages};
	$KEY{_messages}	= { ( %{ $KEY{_messages} }, map { /^\s*#/ ? () : /^([^\n]*)\n(.*)/s } <$msgs>) };
	close $msgs;
	$KEY{_messages}
}

# List system messages and warnings
# Does NOT load messages or warnings
sub list_messages {
	my $arg1		= shift;
	local *KEY		= ref($arg1) eq __PACKAGE__ ? $arg1 : *{ $arg1 };
	local $/		= '';

	$KEY{_message_list}	= [];
	my %msgs_found;

	my $msg_file	= shift || $KEY{_message_file};
	open my $msgs, "<", "$msg_file" or return $KEY{_messages};
	for (grep { !$msgs_found{$_}++ } sort keys %{ $KEY{_messages} }, map { /^\s*#/ ? () : /^([^\n]*)\n(.*)/s ? $1 : () } <$msgs> ) {
		push @{ $KEY{_message_list} } => $_;
	}
	close $msgs;
	$KEY{_message_list}
}

# Load lists for popups etc
# returns hash of array refs
sub get_lists {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	my $list_file	= shift || $KEY{_list_file} || '';
	my $list_dir	= $KEY{_list_dir} || '';
	$list_dir		= $list_dir eq "./" ? "./lists" : $list_dir;
	unless (-d $list_dir) {
		mkdir($list_dir, 0777) or die "List dir creation failed for $list_dir: $!";
	}
	GET: {
		return $KEY{_lists} unless open my $LISTS, "<", "$list_file";
		local $/		= '';
		LIST: while (my $list = <$LISTS>) {
			my @items		= split "\n" => $list;
			my @list		= map { defined && /\A\s*([^`]+)\s*\z/ ? $1 : () } @items;
			my $list_name	= shift @list;
			next LIST unless $list_name =~ s/^\s*([a-zA-Z0-9_][^\n]+)\s*$/$1/;
			$KEY{_lists}->{$list_name}	= [ @list ];
			$KEY{_listed}->{$list_name}	= { map { $_ => 1 } @list };
			if (open my $lfh, ">", "$list_dir/$list_name") {
				print $lfh join "\n" => @list, '';
				close $lfh;
			} else {
				$obj->oops("Can't save list file $list_name: $!\n");
			}
		}
		close $LISTS;
	}
	$KEY{_lists}
}

sub load_lists {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	my $list_dir	= $KEY{_list_dir} || '';
	$list_dir		= $list_dir eq "./" ? "./lists" : $list_dir;
	return $KEY{_lists} unless (-d $list_dir);

	use Path::Class;
	my $ldir	= dir($list_dir);
	while (my $lfile = $ldir->next()) {
		next unless -f $lfile;
		my $list_name	= $lfile->basename();
		my @items		= $lfile->slurp(chomp => 1);
		my @list		= map { defined && /\A\s*([^`]+)\s*\z/ ? $1 : () } @items;
		$KEY{_lists}->{$list_name}	= [ @list ];
		$KEY{_listed}->{$list_name}	= { map { $_ => 1 } @list };
	}

# 	for my $file ( glob("$list_dir/*") ) {
# 		my $list_name				=  $file;
# 		$list_name					=~ s{^.*\/(.*)$} {$1};
# 		next unless open my $lfh,"<",$file;
# 		my @items = <$lfh>;
# 		close $lfh;
# 		chomp @items;
# 		my @list	= map { defined && /\A\s*([^`]+)\s*\z/ ? $1 : () } @items;
# 		$KEY{_lists}->{$list_name}	= [ @list ];
# 		$KEY{_listed}->{$list_name}	= { map { $_ => 1 } @list };
# 	}
	$KEY{_lists};
}

sub get_lists_old {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	my $list_file	= shift || $KEY{_list_file} || '';
	GET: {
		local $/		= '';
		open LISTS, "<", "$list_file" or $obj->oops("Can't open list file $list_file: $!\n");
		while (<LISTS>) {
			my ($fld,$list)			= split "\n" => $_, 2;
			my @items				= split "\n" => $list;
			$KEY{_lists}->{$fld}	= [ @items ];
			$KEY{_listed}->{$fld}	= { map { $_ => 1 } @items };
		}
		close LISTS;
	}
	$KEY{_lists}
}

sub get_system_lists {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	my $list_file	= shift || $KEY{_list_file} || '';
	my %sys_lists;
	GET: {
		return {} unless open my $LISTS, "<", "$list_file";
		local $/	= '';
		while (<$LISTS>) {
			my ($list_name,$list)	= split "\n" => $_, 2;
			next unless $list_name =~ s/^\s*([a-zA-Z0-9_][^\n]+)\s*$/$1/;
			my @items				= split "\n" => $list;
			@items					= map { defined && /\A\s*([^`]+)\s*\z/ ? $1 : () } @items;
			$sys_lists{$list_name}	= [ @items ];
		}
		close $LISTS;
	}
	wantarray ? %sys_lists : { %sys_lists };
}

sub load_system_lists {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	my $list_file	= shift || $KEY{_list_file} || '';
	my %sys_lists;
	GET: {
		return {} unless open my $LISTS, "<", "$list_file";
		local $/	= '';
		while (<$LISTS>) {
			my ($list_name,$list)	= split "\n" => $_, 2;
			next unless $list_name =~ s/^\s*([a-zA-Z0-9_][^\n]+)\s*$/$1/;
			my @items				= split "\n" => $list;
			@items					= map { defined && /\A\s*([^`]+)\s*\z/ ? $1 : () } @items;
			$sys_lists{$list_name}	= [ @items ];
			$KEY{_lists}->{$list_name}	= [ @items ];
			$KEY{_listed}->{$list_name}	= { map { $_ => 1 } @items };
		}
		close $LISTS;
	}
	wantarray ? %sys_lists : { %sys_lists };
}

# Listed: Allows checking whether an item appears in a list;
# First optional argument is a list name; additional
# optional args are items to check the named list for.
#
# If one item is provided, returns "true" if the item is in the
# named list and "false" if not; if more than one item, returns
# a list of true/false for each item.
# If no items are provided, returns a boolean lookup hashref
# with the named list's items as keys and "true" as their values.
# Returns false if the named list doesn't exist;
# If no list name is provided, returns a hash ref with all lists
# as keys and each list's boolean lookup as their values;
sub listed ($;@) {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };

	if (@_) {
		my $fld	= shift;
		return unless exists $KEY{_listed}->{$fld};
		if (@_) {
			my @items	= @_;
			if (@items > 1) {
				return map { $KEY{_listed}->{$fld}->{$_} || 0 } @items;
			} else {
				return $KEY{_listed}->{$fld}->{$items[0]} || 0;
			}
		} else {
			return $KEY{_listed}->{$fld} || {};
		}
	} else {
		return $KEY{_listed};
	}
}

sub Xlisted ($;@) {
	my $obj		= shift;
	local *KEY		= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	
	if (@_) {
		my $fld	= shift;
		return unless exists $KEY{_lists}->{$fld};
		my $list_lookup	= { map { $_ => 1 } @{ $KEY{_lists}->{$fld} } };
		if (@_) {
			my @items	= @_;
			if (@items > 1) {
				return map { $list_lookup->{$_} || 0 } @items;
			} else {
				return $list_lookup->{$items[0]} || 0;
			}
		} else {
			return $list_lookup;
		}
	} else {
		return { map { $_ => { map { $_ => 1 } @{ $KEY{_lists}->{$_} } } } keys %{ $KEY{_lists} } };
	}
}


# Optional display management

# Make list of all available templates by reading display directory globbing on '*.tmpl'
sub get_templates {
	my $self	= shift;
	ref($self) eq __PACKAGE__ or unshift @_, $self;

	my $display_dir		= shift;
	-d $display_dir or return '_NODIR_';	# ???
	my $displays_only	= shift;
	my %templates;
	for my $file ( glob("$display_dir/*.tmpl") ) {
		my $name						=  $file;
		$name							=~ s{^.*\/(.*)\.tmpl$} {$1};
		$templates{$name}->{name} 		= $name;
		$templates{$name}->{file} 		= $file;
		$templates{$name}->{display}	= do {
											if ($templates{$name}->{file} =~ /_NOFILE_/) {
												'_NA_'
											} else {
												local $/;
												unless ( open D, "<", $templates{$name}->{file} ) {
													'_NA_'
												} else {
												 	<D>
												}
											}
										};
		#next unless $templates{$name}->{display};
		$templates{$name}->{reveal}		= do {
											my $tmpl	= "\n" . $templates{$name}->{display};
											$tmpl		=~ s/(&)([^a])/&amp;$2/g;
											$tmpl		=~ s/>/&gt;/g; $tmpl =~ s/</&lt;/g;
											$tmpl		=~ s/\n/<br>\n/g;
											$tmpl
										};
	}
	return ( map { $_ => $templates{$_}->{display} } keys %templates )
		if $displays_only;
	%templates
}


## Make list of designated display templates from a database in the
## designated directory, checking directory to verify templates
sub get_displays {
	my $self	= shift;
	ref($self) eq __PACKAGE__ or unshift @_, $self;

	my $display_dir = shift;
	my $display_db	= shift || 'displays_db.txt';
	-d $display_dir or return '_NODIR_';
	-e "$display_dir/$display_db" or return '_NODB_';
	# Connect to the database
	require BVA::XUI::DATA;
	my $displays	= BVA::XUI::DATA->init->connect(file => "$display_dir/$display_db")
		or die "Couldn't connect to display DB: \n!";
	#header:name|tmpl|type|note
	#labels:Display Name|Template|Type|Note
	#formats:t:18|t:30|t:6|a:20.6
	my $all_displays_query	= $displays->prepare( qq{SELECT * WHERE all} );
	$all_displays_query->execute();

	my %display_data;

	while ( my $r = $all_displays_query->fetchrow_hashref() ) {
		$display_data{ $r->{name} } = { name => $r->{tmpl}, };
	}

	for (keys %display_data) {
		my $fname						= $display_data{$_}->{name} . '.tmpl';
		$display_data{$_}->{file}		= -e "$display_dir/$fname" ?
										"$display_dir/$fname" :
										'_NOFILE_';
		$display_data{$_}->{display}	= do {
											if ($display_data{$_}->{file} =~ /_NOFILE_/) {
												'_NA_'
											} else {
												local $/;
												unless ( open D, "<", $display_data{$_}->{file} ) {
													'_NA_'
												} else {
												 	<D>
												}
											}
										};
		#next unless $display_data{$_}->{display};
		$display_data{$_}->{reveal}		= do {
											my $tmpl	= "\n" . $display_data{$_}->{display};
											$tmpl		=~ s/(&)([^a])/&amp;$2/g;
											$tmpl		=~ s/>/&gt;/g; $tmpl =~ s/</&lt;/g;
											$tmpl		=~ s/\n/<br>\n/g;
											$tmpl
										};
	}
	%display_data
}

sub add_display {
	my $new_display		= shift;
	my $display_dir		= shift;
	my $display_note	= shift || '';
	my $display_db		= shift || 'displays_db.txt';
	-d $display_dir or oops( '_NODIR_');
	-e "$display_dir/$display_db" or return '_NODB_';
	# Connect to the database
	require BVA::XUI::DATA;
	my $displays	= BVA::XUI::DATA->init->connect(file => "$display_dir/$display_db");
	#header:name|tmpl|type|note
	#labels:Display Name|Template|Type|Note
	#formats:t:18|t:30|t:6|a:20.6

	my $all_displays_query					= $displays->prepare( qq{
		SELECT name FROM me WHERE all TO cursor: hash
	} );

	$all_displays_query->execute();
	while ( my $r = $all_displays_query->fetchrow_hashref() ) {
		oops ("Display \"$new_display\" already exists.<br>\n
		Please choose a different name.") if $r->{name} eq $new_display
	}

	my $insert								= $displays->prepare( qq{
		INSERT INTO me VALUES name=>$new_display;note=>$display_note WITH id_override: 1
	} );

	return $insert->execute;
}


## Interactive output

## reset_header	-- enables emission of a new header
##				-- for secondary output
sub reset_header {
	my $self				= shift();
	(ref($self) eq __PACKAGE__
		and local *KEY		= *{ $self } )
			or unshift @_, $self;

	$hdr_prntd	= '';

	1;
}

## Header: returns a server header if one hasn't already been returned,
##         and remembers what kind
## out_header  -- new name 2002-12-11
##			   -- use only CGI.pm header methods for http output 2004-11-14
sub out_header ($;@) {
	return '' if $hdr_prntd;
	my $self				= shift();
	(ref($self) eq __PACKAGE__
		and local *KEY		= *{ $self } )
			or unshift @_, $self;
	{
		local $_			= shift() || $KEY{_env} || 'htm';

		/^htm/i	and do {
					$hdr_prntd = 'HTM';
					return ($ENV{SCRIPT_NAME} and $ENV{SCRIPT_NAME} =~ m{^.*?/nph-[^/]+$}) ?
						CGI::header(-status=>'200 OK', -nph=>1, -type=>'text/html', -charset=>'utf-8') :
							CGI::header(-status=>'200 OK', -type=>'text/html', -charset=>'utf-8')
		} or
		/^xhr/i	and do {
					$hdr_prntd = 'HTM';
					return CGI::header(-status=>'200 OK', -type=>'text/html', -charset=>'utf-8')
		}  or
		/^gzip/i	and do {
					$hdr_prntd = 'HTM';
 					return CGI::header(-status=>'200 OK', -type=>'text/html', -encoding=>'gzip')
#					return CGI::header(-status=>'200 OK', -type=>'application/x-gzip', -encoding=>'identity')
		}  or
		/^xml/i	and do {
					$hdr_prntd = 'XML';
					return CGI::header(-status=>'200 OK', -type=>'text/xml', -charset=>'utf-8')
		} or
		/^loc/i	and do {
					$hdr_prntd	= 'LOC';
# 					my $url		= $self->url_encode(shift || $ENV{SCRIPT_NAME});
# 					return CGI::redirect(-status=>'302 Found', -location=>"$url")
					my $url		= shift || $ENV{SCRIPT_NAME};
					return qq{Location: $url\n\n}
		} or
		/^te?xt/i and do {
					$hdr_prntd = 'TXT';
					return CGI::header(-status=>'200 OK', -type=>'text/plain', -charset=>'utf-8')
		} or
		/^nph/i and do {
					$hdr_prntd = 'NPH';
					return CGI::header(-status=>'200 OK', -nph=>1, -type=>'text/html', -charset=>'utf-8')
		} or
		/^no/i and do {
					$hdr_prntd = 'NO';
					return CGI::header( -status=>'204 No Response')
		} or
		/^mime:(.*)\s*$/i and do {
					$hdr_prntd	= 'HTM';
					my $type	= $1;
					return CGI::header( -status=>'200 OK', -type=>$type, -charset=>'utf-8')
		} or
		/^pdf(.*)\s*$/i and do {
					$hdr_prntd = 'PDF';
					my $file	= $1 || 'PDF Attachment';
					return CGI::header(-status=>'200 OK', -type=>'application/x-pdf',  -attachment => $file, -charset=>'utf-8')
		} or
		/^download:(.*)\s*$/i and do {
					$hdr_prntd	= 'HTM';
					my $file	= $1;
					return <<DL;
Content-Type: application/x-download
Content-Disposition: attachment; filename=$file

DL
		} or
		/^audio:(.*)\s*$/i and do {
					$hdr_prntd	= 'HTM';
					my $file	= $1;
					return <<DL;
Content-Type: audio/mp3

DL
		} or ''
	}
}



sub ask ($;@) {
	my $arg1		= shift;
 	ref($arg1) eq __PACKAGE__ or unshift @_, $arg1;
	my $text		= shift() || $arg1->process("MSG") || '';
	my $title		= shift() || 'Info, please:';
	my $env			= shift() || $arg1->data('_env') || 'htm';
	my $state		= shift() || $arg1->data('state') || $arg1->input_data('state');
	my $hid_vals	= shift() || '';
 	my $mark		= $arg1->data('_mark');
 	my $style		= $arg1->process("STYLE") || '';
	my $cgi			= $arg1->data('cgi') || $ENV{SCRIPT_NAME};
	my $path_info	= $arg1->input_data('_path_info'); $path_info = $path_info ? "/$path_info" : "";
	my $ask_act		= $arg1->data('_ask_act') || $arg1->data('action') || "OK";
	my $return		= $arg1->data('return_to') || $arg1->marked_input_data("return_to");
	my $ask			= $env eq 'htm' ? <<HT_ASK : <<EDIT_ASK;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
        "http://www.w3.org/TR/html4/strict.dtd">

<html lang="en">
<head>
	$style
	<title>$title</title>
</head>
<body bgcolor="#CCFFCC" onLoad="var e=document.forms[0].elements;
for (n=0; n!=e.length+1; n++) {
if (e[n]!=null) {if (e[n].type=='text' || e[n].type=='textarea') { e[n].focus(); break;} } }">

<div align="center">
<form action="$cgi$path_info" method="POST" enctype="application/x-www-form-urlencoded" name="ask">
	<table cellpadding="18" cellspacing="1"  class="form">
		<tr>
			<th bgcolor="#CCCC99">
				<h2>$title</h2>
			</th>
		</tr>
		<tr>
			<td bgcolor="#FFFFCC">
				$text
			</td>
		</tr>
		<tr>
			<td bgcolor="#CCCC99">
<input type="submit" name="action" value="$ask_act"> &nbsp;
<input type="submit" name="action" value="Cancel">

<input type="hidden" name="state" value="$state">
<input type="hidden" name="$mark:return_to" value="$return">
$hid_vals
			</td>
		</tr>

	</table>
</form>
</div>

</body>
</html>

HT_ASK
$title
$text

EDIT_ASK

	print out_header $env;
    print $ask;
    exit;
}


sub holdit ($;@) {
	my $arg1		= shift;
 	ref($arg1) eq __PACKAGE__ or unshift @_, $arg1;
	my $text		= shift();
	my $title		= shift() || 'Please Wait:';
	my $pause		= shift() || 0;
	my $next		= shift() || '';
	my $ask_act		= shift() || $arg1->data('_ask_act') || "OK";
	my $state		= shift() || $arg1->data('state');
	my $actor		= shift() || $arg1->data('actor');
	my $env			= shift() || $arg1->data('_env') || 'htm';
	my $cgi			= $arg1->data('cgi');
	my $path_info	= $arg1->input_data('_path_info'); $path_info = $path_info ? "/$path_info" : "";
	my $ask			= $env eq 'htm' ? <<HT_ASK : <<EDIT_ASK;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
        "http://www.w3.org/TR/html4/strict.dtd">

<html lang="en">
<head>
	<title>$title</title>
	<meta http-equiv="Refresh" content="$pause; URL=$next">
</head>
<body bgcolor="#CCFFCC">

<div align="center">

<form action="$cgi$path_info" method="POST" enctype="application/x-www-form-urlencoded" name="ask">

<h2>$title</h2>

$text

<input type="submit" name="action" value="$ask_act"> &nbsp;
<input type="submit" name="action" value="Cancel">
<input type="hidden" name="state" value="$state">
<input type="hidden" name="actor" value="$actor">
<input type="hidden" name="OK" value="1">

</form>
</div>

</body>
</html>

HT_ASK
$title
$text

EDIT_ASK

	print $arg1->out_header($env);
    print $ask;
    exit;
}


sub oops ($;@) {
	my $arg1		= shift;
 	ref($arg1) eq __PACKAGE__ or unshift @_, $arg1;
	my $text		= shift();
	my $title		= shift() || 'Oops!';
	my $env			= shift() ||  'htm';
	my $oops		= $env eq 'htm' ? <<HT_OOPS : <<EDIT_OOPS;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
        "http://www.w3.org/TR/html4/strict.dtd">

<html lang="en">
<head>
	<title>$title</title>
</head>
<body bgcolor="#CCFFCC">
<div align="center">
	<table cellpadding="18" cellspacing="1">
		<tr>
			<th bgcolor="#CCCCFF">
				<h2>$title ($env)</h2>
			</th>
		</tr>
		<tr>
			<td bgcolor="#FFFFCC">
				$text
			</td>
		</tr>
	</table>
</div>
</body>
</html>

HT_OOPS
$title ($env)
$text

EDIT_OOPS

	print out_header $env;
    print $oops;
    exit;
}


sub hey ($;@) {
	my $self				= shift();
	local *KEY				= ref($self) eq __PACKAGE__ ? $self : *{ $self };
	my $ref					= shift() || ( $KEY{_mark} eq $KEY{_ui_mark} ? \%KEY : \*KEY );
	my $env					= $KEY{_env} || 'htm';
	my $title				= shift() || 'Hey!';
	my $pure				= shift();
	my $output				= '';
	
	if (ref($ref)) {
		use Data::Dumper;
 		$Data::Dumper::Purity	= length($pure) ? $pure : 1;
 		$Data::Dumper::Deparse = 1;
 		$Data::Dumper::Sortkeys	= 1;
 		no warnings;
 		eval { $output = Dumper($ref); 1 } 
 			or do { $output .= "\nDumper problem: \n$@"; }; 		
		$output					= $self->HTML_entify($output) if $env eq 'htm';
	} else {
		$output					= $ref;
	}
	$output					||= "Oh, never mind.";

	my $hey	= $env eq 'htm' ? <<HT_HEY : $env eq 'term' ? <<EDIT_HEY : <<TERM_HEY;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
        "http://www.w3.org/TR/html4/strict.dtd">

<html lang="en">
<head>
	<title>$title</title>
</head>
<body bgcolor="#FFFFFF">
<div align="center">
	<table cellpadding="8" cellspacing="1" bgcolor="#CCFFCC">
		<tr>
			<th>
				<h2>
$title
				</h2>
			</th>
		</tr>
		<tr>
			<td>
				<pre>
$output
				</pre>
			</td>
		</tr>
	</table>
</div>
</body>
</html>

HT_HEY
$title ($env)
$output

EDIT_HEY
$title ($env)
$output

TERM_HEY

	print out_header $env;
    print $hey;
    exit;
}

sub oh ($;@) {
	my $self				= shift();
	local *KEY				= ref($self) eq __PACKAGE__ ? $self : *{ $self };
	my $env					= $KEY{_env} || 'htm';
	my $ref					= shift() || ( $KEY{_mark} eq $KEY{_ui_mark} ? \%KEY : \*KEY );

	my $title				= shift() || 'Oh!';
	my $pure				= shift();
	my $output				= '';
	if (ref($ref)) {
		use Data::Dumper;
 		$Data::Dumper::Purity	= length($pure) ? $pure : 1;
 		$Data::Dumper::Deparse = 1;
 		$Data::Dumper::Sortkeys	= 1;
 		no warnings;
 		eval { $output = Dumper($ref); 1 } 
 			or do { $output .= "\nDumper problem: \n$@"; };
		$output					= $self->HTML_entify($output) if $env eq 'htm';
	} else {
		$output					= $ref;
	}
	$output					||= "Oh, never mind.";

	my $obj					= $self->direct();
	$obj->charge(title => $title, output => $output);
	
	print out_header $env;

	print $obj->_process_tmpl ($env eq 'htm' ? <<"HT_HEY" : $env eq 'term' ? <<"EDIT_HEY" : <<"TERM_HEY");
<!DOCTYPE html>
<html lang="en">
<head>
	<meta charset="utf-8" />
	<meta name="generator" content="Interworks" />
	<title>[:title] ([:_env])</title>
    <style type="text/css">
		article, section, aside, hgroup, nav, header, footer, figure, figcaption {
		  display: block;
		}
		body {
		  background-color:#FFF;
		}
		#page {
		  background-color:#CFC;
		  text-align:center;
		  max-width:80em;
		}
		header {
		  background-color:#FCF;
		}
		footer{
		  background-color:#FCF;
		}
		#report {
		  text-align:left;
		  margin-left:6em;
		  margin-right:6em;
		  background-color:#FFC;
		}
	</style>
</head>
<body>
<div id="page">
	<header>
[:title] ([:_env])
	</header>
	<nav>
	</nav>
	<section id="report">
		<pre>
[:
output
]
		</pre>
	</section>
	<footer>
[:title]
	</footer>
</div>
</body>
</html>

HT_HEY

[: title] ([:_env])
[:output]

EDIT_HEY
[:title] ([:_env])
[:output]

TERM_HEY

    exit;
}


sub reveal ($;@) {
	local *KEY		= *{ shift() };

	my $tmpl		= join('' => @_)
						|| $KEY
							|| $KEY{_default_tmpl};
	my $output		= ' ';

 	my @objects		= objects( *KEY );

	LOAD: {
		foreach my $obj ( @objects ) {
			$obj->charge_auto();
			$obj->calculate();
		}
	}

	## Temporarily disable header block
	my $temp_hdr_status		= $hdr_prntd;
	$hdr_prntd				= 0;

	# Make sure nesting works across UI objects
	my %starts;
	my $start_pat	= '(' . join( '|' => map { qq{\Q$_\E}  } grep { !$starts{$_}++ } map { $_->data('_start') } @objects) . ')';
	my $mark_pat	= '(' . join( '|' => map { $_->data('_mark') } @objects) . ')';
	my %cycles		= (_cycle_num	=> 1);
	my @renderlines;
	my %used_objects;
	use vars qw/*REV/;
	RENDER: {
		push @renderlines => qq{<a name=\"cycle$cycles{_cycle_num}\"> </a>\n<hr>Cycle $cycles{_cycle_num}:\t\t\t[<a href="#top">top</a>]\n<hr>\n};
		foreach my $obj ( @objects ) {
			local *REV	= *{ $obj };
			defined	&REV
				or	*REV	= \&OUTPUT;

			my ($start, $end, $mark)		= $obj->data('_start','_end','_mark');
			push @renderlines => qq{<pre>\nProcess:\nMark\tStart\tEnd\n}, BVA::XUI::UTILS::HTML_entify(qq{$mark\t$start\t$end}), qq{\n</pre>\n};
 			$REV{_match_str}				= qr{(?s:((\Q${start}${mark}\E)[: ]((.(?!${start_pat}${mark_pat}))*?)\Q$end\E))};

			GO: {
				my $str = $tmpl;
				study $str;
				$str =~ s{$REV{_match_str}}
				   {
					my ($t,$token)	= ($1,$1);
					my $arg			= $3;
					my $out			= REV($obj, $arg);
					$out			||= ($REV{__DONE__} ? $out : OUTPUT($obj, $arg));
					$out			||= (delete $REV{__DONE__} ? $out : $t);
					my $form		= $out;
					$token			= BVA::XUI::UTILS::HTML_entify($token);
					$form			= BVA::XUI::UTILS::HTML_entify($form);

# 					push @renderlines => qq{\n$token\n<blockquote><b>\n$form\n</b></blockquote>\n};
					push @renderlines => qq{\n$token\n<blockquote>\n<pre><b>\n$form\n</b></pre></blockquote>\n};
					push( @{ $used_objects{_names} } => $mark) unless $used_objects{$mark}++;
					push( @{ $cycles{$cycles{_cycle_num}}{_names} } => $mark) unless $cycles{$cycles{_cycle_num}}{$mark}++;
					$out;
				}gsex;  # $form			=~ s/\n/<br>\n/g;
				last GO if $str eq $tmpl;
				$tmpl = $str and redo GO;
			}
			push @renderlines => qq{\n}
		}
		last RENDER if $output eq $tmpl;
 		$cycles{_cycle_num}++;
		$output = $tmpl and redo RENDER
	}
	# Restore header status
	$hdr_prntd			= $temp_hdr_status;

	my $title			= 'Reveal';
	my $revelation			= join "" =>
		join("  " => "Objects available:", map { $_->data('_mark') } @objects),
		qq{\n<br>\nProcessing Done:<br>\n$cycles{_cycle_num} cycles },
		qq{rendering from }, scalar @{ $used_objects{_names} }, qq{ of the available objects },
		join( ' ' => '(', @{ $used_objects{_names} }, ")<br>\n" ),
 		map( {
 			qq{<a href=\"#cycle$_\">Cycle $_</a> &nbsp; } . join( ' ' => '(', @{ $cycles{$_}{_names} }, ")<br>\n")
 			} sort grep {! /^_cycle_num$/ } keys %cycles),
 		qq{<a href=\"#final\">Final Rendering</a>},
		@renderlines,
		qq{<a name=\"final\"> </a>\n<hr>},
		qq{Final Rendering:\t\t\t[<a href="#top">top</a>]\n<hr>\n},
		qq{<pre><b>\n},
		BVA::XUI::UTILS::HTML_entify($output),
		qq{</b></pre>},
		qq{<br>\n};

		my $hey	= <<HEY;
<!DOCTYPE HTML>
<html lang="en">
<head>
	<title>$title</title>
</head>
<body style="background-color:#CCFFCC;padding-top:0;margin-top:0;">
<hr id="top" style="width:100%;height:0;color:#CCFFCC;margin:0;">

<h2>
$title
</h2>

<div style="text-align: left;">

$revelation

</div>

</body>
</html>

HEY

	print out_header('htm');
    print $hey;
    exit;

}


## Test Subroutines

sub dumpit {
	my $self				= shift();
	local *KEY				= ref($self) eq __PACKAGE__ ? $self : *{ $self };
	my $env					= $KEY{_env} || 'htm';

	my $ref					= shift();
	my $pure				= shift();
	my $output				= '';
	use Data::Dumper;
	$Data::Dumper::Purity	= length($pure) ? $pure : 1;
	$Data::Dumper::Deparse	= 1;
	$Data::Dumper::Sortkeys	= 1;
	no warnings;
	eval { $output = Dumper($ref); 1 } 
		or do { $output .= "\nDumper problem: \n$@"; };
	$output					= "<pre>\n" . $self->HTML_entify($output) . "\n</pre>" if $env eq 'htm';
	return "\n$output\n";
}


## Generate message closures
## for placing dynamic text into layouts and templates
## $msg = make_msg("Record $id");	# creates the message sub with optional initial msg
## The closure accepts a string or a hash ref:
## $msg->("add: ...");			# appends ' ...' to msg
## $msg->("or: ...");			# inserts '...' if msg is empty
## $msg->("print: ...");			#outputs whole msg plus '...'
## $msg->("log: ...");				#outputs a timestamp and the whole msg, stripped of html
## $msg->("msg: msg_name, ...");	#looks up msg_name, inserts any message plus '...'
##  $msg->({msg => msg_name});
## Used by charge_msg() and charge_err().
sub make_msg ($;@) {
	# $key may be a UI object or typeglob (*KEY)
	my $key			= shift;
	my $default_msg = shift || '';
	$default_msg	=~ s/\<time\>/BVA::XUI::DATETIME::tell_time('iso')/ge;
	my $msg_holder	= '';
	return sub {
		my $in					= shift;
		return $msg_holder unless $in;
		my $time				= BVA::XUI::DATETIME::tell_time('iso');
		my ($command, $add_msg)	= ref($in) ? %{ $in } : split ': ',  $in, 2;
		$add_msg and $add_msg	=~ s/\<time\>/$time/ge;

		if ($command =~ /^print/i) {
			$msg_holder			.=  $add_msg;
			return qq|$default_msg $msg_holder|;
		} elsif ($command =~ /^add/i) {
			$msg_holder			.=  $add_msg;
			return $add_msg;
		} elsif ($command =~ /^msg/i) {
			my ($msg_name,$msg)	= split /\s*,\s*/ => $add_msg, 2;
			$msg_holder			=  message($key, $msg_name) . ($msg || '');
			return $msg_holder;
		} elsif ($command =~ /^or/i) {
			$msg_holder			= $msg_holder =~ /^\s*$/s ? $add_msg : $msg_holder;
			return $msg_holder;
		} elsif ($command =~ /^log/i) {
			$msg_holder			.= qq|\n$time $add_msg|;
			$msg_holder			=~ s/<br>//i;
			return qq|$time $default_msg $msg_holder|;
		} else {
			$msg_holder			= $in;
			return $msg_holder;
		}
	}
}

=head1 AUTHOR

Bruce W Van Allen, C<< <bva at cruzio.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-bva-xui at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=BVA-XUI>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc BVA::XUI


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=BVA-XUI>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/BVA-XUI>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/BVA-XUI>

=item * Search CPAN

L<http://search.cpan.org/dist/BVA-XUI/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2014 Bruce W Van Allen.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut


1;
