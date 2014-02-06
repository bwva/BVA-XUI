package BVA::XUI::DATA::WHERE;

$BVA::XUI::DATA::WHERE::VERSION	= '1.030_001';	# 2013-03-31 bva@cruzio.com

use strict;
use warnings;

# use BVA::XUI::DATA::WHERE::Parser;

## Make Selector [updated 2013-03-28]
# Returns the closure used for testing selection matches
# ** Revised 2013-03 to use a parser created with Parse::RecDscent; the parser is 
# accessed with the method ->_parser(), which calls $parser = DATA::WHERE::Parser->new()
# and memoizes the parser.
# ** Revised 2013-02-03 to allow search fields to be identified with the '+' or '-' wildcard;
# this allows searching all the fields whose names CONTAIN the partial field name provided.
# A '+' yields an OR closure that returns true if ANY of these fields matches the search pattern;
# A '-' yields an AND closure that returns true if ALL of these fields match the search pattern.
# The latter would mostly be used with negating search patterns.
# E.g., 'city+ = Santa Cruz' returns records for which ANY field with 'city' in its name
# has the value 'Santa Cruz'. (home_city=Santa Cruz OR work_city=Santa Cruz)
# 'city- != Santa Cruz' returns records for which NO field with 'city' in its name has
# the value 'Santa Cruz'.  (home_city!=Santa Cruz AND work_city!=Santa Cruz)
# TODO: the '+/-' wildcard searches work, but the selector's 'where' attribute gets parens
# around the OR-ed and AND-ed matches, which won't parse if passed in again.
# New Version 2005-05-24
# 2007-03-03 Standard acceptor and rejector added.
# 	$self->{selector} no longer automatically set
# WHERE	(fld|?)(~|=~|@~|!~|^|#|*|!*|=|:|!|>|<|!>|!<|==|>>|<<|<=|>=|!!)[val] #
sub make_selector {
	my ($self,$sttmt)	= @_;
	my %init;
	
	$init{acceptor}		= {
		where		=> '_ALL_',
		count		=> 0,
		tried		=> 0,
		skipped		=> 0,
		phrase		=> 'all records accepted',
		test		=> sub {
			++$_[0]->{selector}->{skipped} and return if $_[1] =~ /$_[0]->{_line_blank}|$_[0]->{_line_skip}/;
			++$_[0]->{selector}->{tried},  $_[0]->{selector}->{COUNT} = ++$_[0]->{selector}->{count}
		},
	};

	$init{rejector}		= {
		where		=> '_NONE_',
		count		=> 0,
		tried		=> 0,
		skipped		=> 0,
		phrase		=> 'all records rejected',
		test		=> sub {
			++$_[0]->{selector}->{skipped} and return if $_[1] =~ /$_[0]->{_line_blank}|$_[0]->{_line_skip}/;
			++$_[0]->{selector}->{tried};
			return;
		},
	};

	if (!$sttmt or $sttmt  =~ /^\s*_?all_?\s*$/i) {
		return $init{acceptor};

	} elsif ($sttmt eq '_NONE_') {
		return $init{rejector};
	}
	
	# Check for balanced parentheses
	unless ($self->parens_balanced($sttmt)) {
		$init{rejector}->{phrase}	.= qq{. Unbalanced parentheses in Search Phrase};
		$init{rejector}->{error}	= '1';
		return $init{rejector};
	} 
	
	# Substitute ' AND ' for comma, for legacy compatibility (WHERE home_city ^ san, home_address ~ hanover)
	$sttmt	=~ s/\s*,\s*/' AND '/ge;
	
	# Now parse $where and build its criteria into a selector
	my $where			= $sttmt;
	my $acceptor		= $init{acceptor};
	my $rejector		= $init{rejector};
 	my @parts			= $self->_parser($where);	
 	my @chunks			= $self->_chunker(@parts);
 	my @groups			= $self->_grouper(@chunks);
	my @tagged_groups	= $self->_tagger(@groups);
	my $selector		= $self->_parse_conditions(\@tagged_groups);
	if ($selector->{error} or !exists $selector->{'test'}) {
		$rejector->{phrase}	.= qq{. $selector->{'phrase'} [based on "$where"]};
		$rejector->{error}	= 1;
		return $rejector;
	} else {
		$acceptor->{phrase}	= $selector->{phrase};
		$acceptor->{error}	= 0;
		$acceptor->{depth}	= $selector->{depth};
		$acceptor->{where}	= $selector->{canon} || $where;  # '_WHERE_';
		$acceptor->{test}	= sub {
			if ($_[1] =~ /$_[0]->{'_line_blank'}|$_[0]->{'_line_skip'}/) {
				++$_[0]->{'selector'}->{'skipped'}, return;
			}
			++$_[0]->{'selector'}->{'tried'};
			if ($selector->{'test'}->($_[1])) {
				return $_[0]->{'selector'}->{'COUNT'} = ++$_[0]->{'selector'}->{'count'};
			}
			return;
		};
		return $acceptor;
	}
}

# =~ m/^(.*?) ?([:=><~!^#*@]{1,2}) ?(.*)$/s
# This works in Perl 5.8.x. 5.10 could use (?2) instead of (??{ $np })
sub _RE_parser {
	my $self	= shift;
	my @queue	= @_;
	my @parts;
	our $np; # won't stay shared with 'my $np'
# 			(?<! \S)(?: \()?\(      # opening paren NOT preceded by non-space to allow parens in regexes

# 	$np			= qr{
# 		([^()]+)?
# 		(
# 			(?<! [:=><~!^#*@])\(      # opening paren NOT preceded by sttmt op to allow parens in regexes
# 			(?:				
# 				[^()]++
# 				|
#  				(??{ $np }) # compiled pattern referring to itself
# 			)*
# 			\)
# 		)
# 		([^()]+)?
# 	}x;

# 	$np			= qr{
# 		([^()]+)?
# 		(
# 			(?<! \S)\(      # opening paren NOT preceded by non-space to allow parens in regexes
# 			(?:				
# 				[^()]++
# 				|
#  				(??{ $np }) # compiled pattern referring to itself
# 			)*
# 			\)
# 		)
#  		([^()]+)?
#  	}x;

# 	$np			= qr{
# 		(.*)
# 		(
# 			(?<! \S)\(      # opening paren NOT preceded by non-space to allow parens in regexes
# 			(?:				
# 				[^()]++
# 				|
#  				(??{ $np }) # compiled pattern referring to itself
# 			)*
# 			\)
# 		)
#  		(.*)
#  	}x;

	$np			= qr{
  		([^()]+)?
  		(.*?)
		(?<! [[:punct:]])
		(\()      # opening paren NOT preceded by punctuation, to allow parens in regexes
		(
			(?:				
				[^()]++
				|
 				(??{ $np }) # compiled pattern referring to itself
			)*
		)
		(\))
  		([^()]+)?
 	}x;

	while ( @queue ) {
		my $str	= shift @queue;
		next unless defined $str;
		$str =~ s/^\s*(.*)\s*$/$1/;
		next unless length($str);
		
		my @str_parts;
# 		my $crit_match		= qr/(?:\S+) ?(?:[:=><~!^#*@]{1,2})(?:.+)/;
		my $crit_match		= qr/(?:[A-Za-z][\w_.:-]+) ?(?:[:=><~!^#*@]{1,2})(?:.+)/;
		my $or_group_match	= qr/\((?:$crit_match\s+OR\s){1,}\s*$crit_match\)/;
		my $and_group_match	= qr/\(?(?:(?:$crit_match|$or_group_match)\s+AND\s){1,}\s*(?:$crit_match|$or_group_match)\)?/;
		
 		if (@str_parts	= $str =~ /(?:($and_group_match)( OR )?){1,}\s*($and_group_match)/g ) {
#		if (@str_parts	= $str =~ /(?:($and_group_match)( OR )){1,}\s*($crit_match|$and_group_match)/g ) {
			
		} elsif ($str =~ /^(.*?)($and_group_match)(.*)$/ ) {
			@str_parts	= ($1,$2,$6);
		} elsif ($str =~ /^(.*?)($or_group_match)(.*)$/ ) {
			@str_parts	= ($1,$2,$6);
		} elsif ($str =~ /^(\(($crit_match)\s+(OR)\s+($crit_match)\))$/ ) {
			@str_parts	= ($1);
		} elsif (
			$str =~ /^(.*?\S\s*\))\s+(OR)\s+(\(\s*\S.*)$/
				||
			$str =~ /^(.*?\S\s*\)?)\s+(OR)\s+(\S.*)$/
				||
			$str =~ /^(.*?\S)\s+(OR)\s+(\(?\s*\S.*)$/
		) {
			@str_parts	= ($1,$2,$3);
		}
		#die "$str:\n\t" . join("\n\t" => @str_parts) . "\n\n";
		my @particles	= $str =~ m/$np/g;
		if (@particles) {
  			#unshift @queue => map { if ($_ =~ /[()]/){push @parts=>$_; () } else { $_ }  } @particles;
 			unshift @queue => @particles;
		} else {
# 			if ($str =~ m/^\((.*?) ?([:=><~!^#*@]{1,2})(.*)\)$/s) {
			if ($str =~ m/^\($crit_match\)$/s) {
				$str =~ s/^\(// unless $str =~ / (OR|AND) /;
				$str =~ s/\)$// unless $str =~ / (OR|AND) /;
			} elsif ($str =~ /^\(([^()]+)\)$/) {
				$str	= $1 unless $str =~ / (OR|AND) /;
			} elsif ($str =~ /([^()]*?) ?([:=><~!^#*@]{1,2})(.*)/) {
				
			}
			push @parts => $str;
			#die "Here" if $str =~ /last=/;
		}
	}	
	#die join( ' | ' => grep { $_ } @parts), "\n";
# 	return grep { $_ } splice @parts;
	return grep { $_ } splice @parts;
}

# Uses a parser precompiled by Parse::RecDescent
sub _parser {
	my $self	= shift();
	my $sttmt	= shift;
	require BVA::XUI::DATA::WHERE::Parser;
	$self->{_sttmt_parser}	||= BVA::XUI::DATA::WHERE::Parser->new();
	return $self->{_sttmt_parser}->list($sttmt);
}

sub _chunker {
	my $self	= shift;
	my @parts	= @_;
	if (@parts == 1 and ref($parts[0]) eq 'ARRAY') {
		@parts	= @{ $parts[0] };
	}
	my @chunks;
	for my $part (@parts) {
		if (ref($part) eq 'ARRAY') {
 			push @chunks => [ $self->_chunker( @{ $part }) ];
		} elsif ($part =~ /^\s*(.+?)\s+AND\s*$/) {
			push @chunks => $self->_chunker($1), 'AND';
		} elsif ($part =~ /^\s*AND\s+(.+?)\s*$/) {
			push @chunks => 'AND', $self->_chunker($1);
		} elsif ($part =~ /^\s*AND\s*$/) {
			push @chunks => 'AND';
		} elsif ($part =~ /^\s*OR\s+(.+?)\s*$/) {
			push @chunks => 'OR', $self->_chunker($1);
		} elsif ($part =~ /^\s*OR\s*$/) {
			push @chunks => 'OR';
		} elsif ($part =~ /\S\s*\)\s+OR\s+\(\s*\S/) {
			die "OR between parens";
		} elsif ($part =~ /\S\s+(AND|OR)\s+\S/) {
 			push @chunks => [ $self->_chunker( split /\s+(AND|OR)\s+/ => $part ) ];
		} else {
			push @chunks => $part;
		}
	}
	return @chunks;
}
	
sub _grouper {
	my $self	= shift;
	my @chunks	= @_;
	my $hanging_OR	= 0;
	my $hanging_AND	= 0;
	my $group_idx	= 0;
	my @groups;
	for my $chunk (@chunks) {
		if ($chunk eq 'OR') {
			push @groups => $chunk;
			$group_idx += 2;
		} elsif ($chunk eq 'AND') {
			push @{ $groups[$group_idx] } => $chunk;
		} elsif ($chunk =~ /^(.+) OR\s*$/) {
			push @{ $groups[$group_idx] } => [$1,'OR'];
			$hanging_OR++;
		} elsif ($hanging_OR) {
			push @{ $groups[$group_idx]->[-1] } => ref($chunk) eq 'ARRAY' ? @{ $chunk } : $chunk;
			$hanging_OR	= 0;
		} else {
 			push @{ $groups[$group_idx] } => $chunk;
#			push @{ $groups[$group_idx] } => ref($chunk) eq 'ARRAY' ? @{ $chunk } : $chunk;
		}
	}	
 	return @groups;		
}

sub _tagger {
	my $self	= shift;
	my $item	= @_ > 1 ? [ @_ ] : $_[0];
	if (ref($item) eq 'ARRAY') {
		my @pieces	= @{ $item };
		my $tag		= 'ONLY';
		if (exists $pieces[1] and $pieces[1] eq 'OR') {
			$tag = 'O';
		} elsif (exists $pieces[1] and $pieces[1] eq 'AND') {
			$tag = 'A';
		}
		@pieces = map {
			my $piece = $_;
			if (ref($piece) eq 'ARRAY') {
				$self->_tagger($piece);
			} elsif ($piece =~ /^(AND|OR)$/) {
				();
			} else {
				$piece
			}
		} @pieces;
		return @pieces > 1 ? [ $tag,  @pieces ] : $pieces[0];
	}
	return $item;
}

sub _parse_conditions {
	my $self	= shift;
 	my $item	= shift;
	$item		= ref($item) eq 'ARRAY' ? $item : [ $item ];
 	my $parse_depth	= shift || 0;
	my @conditions;
 	CONDITION: for my $condition_expr (@{ $item }) {
		if (ref($condition_expr) eq 'ARRAY') {
			push @conditions => $self->_parse_conditions($condition_expr,$parse_depth+1);
			next CONDITION;
		}
		if ($condition_expr =~ /^(O|A)$/) {
			push @conditions => $condition_expr;
			next CONDITION;
		}
		my ($fld, $rel, $val)	= $condition_expr =~ m/^(.*?) ?([:=><~!^#*@]{1,2}) ?(.*)$/s;
		if ($fld =~ /^(.+?)([+-])$/) {
			my $fldbase		= $1;
			my $inner_op	= ($2 eq '+' ? 'O' : 'A');
			my @cond_flds	= $self->matched_fields($fldbase);
			unless (@cond_flds > 0) {
				my %cur_cond		= $self->_sttmts_to_tests(qq{Unknown Field basename "$fldbase"}, $rel, $val);
				$cur_cond{depth}	= $parse_depth;
				$cur_cond{error}	= 1;
				my @subconditions 	= ('S');
				push @subconditions => \%cur_cond;
				push @conditions 	=> $self->_assemble_test_subs(@subconditions);				
				next CONDITION;
			}
			my @subconditions	= ($inner_op);
			for my $cfld (@cond_flds) {
				my %cur_cond		= $self->_sttmts_to_tests($cfld, $rel, $val);
				$cur_cond{depth}	= $parse_depth;
				push @subconditions => \%cur_cond;
			}
 			push @conditions => $parse_depth > 0 ? $self->_assemble_test_subs(@subconditions) : $self->_final_selector(@subconditions);
		} else {
			#next CONDITION unless $self->field($fld);
			my @subconditions	= ('S');
			my %cur_cond		= $self->_sttmts_to_tests($fld, $rel, $val);
			$cur_cond{depth}	= $parse_depth;
			push @subconditions => \%cur_cond;
 			push @conditions => $parse_depth > 0 ? $self->_assemble_test_subs(@subconditions) : $self->_final_selector(@subconditions);
		}
	}
 	return $parse_depth > 1 ? $self->_assemble_test_subs(@conditions) : $self->_final_selector(@conditions);
}

# used inside _parse_conditions
sub _sttmts_to_tests {
	my $self	= shift;
	my ($fld, $rel, $val)	= @_;
	my %current_condition	= (
		fld		=> $fld,
		rel		=> $rel,
		val		=> $val,
		pre		=> '',
		reg		=> qr{},
		sep		=> $self->{_input_sep},
		post	=> qr{(?s:$self->{_input_sep}.*|)$self->{_record_sep}},
		isep	=> qr{$self->{_item_sep}},
		test	=> sub { return },
		phrase	=> 'Incomplete Condition',
		canon	=> join '' => ($fld, $rel, $val),
	);
	
	
	my $op	= $self->_operators(\%current_condition);
	$current_condition{test}	= $op->{test};
	$current_condition{evalstr}	= $op->{evalstr};
	$current_condition{symbol}	= $op->{symbol};
	$current_condition{phrase}	= $op->{phrase};
	$current_condition{error}	= $op->{error} || 0;

	return %current_condition;
}

sub _assemble_test_subs {
	my $self	= shift;
	my @tests	= @_;
	my %test;
	my $has_error	= 0;
	$test{type}	= 
		@tests > 1 ? 
			shift @tests 
		: @tests == 1 ? 
			ref($tests[0]) eq 'HASH' ? 
				$tests[0]->{type} 
			: 'S' 
		: 'U';
	if ($test{type} eq 'S') {
		$test{test}	= sub {
			return $tests[0]->{test}->($_[0]) ? 1 : 0;
		};
		$test{phrase}	= $tests[0]->{phrase};
		$test{canon}	= $tests[0]->{canon};
		$test{depth}	= $tests[0]->{depth};
		$test{error}	= $tests[0]->{error};
		$has_error		+= $tests[0]->{error};
	} elsif ($test{type} eq 'O') {
		@tests = grep { ref($_->{test}) eq 'CODE' } @tests;
		$test{test}	= sub {
			for (@tests) {
				return 1 if $_->{test}->($_[0]);
			}
			return 0;
		};
		$test{depth}	= join '-' => map { $has_error += $_->{error}; $_->{depth} } @tests;
		$test{phrase}	= '(' . join( ' OR ' => map { $_->{phrase} } @tests ) . ')';
		$test{canon}	= '(' . join( ' OR ' => map { $_->{canon} } @tests ) . ')';
		$test{error}	= $has_error;
	}  elsif ($test{type} eq 'A') {
		$test{test}	= sub {
			for (@tests) {
				return 0 unless $_->{test}->($_[0]);
			}
			return 1;
		};
		$test{depth}	= join '-' => map {$has_error += $_->{error}; $_->{depth} } @tests;
		$test{phrase}	= '(' . join( ' AND ' => map { $_->{phrase} } @tests ) . ')';
		$test{canon}	= '(' . join( ' AND ' => map { $_->{canon} } @tests ) . ')';
		$test{error}	= $has_error;
	} else {
	
	}
	\%test;
}

sub _final_selector {
	my $self	= shift;
	my @tests	= @_;
	my %test;
	my $has_error	= 0;
	$test{type}	= 
		@tests > 1 ? 
			shift @tests 
		: @tests == 1 ? 
			ref($tests[0]) eq 'HASH' ? 
				$tests[0]->{type} 
			: 'S' 
		: 'U';
	if ($test{type} eq 'S') {
		$test{test}	= sub {
			return $tests[0]->{test}->($_[0]) ? 1 : 0;
		};
		$test{phrase}	= $tests[0]->{phrase};
		$test{canon}	= $tests[0]->{canon};
		$test{depth}	= $tests[0]->{depth};
		$test{error}	= $tests[0]->{error};
		$has_error		+= $tests[0]->{error};
	} elsif ($test{type} eq 'O') {
		@tests = grep { ref($_->{test}) eq 'CODE' } @tests;
		$test{test}	= sub {
			for (@tests) {
				return 1 if $_->{test}->($_[0]);
			}
			return 0;
		};
		$test{depth}	= join '-' => map { $has_error += $_->{error}; $_->{depth} } @tests;
		$test{phrase}	= join( ' OR ' => map { $_->{phrase} } @tests );
		$test{canon}	= join( ' OR ' => map { $_->{canon} } @tests );
		$test{error}	= $has_error;
	}  elsif ($test{type} eq 'A') {
		$test{test}	= sub {
			for (@tests) {
				return 0 unless $_->{test}->($_[0]);
			}
			return 1;
		};
		$test{depth}	= join '-' => map {$has_error += $_->{error}; $_->{depth} } @tests;
		$test{phrase}	= join( ' AND ' => map { $_->{phrase} } @tests );
		$test{canon}	= join( ' AND ' => map { $_->{canon} } @tests );
		$test{error}	= $has_error;
	} else {
	
	}
	
	\%test;
}


# Selector dispatch
# operators:  '~', '=~', '@~', '!~', '^', '#', '*', '!*', '=', ':', '!', '>', '<', '!>', '!<', '==', '>>', '<<', '<=', '>=', '!!'
sub _operators {
	my $self	= shift;
 	my ($fld, $rel, $val, $pre, $reg, $post, $sep, $isep)	= @{shift()}{qw/fld rel val pre reg post sep isep/};
	my ($fld_name);
	if ($fld eq '?') {
		$pre		= qr{(?s:.*$sep|)};
		$reg		= qr{$val};
		$fld_name	= "Any Field";
	} elsif ($self->{_hd_nums}->{$fld}) {
		$pre		= "[^$sep]*$sep" x $self->{_hd_nums}->{$fld};
		$reg		= qr{[^$sep]*};
		$fld_name	= $self->{_labels}->{$fld};
	} elsif ($fld =~ /^Unknown Field/) {
		$rel		= 'err';
		$fld_name	= $fld;
	} else {
		$rel		= 'err';
		$fld_name	= qq{Unknown Fieldname "$fld"};
	}
	
	$rel			||= 'err';

	my %ops = (
	
	'err'	=> {	
					error	=> 1,
					test	=> sub { return },
					evalstr	=> qq{ 0 },
					phrase	=> "Error: Testing for $val with $fld_name",
					symbol	=> '',			
					fldname	=> $fld_name,
				},

	'~'		=> {	test	=> sub { $_[0] =~ m/^$pre[^$sep]*$val[^$sep]*$post$/si },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre[^\$sep]*\$val[^\$sep]*\$post\$/si },
					phrase	=> join( ' ' => $fld_name,'contains',$val),
					symbol	=> '~',
					fldname	=> $fld_name,
				},

	'=~'	=> {	test	=> sub { $_[0] =~ m/^$pre$val$post$/si },
 					evalstr	=> qq{ \$_[0] =~ m/^\$pre\$val\$post\$/si },
					phrase => join( ' ' => $fld_name,'exactly matches',$val),
					symbol	=> '=~',
					fldname	=> $fld_name,
				},

	':~'	=> {	test	=> sub { $_[0] =~ m/^$pre$val$post$/s },
 					evalstr	=> qq{ \$_[0] =~ m/^\$pre\$val\$post\$/s },
					phrase => join( ' ' => $fld_name,'exactly matches by case',$val),
					symbol	=> ':~',
					fldname	=> $fld_name,
				},

	'@~'	=> {	test	=> sub { $_[0] =~ m/^$pre(?:.*$isep ?|)$val(?:$isep.*)?$post$/si },
 					evalstr	=> qq{ \$_[0] =~ m/^\$pre(?:.*$isep ?|)\$val(?:$isep.*)?\$post\$/si },
					phrase => join( ' ' => $fld_name,'has an item exactly matching',$val),
					symbol	=> '@~',
					fldname	=> $fld_name,
				},

	'!~'	=> {	test	=> sub { $_[0] !~ m/^$pre[^$sep]*$val[^$sep]*$post$/si },
					evalstr	=> qq{ \$_[0] !~ m/^\$pre[^\$sep]*\$val[^\$sep]*\$post\$/si },
					phrase => join( ' ' => $fld_name,'does not contain',$val),
					symbol	=> '!~',
					fldname	=> $fld_name,
				},

	'^'		=> {	test	=> sub { $_[0] =~ m/^$pre$val/i },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre\$val/i },
					phrase => join( ' ' => $fld_name,'starts with',$val),
					symbol	=> '^',
					fldname	=> $fld_name,
				},

	'#'		=> {	test	=> sub { $_[0] =~ m/^$pre[^$sep]*$val$post$/si },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre[^\$sep]*\$val\$post\$/si },
					phrase => join( ' ' => $fld_name,'ends with',$val),
					symbol	=> '#',
					fldname	=> $fld_name,
				},

	'*'		=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, $1 && $1 =~ /\S/  },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s, \$1 && \$1 =~ /\\S/  },
					phrase => join( ' ' => $fld_name,'is not blank',$val),
					symbol	=> '*',
					fldname	=> $fld_name,
				},

	'!*'	=> {	test	=> sub { $_[0] =~ m/^$pre\s*$post$/s },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre\\s*\$post\$/s },
					phrase => join( ' ' => $fld_name,'is blank',$val),
					symbol	=> '!*',
					fldname	=> $fld_name,
				},

	'='		=> {	test	=> ($fld eq '?' ?
								sub { $_[0] =~ m/^$pre($reg)$post$/s ? 1 : 0 } :
								sub {
								$_[0] =~ m/^$pre($reg)$post$/s,
									$val eq 'TRUE' ? ( ($1 and $1 ne $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'FALSE' ? ( (!$1 or $1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'BLANK' ? ( (!$1 or $1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
											$val eq 'ZERO' ? (defined($1) && !$1 && ~$1 ? 1 : 0) :
												uc($1) eq uc($val)
												}),
					evalstr	=> qq{ \$line =~ m\/\^$pre\($reg\)$post\$\/s;
									$val eq 'TRUE' ? ( (\$1 and \$1 ne $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'FALSE' ? ( (!\$1 or \$1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'BLANK' ? ( (!\$1 or \$1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
											$val eq 'ZERO' ? (defined(\$1) && !\$1 && ~\$1 ? 1 : 0) :
												uc(\$1) eq uc($val) },
					phrase => join( ' ' => $fld_name,'is',$val),
					symbol	=> 'eq',
					fldname	=> $fld_name,
				},
				# alt form of '='
	':'		=> {	test	=> ($fld eq '?' ?
								sub { $_[0] =~ m/^$pre($reg)$post$/s ? 1 : 0 } :
								sub { $_[0] =~ m/^$pre($reg)$post$/s,
									$val eq 'TRUE' ? ( ($1 and $1 ne $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'FALSE' ? ( (!$1 or $1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'BLANK' ? ( (!$1 or $1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
											$val eq 'ZERO' ? (defined($1) && !$1 && ~$1 ? 1 : 0) :
												uc($1) eq uc($val)
												}),
					evalstr	=> qq{ \$line =~ m\/\^$pre\($reg\)$post\$\/s;
									$val eq 'TRUE' ? ( (\$1 and \$1 ne $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'FALSE' ? ( (!\$1 or \$1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
										$val eq 'BLANK' ? ( (!\$1 or \$1 eq $self->{_nulls}->{$fld}) ? 1 : 0) :
											$val eq 'ZERO' ? (defined(\$1) && !\$1 && ~\$1 ? 1 : 0) :
												uc(\$1) eq uc($val) },
					phrase => join( ' ' => $fld_name,'is',$val),
					symbol	=> 'eq',
					fldname	=> $fld_name,
				},

	'!'		=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s,
									$val eq 'BLANK' ? ( (!$1 or $1 eq $self->{_nulls}->{$fld}) ? 0 : 1) :
										$1 ne $val },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s,
									\$val eq 'BLANK' ? ( (!\$1 or \$1 eq $self->{_nulls}->{$fld}) ? 0 : 1) :
										\$1 ne \$val },
					phrase => join( ' ' => $fld_name,'is not',$val),
					symbol	=> 'ne',
					fldname	=> $fld_name,
				},
				# alias for '!'
	'!='		=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s,
									$val eq 'BLANK' ? ( (!$1 or $1 eq $self->{_nulls}->{$fld}) ? 0 : 1) :
										$1 ne $val },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s,
									\$val eq 'BLANK' ? ( (!\$1 or \$1 eq $self->{_nulls}->{$fld}) ? 0 : 1) :
										\$1 ne \$val },
					phrase => join( ' ' => $fld_name,'is not',$val),
					symbol	=> 'ne',
					fldname	=> $fld_name,
				},

	'>'		=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, uc($1) gt uc($val) },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s, uc(\$1) gt uc(\$val) },
					phrase => join( ' ' => $fld_name,'is after',$val),
					symbol	=> 'gt',
					fldname	=> $fld_name,
				},

	'<'		=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, uc($1) lt uc($val) },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s, uc(\$1) lt uc(\$val) },
					phrase => join( ' ' => $fld_name,'is before',$val),
					symbol	=> 'lt',
					fldname	=> $fld_name,
				},

	'!>'	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, $1 le $val },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s, \$1 le \$val },
					phrase => join( ' ' => $fld_name,'is not after',$val),
					symbol	=> 'le',
					fldname	=> $fld_name,
				},

	'!<'	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, $1 ge $val },
					evalstr	=> qq{ \$_[0] =~ m/^$pre($reg)$post$/s, $1 ge $val },
					phrase => join( ' ' => $fld_name,'is not before',$val),
					symbol	=> 'ge',
					fldname	=> $fld_name,
				},

	'=='	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, ($1||0) == $val },
					evalstr	=> qq{ \$_[0] =~ m/^$pre($reg)$post$/s, ($1||0) == $val },
					phrase => join( ' ' => $fld_name,'equals',$val),
					symbol	=> '=',
					fldname	=> $fld_name,
				},

	'>>'	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, ($1||0) > $val },
					evalstr	=> qq{ \$_[0] =~ m/^$pre($reg)$post$/s, ($1||0) > $val },
					phrase => join( ' ' => $fld_name,'is greater than',$val),
					symbol	=> '>',
					fldname	=> $fld_name,
				},

	'<<'	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, ($1||0) < $val },
					evalstr	=> qq{ \$_[0] =~ m/^$pre($reg)$post$/s, ($1||0) < $val },
					phrase => join( ' ' => $fld_name,'is less than',$val),
					symbol	=> '<',
					fldname	=> $fld_name,
				},

	'<='	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, ($1||0) <= $val },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s, (\$1||0) <= \$val },
					phrase => join( ' ' => $fld_name,'is not more than',$val),
					symbol	=> '<=',
					fldname	=> $fld_name,
				},

	'>='	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, ($1||0) >= $val },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s, (\$1||0) >= \$val },
					phrase => join( ' ' => $fld_name,'is at least',$val),
					symbol	=> '>=',
					fldname	=> $fld_name,
				},

	'!!'	=> {	test	=> sub { $_[0] =~ m/^$pre($reg)$post$/s, ($1||0) != $val },
					evalstr	=> qq{ \$_[0] =~ m/^\$pre(\$reg)\$post\$/s, (\$1||0) != \$val },
					phrase => join( ' ' => $fld_name,'does not equal',$val),
					symbol	=> '!=',
					fldname	=> $fld_name,
				},

	);

	return $ops{$rel};
}

sub parens_balanced {
	my $self		= shift;
	my $try_where	= shift;	
	return 1 unless $try_where =~ /[()]/;
	my $lps	= $try_where =~ s/\(/\(/g;
	my $rps	= $try_where =~ s/\)/\)/g;
	return ($lps == $rps);
}



1;
