package BVA::XUI::UTILS;

$BVA::XUI::UTILS::VERSION = '1.02_001';    # 2013-03-31

use strict;

BEGIN {
	eval { use warnings qw/all/ };
}

sub convert_file_date {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);
	my @mon_nums  = qw/00 01 02 03 04 05 06 07 08 09 10 11 12/;
	my @mon_abbrs = qw/Month Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec/;
	my ( $yy, $mm ) = split /\./ => shift();

	return ( $yy + 2000, $mon_abbrs[$mm] );
}

# Put a list into a sentence clause
sub string_list {
	my $self = shift;
	if ( @_ == 0 ) {
		"";
	}
	elsif ( @_ == 1 ) {
		$_[0];
	}
	elsif ( @_ == 2 ) {
		"$_[0] and $_[1]";
	}
	else {
		join( ', ' => @_[ 0 .. $#_ - 1 ], "and $_[-1]" );
	}
}

# convert a cardinal number to its ordinal
sub ordinalize {
	my $self   = shift;
	my $number = shift;
	my $suffix = '';
	return $number unless $number =~ /\d+$/;
	if ( $number =~ /(?<!1)1$/ ) {
		$suffix = 'st';
	}
	elsif ( $number =~ /(?<!1)2$/ ) {
		$suffix = 'nd';
	}
	elsif ( $number =~ /(?<!1)3$/ ) {
		$suffix = 'rd';
	}
	else {
		$suffix = 'th';
	}
	qq{$number$suffix};
}

# make_counter
sub make_counter ($;@) {
	my $self = shift;
	my ( $label, $start, $progress, $increment, $column, $eol ) = @_;

	$label ||= 'Count:';
	$start ||= 0;
	my $count = $start - 1;

	$progress  ||= '.';
	$increment ||= 200;
	$column    ||= 50;
	$eol       ||= "\t#\n";

	my $first_time = 1;
	return sub {
		my $flag = shift() || '';
		return $start if $flag eq 'first';
		return $count if $flag eq 'last';

		if ( $flag eq 'progress' ) {
			return '' if $count == 0;
			my $progress_marker = '';
			$progress_marker .= $progress if ( $count % $increment == 0 );
			if ( $count % ( $column * $increment ) == 0 ) {
				$progress_marker .= $eol;
				$progress_marker =~ s/\#/$count/g;
			}
			return $progress_marker;
		}

		( $flag eq 'labeled' or $flag eq 'last_labeled' )
		  and return "$label  " . $count;

		$first_time
		  ? $first_time-- && "$label  " . ++$count
		  : "\b" x length($count) . ++$count;
	  }
}

=head1 NAME
entrain() -- return reliable element sets from lists

=head1 SYNOPSIS

  my $pairs		= entrain 2, @list;
  while (my ($key,$val) = $pairs->()) { ... }

  my $price_list	= entrain ['','0','call'], qw/Item ID Cost/;
  my $total			= 0;
  while (my @items = $price_list->(@list)) {
	printf qq{%6s %5s %8s\n} => @items;
	$total += $items[2];
  }




=head1 DESCRIPTION

Returns a closure that returns a specified number of elements
of a list each time it is called, stopping when the list is exhausted.
Guarantees that the number of elements returned is always the
number specified and that no undefined list elements will be returned (unless desired).

Takes arguments in three forms:

	entrain $number, @list
	entrain $string, @list
	entrain $arrayref, @list

If the first arg is a number, it will be used as the quantifier for the
number of elements returned.

If the first arg is a string, it must start with a number, which serves as
the quantifier for the number of elements returned.

With a plain number arg, '' will be used for all missing and undefined elements.

The string arg may also be a number followed by a colon followed by an
expression of zero length or longer; the number is again the group quantifier,
and the expression is the default value for all elements. The expression may be white space.

If the first arg is an array ref, the number of array elements is the group quantifier,
and the array elements are the defaults, in order.

With both string and arrayref forms, any default expressions are passed through unchanged,
so they may be references, including code refs. Perl's undef may also be passed as a default
value if desired

Any args passed to the closure are appended once to the list before the next set of elements is returned.
This allows creation of the closure with the quantifier and defaults but without a list, for use later.


=cut

sub entrain ($;@) {
	my $pattern   = shift();
	my @orig_list = @_;

	my $num_elems;
	my @defaults;
	if ( ref($pattern) =~ /array/i ) {
		@defaults  = @{$pattern};
		$num_elems = @defaults;
	}
	elsif ( $pattern =~ /^\s*(\d+)\s*(:(.*))?$/ ) {
		$num_elems = $1;
		my $def = defined($3) ? $3 : '';
		@defaults = ($def) x $num_elems;
	}

	my @padding;
	for (@defaults) {
		if (s/^(.*?)\*$/$1/) {
			push @padding => $_;
		}
		else {
			last;
		}
	}

	my @list = ( @padding, @orig_list );
	my @added;

	return sub {
		if ( @_ and not @added ) {
			@added = @_;
			push @list => @added;
		}

		if ( @list and @defaults ) {
			my @elems = splice @list, 0, $num_elems;
			return map { $elems[$_] || $defaults[$_] } ( 0 .. $num_elems - 1 );
		}

		@list = ( @padding, @orig_list );
		@added = ();
	};
}

# Put a list of alternative match patterns into tight (non-greedy) matching order.
# E.g., TRUSTEES before TRUSTEE before TRUST   TRUSTEES|TRUSTEE|TRUST
sub tight_match_order {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	my @alts = @_;
	return unless @alts;

	my %alts_seen;
	@alts = map { $alts_seen{$_}++ ? () : $_ } @alts;

	my @ordered_alts;

  TEST: do {
		my $alt = shift @alts;
		if ( grep m#$alt#, @alts ) {
			push @alts => $alt;
		}
		else {
			push @ordered_alts => $alt;
		}
	} while @alts;

	@ordered_alts;
}

## Minimal Conversion to & from HTML protected entities
## &, <,  >, "
sub HTML_entify {

	# thanks to Lincoln Stein
	my $self = shift;
	unshift( @_, $self ) unless ref($self);
	my ( $orig, @flags ) = @_;
	return unless defined($orig);
	return $orig if ref($self) && $self->data('_accept_HTML');
	study $orig;
	$orig =~ s{&(?!\S+;)}{&amp;}gso;
	$orig =~ s{<}{&lt;}gso;
	$orig =~ s{>}{&gt;}gso;
	$orig =~ s{"}{&quot;}gso;
	return $orig;
}

sub HTML_de_entify {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);
	my ( $orig, @flags ) = @_;
	return unless defined($orig);
	study $orig;

	# thanks to Lincoln Stein and Randal Schwartz
	# for the correct solution to this one
	$orig =~ s{&(.*?);}
    			{
				local $_ = $1;
				/^amp$/i	? "&" :
				/^quot$/i	? '"' :
				/^gt$/i		? ">" :
				/^lt$/i		? "<" :
				/^nbsp$/i	? " " :
				$_
				}gex;
	return $orig;
}

sub url_encode {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);
	my $arg = shift;
	( my $url = $arg ) =~ s/\s+/%20/g;
	$url =~ s/,/%2C/g;
	$url =~ s/(&)([^a])/&amp;$2/g;
	return $url;
}

sub url_decode {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);
	my $arg = shift;
	( my $url = $arg ) =~ s/%20/ /g;
	$url =~ s/%2C/,/g;
	$url =~ s/&amp;/&/g;
	return $url;
}

## URL converter  ###########################
# by Bruce Van Allen, bva@cruzio.com
# after Christiansen & Torkington
#
# rev 1.2  7/13/01
#
# Converts most URLs within a text to HTML links.
# Also converts most plain email addresses
#
# Rules:
#	- URLs must start with a protocol (http, ftp, etc)
#	   unless an assumed URL without protocol is the entire text;
#	- URLs & email bounded by < and > will be converted;
#	- URLs & email bounded by quotes (" or ') will *not* be converted;
#	- existing link markup left alone;
#	- other markup preserved;

sub urlify {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);
	my ( $text, $guess_ok, $target, $urls, $ltrs, $gunk, $punc, $ante, $any );

	# Get the original text
	return $text unless $text = shift;
	return '' if $text =~ /^\s*$/;
	$guess_ok = shift || '';
	$target   = shift || '_self';

	# Make some regex pieces
	$urls = '(https?|file|ftp|mailto|afp)';
	$ltrs = '\w';
	$gunk = '/#~:.?+=&%@!\-';                 # $gunk & $punc overlap
	$punc = '.:?\-';
	$ante = '=\"\'';
	$any  = "${ltrs}${gunk}${punc}";

	## Special case to add http/mailto protocol to assumed url
	if ( $guess_ok and $text =~ /^\s*([$any]+)\s*$/ ) {
		my $addr = $1;
		if ( $addr !~ /^$urls:/ ) {
			$addr =~ /\@/ and $text = qq{<a href="mailto:$addr">$addr</a>}
			  or $text = qq{<a href="http://$addr">$addr</a>};
		}
	}

	# First convert plain email addresses
	$text =~
s{((\s*<?)(mailto:)?\b([${ltrs}${punc}]+@[${any}]+?)\s*(?!</a>)(?=[$punc]*[^${any}]|$))}
				{qq{$2<a href="mailto:$4">$4</a>} }egoi;

	# Convert URLs
	$text =~
s{(([$ante]?)(\s*<?)\b(${urls}:[$any]+?)\s*(?!</a>)(>|\&gt;)?(?=[$punc]*[^$any]|$))}
				{$2 ? $1 : qq{$3<a href="$4" target="$target">$4</a>$6}}egoi;

	$text =~ s/ $//g;    # ****

	return $text;
}

sub extract_urls {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);
	my ( $text, $urls, $ltrs, $gunk, $punc, $ante, $any );

	# Get the original text
	return '' unless $text = shift;
	return '' if $text =~ /^\s*$/;

	# Make some regex pieces
	$urls = '(http|file|ftp|mailto|afp)';
	$ltrs = '\w';
	$gunk = '/#~:.?+=&%@!\-';               # $gunk & $punc overlap
	$punc = '.:?\-';
	$ante = '=\"\'';
	$any  = "${ltrs}${gunk}${punc}";

	my %url_hash;
	my $url_counter = "url000";

	# First convert plain email addresses
	$text =~
s{((\s*<?)(mailto:)?\b([${ltrs}${punc}]+@[${any}]+?)\s*(?!</a>)(?=[$punc]*[^${any}]|$))}
				{
					if ($3) {
						$1;
					} else {
						qq{$2mailto:$4};
					}
				}egoi;

	# Extract URLs
	$text =~ s{(([$ante]?)\b(${urls}:[$any]+?)\s*(?!</a>)(?=[$punc]*[^$any]|$))}
				{
					$url_counter++;
					if ($2) {
						$url_hash{$url_counter}	= $1;
					} else {
						$url_hash{$url_counter}	= $3;
					}
					qq{[$url_counter]};
				}egoi;

	return ( {%url_hash}, $text );
}

sub text_to_html {
	my $self = shift;
	unshift( @_, $self ) unless ref($self);

	my $text = shift || "";

	return unless $text;

	## A little housecleaning
	# Remove extra white space at end; then add back consistent newlines
	$text =~ s/\s*$//;

	# Protect URLs by extracting them during processing
	my $urls;
	( $urls, $text ) = $self->extract_urls($text);

	# Convert '\' to "\n" (newline)
	$text =~ s{\\}{\n}gs;

	open my $strh, "<", \$text
	  or return;

	my @lines;
	PARSE: {
		local $/ = "\n\n";
		while (<$strh>) {
			chomp;
			s/^\n*(.*?)\s*$/$1/;
			next unless $_;
			push @lines => $_;
		}
	}

	close $strh;

	my @formatted_lines;

	for (@lines) {

		my $line = $_;

		# Conversions

		# Convert ampersand (&) to HTML entity
		$line =~ s{&(?!\S+;)}{&amp;}gso;

		# Convert left angle bracket (<) to HTML entity
		$line =~ s{<}{&lt;}gso;

		# Convert right angle bracket (>) to HTML entity
		$line =~ s{>}{&gt;}gso;

		# Convert exclamation point (!) at start of word to HTML entity for inverted (¡)
		$line =~ s{(?<![A-Za-z0-9])\!(?=[A-Za-z0-9])}{&iexcl;}gso;

		# Convert question mark (?) at start of word to HTML entity for inverted (¿)
		$line =~ s{(?<![A-Za-z0-9])\?(?=[A-Za-z0-9])}{&iquest;}gso;

		# Convert double quotes at start of word to double left curly quote HTML entities
		$line =~ s{([^\S\\])\"(?=[A-Za-z0-9])}
									{"$1&ldquo;"}gse;

		# Convert double quotes at end of word to double right curly quote HTML entities
		$line =~ s{(?<=[A-Za-z0-9.,;:?!])\"([^A-Za-z0-9>])}
									{"&rdquo;$1"}gse;

		# Convert any other double quotes to double straight quote HTML entities
		$line =~ s{"}
									{"&quot;"}gse;

		# Convert single quotes used as apostrophes to single right quote HTML enities
		$line =~ s{(?<=[A-Za-z0-9])\'(?=[A-Za-z0-9])}
									{"&rsquo;"}gse;

		# Convert single quotes at start of word to single left quote HTML enities
		$line =~ s{([^\S\\])\'(?=[A-Za-z0-9])}
									{"$1&lsquo;"}gse;

		# Convert single quotes at end of word to single right quote HTML enities
		$line =~ s{(?<=[A-Za-z0-9.,;:?!])\'([^A-Za-z0-9>])}
									{"&rsquo;$1"}gse;

		# Convert two or more hyphens on otherwise blank line immediately following a text line
		# to partial width horizontal rule <hr width='25%' align='left'>
		# Four or more spaces before hyphens make rule centered align='center'
		$line =~ s{(?<=[a-zA-Z0-9.,;:?!])\n+ *?(    +)?--+ *(\n|$)}
									{"\n<hr" . ($1 ? q{ align="center"} : q{ align="left"}) . " width=\'25%\'>\n"}gse;
		# Convert two or more hyphens on otherwise blank line to horizontal rule <hr>
		$line =~ s{(^|\n) *--+ *(\n|$)}
					{"$1<hr>$2"}gse;
		# Convert single hyphens at start of line to bullets
		$line =~ s{((?:^|\n) *)-}
					{"$1&bull;"}gse;

		# Formatting

		# Make headings from lines starting with '*'
		# Four asterisks - largest heading <h1>
		# One asterisk - smallest heading <h4>
		# Four or more spaces before askerisk make heading centered
		$line =~ s{(^|\n)(    +)?([\*]+) *(.+?) *([\*]+)? *$}
					{
						my $num	= ($3 eq '****' ? '1' : $3 eq '***' ? '2' : $3 eq '**' ? 3 : 4);
						qq{<h$num} . ($2 ? q{ align="center"} : "") . qq{>\n$4\n</h$num>}
					}se;

		# Make blockquotes from lines starting with at least 4 spaces
		# Replace any internal newlines with <br>
		$line =~ s{(^|\n)    +(.*)$}
					{
						if($2) {
							my $t = $2;
							$t =~ s/\n/"<br>\n"/esg;
							qq{<blockquote>\n$t\n</blockquote>}
						} else {
							""
						}
					}se;

		# Convert single paragraph returns to line breaks <br>
		#		$line			=~ s{(.+?(?!\>)) *\n *([^<]\S+)}
		#  		$line			=~ s{(\S[^>]) *\n *([^<]\S+)}
		$line =~ s{([^>]) *\n *([^<]\S+)}
					{"$1\n<br>\n$2"}ge;

		# Put paragraph tags <p> around sections not already marked as blocks
		$line =~ s{^ *([^<].+)$}
					{$1 ? qq{<p>\n$1\n</p>} : ""}se;

		# Format short phrases at start of line followed by a colon & space/tab in bold type
		$line =~
s{((?:^|\n) *)((?!(?:<|\&lt;)?http)[A-Za-z0-9._•\&-][ A-Za-z0-9.;_•\&-]{1,16}[A-Za-z0-9.;_•\&-]:[\t ])}
					{"$1<b>$2</b>"}gse;

		push @formatted_lines => $line

	}

	my $formatted_msg = join "\n\n" => @formatted_lines;

	# Restore extracted URLs, formatted as hyperlinks
	for ( keys %{$urls} ) {
		$formatted_msg =~
		  s{(<|\&lt;)?\[$_\](>|\&gt;)?}{ $self->urlify($urls->{$_}) }e;
	}

	return $formatted_msg;

}    # end text_to_html()

sub std_colors {
	my $self = shift;
	[
		qw/
		  aqua
		  black
		  blue
		  fuchsia
		  gray
		  green
		  lime
		  maroon
		  navy
		  olive
		  purple
		  red
		  silver
		  teal
		  yellow
		  white
		  /
	];
}

sub web_safe_colors {
	my $self = shift;

	my @web_hex_keys =
	  grep { /#([a-f0-9])\1([a-f0-9])\2([a-f0-9])\3/ }
	  keys %BVA::XUI::UTILS::HEXES_PLUS;

	[ @BVA::XUI::UTILS::HEXES_PLUS{@web_hex_keys} ];
}

sub color_to_hex {
	my $self = shift;
	my @colors =
	    ref( $_[0] ) =~ 'ARRAY' ? @{ $_[0] }
	  : @_                      ? @_
	  :                           sort keys %BVA::XUI::UTILS::COLORS;

	my @color_errors;

	my @hexes;
  COLOR: for (@colors) {
		if ( exists $BVA::XUI::UTILS::COLORS{$_} ) {
			push @hexes => $BVA::XUI::UTILS::COLORS{$_};
			next COLOR;
		}

		my @color_try_words = split ' ' => $_;
		@color_try_words = map { ucfirst( lc($_) ) } @color_try_words;
		my $color_try = join '' => @color_try_words;

		if ( exists $BVA::XUI::UTILS::COLORS{$color_try} ) {
			push @hexes => $BVA::XUI::UTILS::COLORS{$color_try};
			next COLOR;
		}

		push @hexes        => '#999999';
		push @color_errors => $_;
	}

	wantarray ? @hexes
	  : @color_errors
	  ? 'Color names not recognized: ' . join( ' ' => @color_errors )
	  : 0;

}

sub hex_to_color {
	my $self = shift;
	my @hexes =
	    ref( $_[0] ) =~ 'ARRAY' ? @{ $_[0] }
	  : @_                      ? @_
	  :                           sort keys %BVA::XUI::UTILS::HEXES;

	my @hex_errors;

	my @colors;
  HEX: for (@hexes) {
		$_ = lc($_);
		if ( exists $BVA::XUI::UTILS::HEXES{"$_"} ) {
			push @colors => $BVA::XUI::UTILS::HEXES{"$_"};
			next HEX;
		}

		push @colors     => 'Gray';
		push @hex_errors => $_;
	}

	wantarray ? @colors
	  : @hex_errors ? 'Hex names not recognized: ' . join( ' ' => @hex_errors )
	  :               0;

}

sub color_to_hex_plus {
	my $self = shift;
	my @colors =
	    ref( $_[0] ) =~ 'ARRAY' ? @{ $_[0] }
	  : @_                      ? @_
	  :                           sort keys %BVA::XUI::UTILS::COLORS_PLUS;

	my @color_errors;

	my @hexes;
	COLOR: for my $clr (@colors) {
		if ( exists $BVA::XUI::UTILS::COLORS_PLUS{$clr} ) {
			push @hexes => $BVA::XUI::UTILS::COLORS_PLUS{$clr};
			next COLOR;
		}

		my @color_try_words = split ' ' => $clr;
		@color_try_words = map { ucfirst( lc($_) ) } @color_try_words;
		my $color_try = join '' => @color_try_words;

		if ( exists $BVA::XUI::UTILS::COLORS_PLUS{$color_try} ) {
			push @hexes => $BVA::XUI::UTILS::COLORS_PLUS{$color_try};
			next COLOR;
		}

		if ( $clr =~ /^\#?([0-9a-f]{3}([0-9a-f]{3})?)$/i ) {
			my $hex = $1;
			if ( $hex =~ /^([0-9a-f])([0-9a-f])([0-9a-f])$/i ) {
				$hex = "$1$1$2$2$3$3";
			}
			my ($colorFromHex) = $self->hex_to_color_plus("#$hex");
			if ( exists $BVA::XUI::UTILS::COLORS_PLUS{$colorFromHex} ) {
				push @hexes => $BVA::XUI::UTILS::COLORS_PLUS{$colorFromHex};
			}
			else {
				push @hexes => "#$hex";
			}
			next COLOR;
		}

		push @hexes        => '#999999';
		push @color_errors => $clr;
	}

	wantarray ? @hexes
	  : @color_errors
	  ? 'Color names not recognized: ' . join( ' ' => @color_errors )
	  : 0;

}

sub hex_to_color_plus {
	my $self = shift;
	my @hexes =
	    ref( $_[0] ) =~ 'ARRAY' ? @{ $_[0] }
	  : @_                      ? @_
	  :                           sort keys %BVA::XUI::UTILS::HEXES_PLUS;

	my @hex_errors;

	my @colors;
	HEX: for (@hexes) {
		$_ = lc($_);
		if ( exists $BVA::XUI::UTILS::HEXES_PLUS{"$_"} ) {
			push @colors => $BVA::XUI::UTILS::HEXES_PLUS{"$_"};
			next HEX;
		}

		push @colors     => 'Gray';
		push @hex_errors => $_;
	}

	wantarray ? @colors
	  : @hex_errors ? 'Hex names not recognized: ' . join( ' ' => @hex_errors )
	  :               0;

}

sub hex_to_color_w_hex_plus {
	my $self = shift;
	my @hexes =
	    ref( $_[0] ) =~ 'ARRAY' ? @{ $_[0] }
	  : @_                      ? @_
	  :                           sort keys %BVA::XUI::UTILS::HEXES_PLUS;

	my @hex_errors;

	my @colors;
  HEX: for (@hexes) {
		my $hex = lc($_);
		if ( exists $BVA::XUI::UTILS::HEXES_PLUS{$hex} ) {
			my $color =
			    $BVA::XUI::UTILS::HEXES_PLUS{$hex} =~ /$hex/i
			  ? $hex
			  : "$BVA::XUI::UTILS::HEXES_PLUS{$hex} ($hex)";
			push @colors => $color;
			next HEX;
		}

		push @colors => "Unknown ($_)";

		#push @hex_errors => $_;
	}

	wantarray ? @colors
	  : @hex_errors ? 'Hex names not recognized: ' . join( ' ' => @hex_errors )
	  :               0;

}

BEGIN {

	%BVA::XUI::UTILS::COLORS = (
		AliceBlue            => '#f0f8ff',
		AntiqueWhite         => '#faebd7',
		Aqua                 => '#00ffff',
		Aquamarine           => '#7fffd4',
		Azure                => '#f0ffff',
		Beige                => '#f5f5dc',
		Bisque               => '#ffe4c4',
		Black                => '#000000',
		BlanchedAlmond       => '#ffebcd',
		Blue                 => '#0000ff',
		BlueViolet           => '#8a2be2',
		Brown                => '#a52a2a',
		BurlyWood            => '#deb887',
		CadetBlue            => '#5f9ea0',
		Chartreuse           => '#7fff00',
		Chocolate            => '#d2691e',
		Coral                => '#ff7f50',
		CornflowerBlue       => '#6495ed',
		Cornsilk             => '#fff8dc',
		Crimson              => '#dc143c',
		Cyan                 => '#00ffff',
		DarkBlue             => '#00008b',
		DarkCyan             => '#008b8b',
		DarkGoldenRod        => '#b8860b',
		DarkGray             => '#a9a9a9',
		DarkGreen            => '#006400',
		DarkKhaki            => '#bdb76b',
		DarkMagenta          => '#8b008b',
		DarkOliveGreen       => '#556b2f',
		Darkorange           => '#ff8c00',
		DarkOrchid           => '#9932cc',
		DarkRed              => '#8b0000',
		DarkSalmon           => '#e9967a',
		DarkSeaGreen         => '#8fbc8f',
		DarkSlateBlue        => '#483d8b',
		DarkSlateGray        => '#2f4f4f',
		DarkTurquoise        => '#00ced1',
		DarkViolet           => '#9400d3',
		DeepPink             => '#ff1493',
		DeepSkyBlue          => '#00bfff',
		DimGray              => '#696969',
		DodgerBlue           => '#1e90ff',
		Feldspar             => '#d19275',
		FireBrick            => '#b22222',
		FloralWhite          => '#fffaf0',
		ForestGreen          => '#228b22',
		Fuchsia              => '#ff00ff',
		Gainsboro            => '#dcdcdc',
		GhostWhite           => '#f8f8ff',
		Gold                 => '#ffd700',
		GoldenRod            => '#daa520',
		Gray                 => '#808080',
		Green                => '#008000',
		GreenYellow          => '#adff2f',
		HoneyDew             => '#f0fff0',
		HotPink              => '#ff69b4',
		IndianRed            => '#cd5c5c',
		Indigo               => '#4b0082',
		Ivory                => '#fffff0',
		Khaki                => '#f0e68c',
		Lavender             => '#e6e6fa',
		LavenderBlush        => '#fff0f5',
		LawnGreen            => '#7cfc00',
		LemonChiffon         => '#fffacd',
		LightBlue            => '#add8e6',
		LightCoral           => '#f08080',
		LightCyan            => '#e0ffff',
		LightGoldenRodYellow => '#fafad2',
		LightGrey            => '#d3d3d3',
		LightGreen           => '#90ee90',
		LightPink            => '#ffb6c1',
		LightSalmon          => '#ffa07a',
		LightSeaGreen        => '#20b2aa',
		LightSkyBlue         => '#87cefa',
		LightSlateBlue       => '#8470ff',
		LightSlateGray       => '#778899',
		LightSteelBlue       => '#b0c4de',
		LightYellow          => '#ffffe0',
		Lime                 => '#00ff00',
		LimeGreen            => '#32cd32',
		Linen                => '#faf0e6',
		Magenta              => '#ff00ff',
		Maroon               => '#800000',
		MediumAquaMarine     => '#66cdaa',
		MediumBlue           => '#0000cd',
		MediumOrchid         => '#ba55d3',
		MediumPurple         => '#9370d8',
		MediumSeaGreen       => '#3cb371',
		MediumSlateBlue      => '#7b68ee',
		MediumSpringGreen    => '#00fa9a',
		MediumTurquoise      => '#48d1cc',
		MediumVioletRed      => '#c71585',
		MidnightBlue         => '#191970',
		MintCream            => '#f5fffa',
		MistyRose            => '#ffe4e1',
		Moccasin             => '#ffe4b5',
		NavajoWhite          => '#ffdead',
		Navy                 => '#000080',
		OldLace              => '#fdf5e6',
		Olive                => '#808000',
		OliveDrab            => '#6b8e23',
		Orange               => '#ffa500',
		OrangeRed            => '#ff4500',
		Orchid               => '#da70d6',
		PaleGoldenRod        => '#eee8aa',
		PaleGreen            => '#98fb98',
		PaleTurquoise        => '#afeeee',
		PaleVioletRed        => '#d87093',
		PapayaWhip           => '#ffefd5',
		PeachPuff            => '#ffdab9',
		Peru                 => '#cd853f',
		Pink                 => '#ffc0cb',
		Plum                 => '#dda0dd',
		PowderBlue           => '#b0e0e6',
		Purple               => '#800080',
		Red                  => '#ff0000',
		RosyBrown            => '#bc8f8f',
		RoyalBlue            => '#4169e1',
		SaddleBrown          => '#8b4513',
		Salmon               => '#fa8072',
		SandyBrown           => '#f4a460',
		SeaGreen             => '#2e8b57',
		SeaShell             => '#fff5ee',
		Sienna               => '#a0522d',
		Silver               => '#c0c0c0',
		SkyBlue              => '#87ceeb',
		SlateBlue            => '#6a5acd',
		SlateGray            => '#708090',
		Snow                 => '#fffafa',
		SpringGreen          => '#00ff7f',
		SteelBlue            => '#4682b4',
		Tan                  => '#d2b48c',
		Teal                 => '#008080',
		Thistle              => '#d8bfd8',
		Tomato               => '#ff6347',
		Turquoise            => '#40e0d0',
		Violet               => '#ee82ee',
		VioletRed            => '#d02090',
		Wheat                => '#f5deb3',
		White                => '#ffffff',
		WhiteSmoke           => '#f5f5f5',
		Yellow               => '#ffff00',
		YellowGreen          => '#9acd32',
	);

	%BVA::XUI::UTILS::HEXES = reverse %BVA::XUI::UTILS::COLORS;

	%BVA::XUI::UTILS::HEXES_PLUS = (
		'#000000' => 'Black',
		'#000033' => '#000033',
		'#000066' => '#000066',
		'#000080' => 'Navy',
		'#00008b' => 'DarkBlue',
		'#000099' => '#000099',
		'#0000cc' => '#0000cc',
		'#0000cd' => 'MediumBlue',
		'#0000ff' => 'Blue',
		'#003300' => '#003300',
		'#003333' => '#003333',
		'#003366' => '#003366',
		'#003399' => '#003399',
		'#0033cc' => '#0033cc',
		'#0033ff' => '#0033ff',
		'#006400' => 'DarkGreen',
		'#006600' => '#006600',
		'#006633' => '#006633',
		'#006666' => '#006666',
		'#006699' => '#006699',
		'#0066cc' => '#0066cc',
		'#0066ff' => '#0066ff',
		'#008000' => 'Green',
		'#008080' => 'Teal',
		'#008b8b' => 'DarkCyan',
		'#009900' => '#009900',
		'#009933' => '#009933',
		'#009966' => '#009966',
		'#009999' => '#009999',
		'#0099cc' => '#0099cc',
		'#0099ff' => '#0099ff',
		'#00bfff' => 'DeepSkyBlue',
		'#00cc00' => '#00cc00',
		'#00cc33' => '#00cc33',
		'#00cc66' => '#00cc66',
		'#00cc99' => '#00cc99',
		'#00cccc' => '#00cccc',
		'#00ccff' => '#00ccff',
		'#00ced1' => 'DarkTurquoise',
		'#00fa9a' => 'MediumSpringGreen',
		'#00ff00' => 'Lime',
		'#00ff33' => '#00ff33',
		'#00ff66' => '#00ff66',
		'#00ff7f' => 'SpringGreen',
		'#00ff99' => '#00ff99',
		'#00ffcc' => '#00ffcc',
		'#00ffff' => 'Cyan',
		'#191970' => 'MidnightBlue',
		'#1e90ff' => 'DodgerBlue',
		'#20b2aa' => 'LightSeaGreen',
		'#228b22' => 'ForestGreen',
		'#2e8b57' => 'SeaGreen',
		'#2f4f4f' => 'DarkSlateGray',
		'#2f4f4f' => 'DarkSlateGrey',
		'#32cd32' => 'LimeGreen',
		'#330000' => '#330000',
		'#330033' => '#330033',
		'#330066' => '#330066',
		'#330099' => '#330099',
		'#3300cc' => '#3300cc',
		'#3300ff' => '#3300ff',
		'#333300' => '#333300',
		'#333333' => '#333333',
		'#333366' => '#333366',
		'#333399' => '#333399',
		'#3333cc' => '#3333cc',
		'#3333ff' => '#3333ff',
		'#336600' => '#336600',
		'#336633' => '#336633',
		'#336666' => '#336666',
		'#336699' => '#336699',
		'#3366cc' => '#3366cc',
		'#3366ff' => '#3366ff',
		'#339900' => '#339900',
		'#339933' => '#339933',
		'#339966' => '#339966',
		'#339999' => '#339999',
		'#3399cc' => '#3399cc',
		'#3399ff' => '#3399ff',
		'#33cc00' => '#33cc00',
		'#33cc33' => '#33cc33',
		'#33cc66' => '#33cc66',
		'#33cc99' => '#33cc99',
		'#33cccc' => '#33cccc',
		'#33ccff' => '#33ccff',
		'#33ff00' => '#33ff00',
		'#33ff33' => '#33ff33',
		'#33ff66' => '#33ff66',
		'#33ff99' => '#33ff99',
		'#33ffcc' => '#33ffcc',
		'#33ffff' => '#33ffff',
		'#3cb371' => 'MediumSeaGreen',
		'#40e0d0' => 'Turquoise',
		'#4169e1' => 'RoyalBlue',
		'#4682b4' => 'SteelBlue',
		'#483d8b' => 'DarkSlateBlue',
		'#48d1cc' => 'MediumTurquoise',
		'#4b0082' => 'Indigo',
		'#556b2f' => 'DarkOliveGreen',
		'#5f9ea0' => 'CadetBlue',
		'#6495ed' => 'CornflowerBlue',
		'#660000' => '#660000',
		'#660033' => '#660033',
		'#660066' => '#660066',
		'#660099' => '#660099',
		'#6600cc' => '#6600cc',
		'#6600ff' => '#6600ff',
		'#663300' => '#663300',
		'#663333' => '#663333',
		'#663366' => '#663366',
		'#663399' => '#663399',
		'#6633cc' => '#6633cc',
		'#6633ff' => '#6633ff',
		'#666600' => '#666600',
		'#666633' => '#666633',
		'#666666' => '#666666',
		'#666699' => '#666699',
		'#6666cc' => '#6666cc',
		'#6666ff' => '#6666ff',
		'#669900' => '#669900',
		'#669933' => '#669933',
		'#669966' => '#669966',
		'#669999' => '#669999',
		'#6699cc' => '#6699cc',
		'#6699ff' => '#6699ff',
		'#66cc00' => '#66cc00',
		'#66cc33' => '#66cc33',
		'#66cc66' => '#66cc66',
		'#66cc99' => '#66cc99',
		'#66cccc' => '#66cccc',
		'#66ccff' => '#66ccff',
		'#66cdaa' => 'MediumAquaMarine',
		'#66ff00' => '#66ff00',
		'#66ff33' => '#66ff33',
		'#66ff66' => '#66ff66',
		'#66ff99' => '#66ff99',
		'#66ffcc' => '#66ffcc',
		'#66ffff' => '#66ffff',
		'#696969' => 'DimGray',
		'#696969' => 'DimGrey',
		'#6a5acd' => 'SlateBlue',
		'#6b8e23' => 'OliveDrab',
		'#708090' => 'SlateGray',
		'#708090' => 'SlateGrey',
		'#778899' => 'LightSlateGray',
		'#778899' => 'LightSlateGrey',
		'#7b68ee' => 'MediumSlateBlue',
		'#7cfc00' => 'LawnGreen',
		'#7fff00' => 'Chartreuse',
		'#7fffd4' => 'Aquamarine',
		'#800000' => 'Maroon',
		'#800080' => 'Purple',
		'#806030' => '#806030',
		'#808000' => 'Olive',
		'#808080' => 'Gray',
		'#808080' => 'Grey',
		'#8470ff' => 'LightSlateBlue',
		'#87ceeb' => 'SkyBlue',
		'#87cefa' => 'LightSkyBlue',
		'#8a2be2' => 'BlueViolet',
		'#8b0000' => 'DarkRed',
		'#8b008b' => 'DarkMagenta',
		'#8b4513' => 'SaddleBrown',
		'#8fbc8f' => 'DarkSeaGreen',
		'#90ee90' => 'LightGreen',
		'#9370d8' => 'MediumPurple',
		'#9400d3' => 'DarkViolet',
		'#98fb98' => 'PaleGreen',
		'#990000' => '#990000',
		'#990033' => '#990033',
		'#990066' => '#990066',
		'#990099' => '#990099',
		'#9900cc' => '#9900cc',
		'#9900ff' => '#9900ff',
		'#9932cc' => 'DarkOrchid',
		'#993300' => '#993300',
		'#993333' => '#993333',
		'#993366' => '#993366',
		'#993399' => '#993399',
		'#9933cc' => '#9933cc',
		'#9933ff' => '#9933ff',
		'#996600' => '#996600',
		'#996633' => '#996633',
		'#996666' => '#996666',
		'#996699' => '#996699',
		'#9966cc' => '#9966cc',
		'#9966ff' => '#9966ff',
		'#999900' => '#999900',
		'#999933' => '#999933',
		'#999966' => '#999966',
		'#999999' => '#999999',
		'#9999cc' => '#9999cc',
		'#9999ff' => '#9999ff',
		'#99cc00' => '#99cc00',
		'#99cc33' => '#99cc33',
		'#99cc66' => '#99cc66',
		'#99cc99' => '#99cc99',
		'#99cccc' => '#99cccc',
		'#99ccff' => '#99ccff',
		'#99ff00' => '#99ff00',
		'#99ff33' => '#99ff33',
		'#99ff66' => '#99ff66',
		'#99ff99' => '#99ff99',
		'#99ffcc' => '#99ffcc',
		'#99ffff' => '#99ffff',
		'#9acd32' => 'YellowGreen',
		'#a0522d' => 'Sienna',
		'#a52a2a' => 'Brown',
		'#a9a9a9' => 'DarkGray',
		'#a9a9a9' => 'DarkGrey',
		'#add8e6' => 'LightBlue',
		'#adff2f' => 'GreenYellow',
		'#afeeee' => 'PaleTurquoise',
		'#b0a030' => '#b0a030',
		'#b0c070' => '#b0c070',
		'#b0c4de' => 'LightSteelBlue',
		'#b0e0e6' => 'PowderBlue',
		'#b22222' => 'FireBrick',
		'#b8860b' => 'DarkGoldenRod',
		'#ba55d3' => 'MediumOrchid',
		'#bc8f8f' => 'RosyBrown',
		'#bdb76b' => 'DarkKhaki',
		'#c0c0c0' => 'Silver',
		'#c71585' => 'MediumVioletRed',
		'#cc0000' => '#cc0000',
		'#cc0033' => '#cc0033',
		'#cc0066' => '#cc0066',
		'#cc0099' => '#cc0099',
		'#cc00cc' => '#cc00cc',
		'#cc00ff' => '#cc00ff',
		'#cc3300' => '#cc3300',
		'#cc3333' => '#cc3333',
		'#cc3366' => '#cc3366',
		'#cc3399' => '#cc3399',
		'#cc33cc' => '#cc33cc',
		'#cc33ff' => '#cc33ff',
		'#cc6600' => '#cc6600',
		'#cc6633' => '#cc6633',
		'#cc6666' => '#cc6666',
		'#cc6699' => '#cc6699',
		'#cc66cc' => '#cc66cc',
		'#cc66ff' => '#cc66ff',
		'#cc9900' => '#cc9900',
		'#cc9933' => '#cc9933',
		'#cc9966' => '#cc9966',
		'#cc9999' => '#cc9999',
		'#cc99cc' => '#cc99cc',
		'#cc99ff' => '#cc99ff',
		'#cccc00' => '#cccc00',
		'#cccc33' => '#cccc33',
		'#cccc66' => '#cccc66',
		'#cccc99' => '#cccc99',
		'#cccccc' => '#cccccc',
		'#ccccff' => '#ccccff',
		'#ccff00' => '#ccff00',
		'#ccff33' => '#ccff33',
		'#ccff66' => '#ccff66',
		'#ccff99' => '#ccff99',
		'#ccffcc' => '#ccffcc',
		'#ccffff' => '#ccffff',
		'#cd5c5c' => 'IndianRed',
		'#cd853f' => 'Peru',
		'#d02090' => 'VioletRed',
		'#d2691e' => 'Chocolate',
		'#d2b48c' => 'Tan',
		'#d3d3d3' => 'LightGray',
		'#d3d3d3' => 'LightGrey',
		'#d87093' => 'PaleVioletRed',
		'#d8bfd8' => 'Thistle',
		'#da70d6' => 'Orchid',
		'#daa520' => 'GoldenRod',
		'#dc143c' => 'Crimson',
		'#dcdcdc' => 'Gainsboro',
		'#dda0dd' => 'Plum',
		'#deb887' => 'BurlyWood',
		'#e0ffff' => 'LightCyan',
		'#e6e6fa' => 'Lavender',
		'#e9967a' => 'DarkSalmon',
		'#ee82ee' => 'Violet',
		'#eee8aa' => 'PaleGoldenRod',
		'#f08080' => 'LightCoral',
		'#f0e68c' => 'Khaki',
		'#f0f8ff' => 'AliceBlue',
		'#f0fff0' => 'HoneyDew',
		'#f0ffff' => 'Azure',
		'#f4a460' => 'SandyBrown',
		'#f5deb3' => 'Wheat',
		'#f5f5dc' => 'Beige',
		'#f5f5f5' => 'WhiteSmoke',
		'#f5fffa' => 'MintCream',
		'#f8f8ff' => 'GhostWhite',
		'#fa8072' => 'Salmon',
		'#faebd7' => 'AntiqueWhite',
		'#faf0e6' => 'Linen',
		'#fafad2' => 'LightGoldenRodYellow',
		'#fdf5e6' => 'OldLace',
		'#ff0000' => 'Red',
		'#ff0033' => '#ff0033',
		'#ff0066' => '#ff0066',
		'#ff0099' => '#ff0099',
		'#ff00cc' => '#ff00cc',
		'#ff00ff' => 'Magenta',
		'#ff1493' => 'DeepPink',
		'#ff3300' => '#ff3300',
		'#ff3333' => '#ff3333',
		'#ff3366' => '#ff3366',
		'#ff3399' => '#ff3399',
		'#ff33cc' => '#ff33cc',
		'#ff33ff' => '#ff33ff',
		'#ff4500' => 'OrangeRed',
		'#ff6347' => 'Tomato',
		'#ff6600' => '#ff6600',
		'#ff6633' => '#ff6633',
		'#ff6666' => '#ff6666',
		'#ff6699' => '#ff6699',
		'#ff66cc' => '#ff66cc',
		'#ff66ff' => '#ff66ff',
		'#ff69b4' => 'HotPink',
		'#ff7f50' => 'Coral',
		'#ff8c00' => 'Darkorange',
		'#ff9900' => '#ff9900',
		'#ff9933' => '#ff9933',
		'#ff9966' => '#ff9966',
		'#ff9999' => '#ff9999',
		'#ff99cc' => '#ff99cc',
		'#ff99ff' => '#ff99ff',
		'#ffa07a' => 'LightSalmon',
		'#ffa500' => 'Orange',
		'#ffb6c1' => 'LightPink',
		'#ffc0cb' => 'Pink',
		'#ffcc00' => '#ffcc00',
		'#ffcc33' => '#ffcc33',
		'#ffcc66' => '#ffcc66',
		'#ffcc99' => '#ffcc99',
		'#ffcccc' => '#ffcccc',
		'#ffccff' => '#ffccff',
		'#ffd700' => 'Gold',
		'#ffdab9' => 'PeachPuff',
		'#ffdead' => 'NavajoWhite',
		'#ffe4b5' => 'Moccasin',
		'#ffe4c4' => 'Bisque',
		'#ffe4e1' => 'MistyRose',
		'#ffebcd' => 'BlanchedAlmond',
		'#ffefd5' => 'PapayaWhip',
		'#fff0f5' => 'LavenderBlush',
		'#fff5ee' => 'SeaShell',
		'#fff8dc' => 'Cornsilk',
		'#fffacd' => 'LemonChiffon',
		'#fffaf0' => 'FloralWhite',
		'#fffafa' => 'Snow',
		'#ffff00' => 'Yellow',
		'#ffff33' => '#ffff33',
		'#ffff66' => '#ffff66',
		'#ffff99' => '#ffff99',
		'#ffffcc' => '#ffffcc',
		'#ffffe0' => 'LightYellow',
		'#fffff0' => 'Ivory',
		'#ffffff' => 'White',
	);

	%BVA::XUI::UTILS::COLORS_PLUS = (
		'#000033'            => '#000033',
		'#000066'            => '#000066',
		'#000099'            => '#000099',
		'#0000cc'            => '#0000cc',
		'#003300'            => '#003300',
		'#003333'            => '#003333',
		'#003366'            => '#003366',
		'#003399'            => '#003399',
		'#0033cc'            => '#0033cc',
		'#0033ff'            => '#0033ff',
		'#006600'            => '#006600',
		'#006633'            => '#006633',
		'#006666'            => '#006666',
		'#006699'            => '#006699',
		'#0066cc'            => '#0066cc',
		'#0066ff'            => '#0066ff',
		'#009900'            => '#009900',
		'#009933'            => '#009933',
		'#009966'            => '#009966',
		'#009999'            => '#009999',
		'#0099cc'            => '#0099cc',
		'#0099ff'            => '#0099ff',
		'#00cc00'            => '#00cc00',
		'#00cc33'            => '#00cc33',
		'#00cc66'            => '#00cc66',
		'#00cc99'            => '#00cc99',
		'#00cccc'            => '#00cccc',
		'#00ccff'            => '#00ccff',
		'#00ff33'            => '#00ff33',
		'#00ff66'            => '#00ff66',
		'#00ff99'            => '#00ff99',
		'#00ffcc'            => '#00ffcc',
		'#330000'            => '#330000',
		'#330033'            => '#330033',
		'#330066'            => '#330066',
		'#330099'            => '#330099',
		'#3300cc'            => '#3300cc',
		'#3300ff'            => '#3300ff',
		'#333300'            => '#333300',
		'#333333'            => '#333333',
		'#333366'            => '#333366',
		'#333399'            => '#333399',
		'#3333cc'            => '#3333cc',
		'#3333ff'            => '#3333ff',
		'#336600'            => '#336600',
		'#336633'            => '#336633',
		'#336666'            => '#336666',
		'#336699'            => '#336699',
		'#3366cc'            => '#3366cc',
		'#3366ff'            => '#3366ff',
		'#339900'            => '#339900',
		'#339933'            => '#339933',
		'#339966'            => '#339966',
		'#339999'            => '#339999',
		'#3399cc'            => '#3399cc',
		'#3399ff'            => '#3399ff',
		'#33cc00'            => '#33cc00',
		'#33cc33'            => '#33cc33',
		'#33cc66'            => '#33cc66',
		'#33cc99'            => '#33cc99',
		'#33cccc'            => '#33cccc',
		'#33ccff'            => '#33ccff',
		'#33ff00'            => '#33ff00',
		'#33ff33'            => '#33ff33',
		'#33ff66'            => '#33ff66',
		'#33ff99'            => '#33ff99',
		'#33ffcc'            => '#33ffcc',
		'#33ffff'            => '#33ffff',
		'#660000'            => '#660000',
		'#660033'            => '#660033',
		'#660066'            => '#660066',
		'#660099'            => '#660099',
		'#6600cc'            => '#6600cc',
		'#6600ff'            => '#6600ff',
		'#663300'            => '#663300',
		'#663333'            => '#663333',
		'#663366'            => '#663366',
		'#663399'            => '#663399',
		'#6633cc'            => '#6633cc',
		'#6633ff'            => '#6633ff',
		'#666600'            => '#666600',
		'#666633'            => '#666633',
		'#666666'            => '#666666',
		'#666699'            => '#666699',
		'#6666cc'            => '#6666cc',
		'#6666ff'            => '#6666ff',
		'#669900'            => '#669900',
		'#669933'            => '#669933',
		'#669966'            => '#669966',
		'#669999'            => '#669999',
		'#6699cc'            => '#6699cc',
		'#6699ff'            => '#6699ff',
		'#66cc00'            => '#66cc00',
		'#66cc33'            => '#66cc33',
		'#66cc66'            => '#66cc66',
		'#66cc99'            => '#66cc99',
		'#66cccc'            => '#66cccc',
		'#66ccff'            => '#66ccff',
		'#66ff00'            => '#66ff00',
		'#66ff33'            => '#66ff33',
		'#66ff66'            => '#66ff66',
		'#66ff99'            => '#66ff99',
		'#66ffcc'            => '#66ffcc',
		'#66ffff'            => '#66ffff',
		'#806030'            => '#806030',
		'#990000'            => '#990000',
		'#990033'            => '#990033',
		'#990066'            => '#990066',
		'#990099'            => '#990099',
		'#9900cc'            => '#9900cc',
		'#9900ff'            => '#9900ff',
		'#993300'            => '#993300',
		'#993333'            => '#993333',
		'#993366'            => '#993366',
		'#993399'            => '#993399',
		'#9933cc'            => '#9933cc',
		'#9933ff'            => '#9933ff',
		'#996600'            => '#996600',
		'#996633'            => '#996633',
		'#996666'            => '#996666',
		'#996699'            => '#996699',
		'#9966cc'            => '#9966cc',
		'#9966ff'            => '#9966ff',
		'#999900'            => '#999900',
		'#999933'            => '#999933',
		'#999966'            => '#999966',
		'#999999'            => '#999999',
		'#9999cc'            => '#9999cc',
		'#9999ff'            => '#9999ff',
		'#99cc00'            => '#99cc00',
		'#99cc33'            => '#99cc33',
		'#99cc66'            => '#99cc66',
		'#99cc99'            => '#99cc99',
		'#99cccc'            => '#99cccc',
		'#99ccff'            => '#99ccff',
		'#99ff00'            => '#99ff00',
		'#99ff33'            => '#99ff33',
		'#99ff66'            => '#99ff66',
		'#99ff99'            => '#99ff99',
		'#99ffcc'            => '#99ffcc',
		'#99ffff'            => '#99ffff',
		'#b0a030'            => '#b0a030',
		'#b0c070'            => '#b0c070',
		'#cc0000'            => '#cc0000',
		'#cc0033'            => '#cc0033',
		'#cc0066'            => '#cc0066',
		'#cc0099'            => '#cc0099',
		'#cc00cc'            => '#cc00cc',
		'#cc00ff'            => '#cc00ff',
		'#cc3300'            => '#cc3300',
		'#cc3333'            => '#cc3333',
		'#cc3366'            => '#cc3366',
		'#cc3399'            => '#cc3399',
		'#cc33cc'            => '#cc33cc',
		'#cc33ff'            => '#cc33ff',
		'#cc6600'            => '#cc6600',
		'#cc6633'            => '#cc6633',
		'#cc6666'            => '#cc6666',
		'#cc6699'            => '#cc6699',
		'#cc66cc'            => '#cc66cc',
		'#cc66ff'            => '#cc66ff',
		'#cc9900'            => '#cc9900',
		'#cc9933'            => '#cc9933',
		'#cc9966'            => '#cc9966',
		'#cc9999'            => '#cc9999',
		'#cc99cc'            => '#cc99cc',
		'#cc99ff'            => '#cc99ff',
		'#cccc00'            => '#cccc00',
		'#cccc33'            => '#cccc33',
		'#cccc66'            => '#cccc66',
		'#cccc99'            => '#cccc99',
		'#cccccc'            => '#cccccc',
		'#ccccff'            => '#ccccff',
		'#ccff00'            => '#ccff00',
		'#ccff33'            => '#ccff33',
		'#ccff66'            => '#ccff66',
		'#ccff99'            => '#ccff99',
		'#ccffcc'            => '#ccffcc',
		'#ccffff'            => '#ccffff',
		'#ff0033'            => '#ff0033',
		'#ff0066'            => '#ff0066',
		'#ff0099'            => '#ff0099',
		'#ff00cc'            => '#ff00cc',
		'#ff3300'            => '#ff3300',
		'#ff3333'            => '#ff3333',
		'#ff3366'            => '#ff3366',
		'#ff3399'            => '#ff3399',
		'#ff33cc'            => '#ff33cc',
		'#ff33ff'            => '#ff33ff',
		'#ff6600'            => '#ff6600',
		'#ff6633'            => '#ff6633',
		'#ff6666'            => '#ff6666',
		'#ff6699'            => '#ff6699',
		'#ff66cc'            => '#ff66cc',
		'#ff66ff'            => '#ff66ff',
		'#ff9900'            => '#ff9900',
		'#ff9933'            => '#ff9933',
		'#ff9966'            => '#ff9966',
		'#ff9999'            => '#ff9999',
		'#ff99cc'            => '#ff99cc',
		'#ff99ff'            => '#ff99ff',
		'#ffcc00'            => '#ffcc00',
		'#ffcc33'            => '#ffcc33',
		'#ffcc66'            => '#ffcc66',
		'#ffcc99'            => '#ffcc99',
		'#ffcccc'            => '#ffcccc',
		'#ffccff'            => '#ffccff',
		'#ffff33'            => '#ffff33',
		'#ffff66'            => '#ffff66',
		'#ffff99'            => '#ffff99',
		'#ffffcc'            => '#ffffcc',
		AliceBlue            => '#f0f8ff',
		AntiqueWhite         => '#faebd7',
		Aquamarine           => '#7fffd4',
		Azure                => '#f0ffff',
		Beige                => '#f5f5dc',
		Bisque               => '#ffe4c4',
		Black                => '#000000',
		BlanchedAlmond       => '#ffebcd',
		Blue                 => '#0000ff',
		BlueViolet           => '#8a2be2',
		Brown                => '#a52a2a',
		BurlyWood            => '#deb887',
		CadetBlue            => '#5f9ea0',
		Chartreuse           => '#7fff00',
		Chocolate            => '#d2691e',
		Coral                => '#ff7f50',
		CornflowerBlue       => '#6495ed',
		Cornsilk             => '#fff8dc',
		Crimson              => '#dc143c',
		Cyan                 => '#00ffff',
		DarkBlue             => '#00008b',
		DarkCyan             => '#008b8b',
		DarkGoldenRod        => '#b8860b',
		DarkGray             => '#a9a9a9',
		DarkGreen            => '#006400',
		DarkGrey             => '#a9a9a9',
		DarkKhaki            => '#bdb76b',
		DarkMagenta          => '#8b008b',
		DarkOliveGreen       => '#556b2f',
		Darkorange           => '#ff8c00',
		DarkOrchid           => '#9932cc',
		DarkRed              => '#8b0000',
		DarkSalmon           => '#e9967a',
		DarkSeaGreen         => '#8fbc8f',
		DarkSlateBlue        => '#483d8b',
		DarkSlateGray        => '#2f4f4f',
		DarkSlateGrey        => '#2f4f4f',
		DarkTurquoise        => '#00ced1',
		DarkViolet           => '#9400d3',
		DeepPink             => '#ff1493',
		DeepSkyBlue          => '#00bfff',
		DimGray              => '#696969',
		DimGrey              => '#696969',
		DodgerBlue           => '#1e90ff',
		FireBrick            => '#b22222',
		FloralWhite          => '#fffaf0',
		ForestGreen          => '#228b22',
		Fuchsia              => '#ff00ff',
		Gainsboro            => '#dcdcdc',
		GhostWhite           => '#f8f8ff',
		Gold                 => '#ffd700',
		GoldenRod            => '#daa520',
		Gray                 => '#808080',
		Green                => '#008000',
		GreenYellow          => '#adff2f',
		Grey                 => '#808080',
		HoneyDew             => '#f0fff0',
		HotPink              => '#ff69b4',
		IndianRed            => '#cd5c5c',
		Indigo               => '#4b0082',
		Ivory                => '#fffff0',
		Khaki                => '#f0e68c',
		Lavender             => '#e6e6fa',
		LavenderBlush        => '#fff0f5',
		LawnGreen            => '#7cfc00',
		LemonChiffon         => '#fffacd',
		LightBlue            => '#add8e6',
		LightCoral           => '#f08080',
		LightCyan            => '#e0ffff',
		LightGoldenRodYellow => '#fafad2',
		LightGray            => '#d3d3d3',
		LightGreen           => '#90ee90',
		LightGrey            => '#d3d3d3',
		LightPink            => '#ffb6c1',
		LightSalmon          => '#ffa07a',
		LightSeaGreen        => '#20b2aa',
		LightSkyBlue         => '#87cefa',
		LightSlateBlue       => '#8470ff',
		LightSlateGray       => '#778899',
		LightSlateGrey       => '#778899',
		LightSteelBlue       => '#b0c4de',
		LightYellow          => '#ffffe0',
		Lime                 => '#00ff00',
		LimeGreen            => '#32cd32',
		Linen                => '#faf0e6',
		Magenta              => '#ff00ff',
		Maroon               => '#800000',
		MediumAquaMarine     => '#66cdaa',
		MediumBlue           => '#0000cd',
		MediumOrchid         => '#ba55d3',
		MediumPurple         => '#9370d8',
		MediumSeaGreen       => '#3cb371',
		MediumSlateBlue      => '#7b68ee',
		MediumSpringGreen    => '#00fa9a',
		MediumTurquoise      => '#48d1cc',
		MediumVioletRed      => '#c71585',
		MidnightBlue         => '#191970',
		MintCream            => '#f5fffa',
		MistyRose            => '#ffe4e1',
		Moccasin             => '#ffe4b5',
		NavajoWhite          => '#ffdead',
		Navy                 => '#000080',
		OldLace              => '#fdf5e6',
		Olive                => '#808000',
		OliveDrab            => '#6b8e23',
		Orange               => '#ffa500',
		OrangeRed            => '#ff4500',
		Orchid               => '#da70d6',
		PaleGoldenRod        => '#eee8aa',
		PaleGreen            => '#98fb98',
		PaleTurquoise        => '#afeeee',
		PaleVioletRed        => '#d87093',
		PapayaWhip           => '#ffefd5',
		PeachPuff            => '#ffdab9',
		Peru                 => '#cd853f',
		Pink                 => '#ffc0cb',
		Plum                 => '#dda0dd',
		PowderBlue           => '#b0e0e6',
		Purple               => '#800080',
		Red                  => '#ff0000',
		RosyBrown            => '#bc8f8f',
		RoyalBlue            => '#4169e1',
		SaddleBrown          => '#8b4513',
		Salmon               => '#fa8072',
		SandyBrown           => '#f4a460',
		SeaGreen             => '#2e8b57',
		SeaShell             => '#fff5ee',
		Sienna               => '#a0522d',
		Silver               => '#c0c0c0',
		SkyBlue              => '#87ceeb',
		SlateBlue            => '#6a5acd',
		SlateGray            => '#708090',
		SlateGrey            => '#708090',
		Snow                 => '#fffafa',
		SpringGreen          => '#00ff7f',
		SteelBlue            => '#4682b4',
		Tan                  => '#d2b48c',
		Teal                 => '#008080',
		Thistle              => '#d8bfd8',
		Tomato               => '#ff6347',
		Turquoise            => '#40e0d0',
		Violet               => '#ee82ee',
		VioletRed            => '#d02090',
		Wheat                => '#f5deb3',
		White                => '#ffffff',
		WhiteSmoke           => '#f5f5f5',
		Yellow               => '#ffff00',
		YellowGreen          => '#9acd32',
	);

	%BVA::XUI::UTILS::MIMES = (
		'html'                                                        => 'html',
		'vnd.ms-excel'                                                => 'xls',
		'msword'                                                      => 'doc',
		'pdf'                                                         => 'pdf',
		'postscript'                                                  => 'ps',
		'rtf'                                                         => 'rtf',
		'zip'                                                         => 'zip',
		'gif'                                                         => 'gif',
		'jpeg'                                                        => 'jpg',
		'tiff'                                                        => 'tif',
		'tif'                                                         => 'tif',
		'png'                                                         => 'png',
		'plain'                                                       => 'txt',
		'mpeg'                                                        => 'mpg',
		'mov'                                                         => 'mov',
		'wav'                                                         => 'wav',
		'avi'                                                         => 'avi',
		'dvi'                                                         => 'dvi',
		'ppt'                                                         => 'ppt',
		'x-filemaker'                                                 => 'fp5',
		'octet-stream'                                                => 'bin',
		'vnd.openxmlformats-officedocument.wordprocessingml.document' => 'docx',
	);

}

sub mime_to_suffix {
	my $self = shift;
	my $mime = shift;
	$BVA::XUI::UTILS::MIMES{$mime} || $mime;
}

1;

