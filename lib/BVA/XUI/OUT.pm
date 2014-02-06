package BVA::XUI::OUT;

$BVA::XUI::OUT::VERSION	= '2.9'; # 2009-06-16 bva@cruzio.com

use strict;
use warnings;

use vars qw/*KEY *OUT/;

# UI Dispatch
sub OUT {
	my $obj				= shift;

    ## How is the value found in the template parsed?
    ## Here, it's subroutine[=arguments_as_string].
    ## Or, subroutine[:arguments_as_string]
    ## Or, subroutine[ is arguments_as_string]    # or ... IS ...
    my $tmpl			= shift();

	# Three scalars here; 3rd arg of split is 2 because sep is being captured
    my ($sub, $sep, $arg)	= split(/\s*(=| is |:)\s*/i => $tmpl => 2);

    ## Make sure $arg is defined, and trim trailing whitespace
    $arg			= defined $arg ? $arg : '';
    $arg			=~ s/^(.*?)\s*$/$1/;

    ## Give $obj & $arg back to @_ for sub called with goto
    unshift @_, $obj, $arg;
    
    ## Normalize sub names to lower case
    $sub			= lc($sub);
   
    ## 1. See if the sub has already been cached,
    ## first trying environment-specific subroutines, if any ** ADDED 2012-11-25.
	## 2. If not, see if the caller has a substitution for the sub,
    ## first trying environment-specific subroutines, if any.
    ## 3. Then try dispatching to this library's methods, 
    ## first trying environment-specific subroutines, if any.
    ## 4. Nothing? Give it back to the rendering engine.
    
	# 2012-11-25 new version
	# Changes:
	# 1. $OUT{_subname_} uses the _env subname if that sub is defined; previously,
	#    _subname_ was always the plain subname even if the _env sub as used.
	# 2. If no defined sub is found, $OUT{null_sub} rather than $OUT{_subname_} 
	#    is defined as sub { return }.

	my $sub_env		= $sub . '_' . ${ *$obj }{_env};
	my $out_sub		= $OUT{$sub_env} || $OUT{$sub} || do {
		defined &{ 'main::' . $sub_env } ?
			$OUT{$sub_env}	= \&{ 'main::' . $sub_env } :
		defined &{ 'main::' . $sub } ?
			$OUT{$sub}	= \&{ 'main::' . $sub } :
		defined &{ $sub_env } ?
			$OUT{$sub_env}	= \&{ $sub_env } :
		defined &{ $sub } ?
			$OUT{$sub}	= \&{ $sub } :
		$OUT{null_sub} ||= sub { return }
	};
	
	goto &{ $out_sub };
}

# Standard Display Subroutines
sub data {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$KEY{$arg} || ''
}

sub output {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $out				= $KEY{"${arg}_out"} || $KEY{"${arg}.out"} || $KEY{_vals}->{$arg} || $KEY{$arg} || '';
	if ($KEY{_env} =~ /htm/i) {
		$out			=~ s{\\}{<br>\n}gs;
	} else {
		$out			=~ s{\\}{\n}gs;
	}
	
	# Protect "false" values
 	$KEY{__DONE__}++ unless $out;

	$out;
}

sub output_htm {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $out				= $KEY{"${arg}_out"} || $KEY{"${arg}.out"} || $KEY{_vals}->{$arg} || $KEY{$arg} || '';
	$out				=~ s{\\}{<br>\n}gs;
	
	# Protect "false" values
 	$KEY{__DONE__}++ unless $out;

	return $out;
}

sub db_value {
	my $obj		= shift;
	local *KEY	= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $fld		= shift;
	
	my $out		= ($KEY{$fld} or (!$KEY{$fld} && ~$KEY{$fld}==1))	? $KEY{$fld}
					: exists $KEY{_meta}{$fld}{null}				? $KEY{_meta}{$fld}{null}
					: '';
	
	$KEY{__DONE__}++ unless $out;
	
	$out		=~ s{\\}{\n}gs;

	$out;
}

sub float {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my ($fld,$right)	= split /\s*,\s*/ => $arg;
	my $amt				= $KEY{"${fld}_out"} 
							|| $KEY{"${fld}.out"} 
								|| $KEY{_vals}->{$fld} 
									|| $KEY{$fld} 
										|| $KEY{_meta}{$fld}{default} 
											|| 0;
	$right				= (!$right && ~$right) ? 0 : $right || 2;

 	$amt =~ /^[0-9.+-]+$/ ? sprintf(qq{%.${right}f}, $amt) : $amt;
}

sub labeled {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my ($fld,$label,$hide_null)	= split /\s*,\s*/ => $arg;				
	return '' unless $fld;
	$hide_null			||= 0;
	my $val;
	$val				= exists $KEY{_vals}->{$fld} ? $KEY{_vals}->{$fld} :
							exists $KEY{$fld} ? $KEY{$fld} : $val;	
	if (!$val && ~$val) {
		$val	= '0'
	} elsif (!$val) {
		$val	= exists $KEY{_meta}{$fld}{default} ? $KEY{_meta}{$fld}{default} :
					exists $KEY{_meta}{$fld}{null} ? $KEY{_meta}{$fld}{null} :
						'';
		return ' ' if (!$val && $hide_null);
	}
	
	if ($label) {
		sprintf qq{%15.30s %s}, 
			$label, 
			$val	
	} elsif (exists $KEY{_meta}) {
		if ($KEY{_meta}{$fld}{type} =~ /a/i) {
			if ($KEY{_env} =~ /htm/i) {
				$val			=~ s{\\}{<br>\n}gs;
				sprintf qq{<dl><dt>%s:</dt><dd class="box_as_item">\n%s\n</dd></dl>}, 
					$KEY{_meta}{$fld}{label}, $val

			} else {
				$val			=~ s{\\}{\n}gs;
				sprintf qq{%s:\n%s\n}, 
					$KEY{_meta}{$fld}{label}, $val
			}
		} elsif ($KEY{_meta}{$fld}{type} =~ /b/i) {
			my $show_val		= $val ? 'Yes' : 'No';
			if ($KEY{_env} =~ /htm/i) {
				sprintf qq{%15.30s: <b>%s</b>}, 
					$KEY{_meta}{$fld}{label} || join( ' ' => map { ucfirst($_) } (split /_|\./ => $fld) ),
					$show_val
			} else {
				sprintf qq{%15.30s: %s}, 
					$KEY{_meta}{$fld}{label} || join( ' ' => map { ucfirst($_) } (split /_|\./ => $fld) ),
					$show_val
			}
		} elsif ($KEY{_meta}{$fld}{type} =~ /ct/i) {
			my ($show_val,$h,$m,$s)	= ('','','','');
			if ($val and $val =~ /^(\d\d):(\d\d)(:(\d\d))?$/) {
				$h	= $1;
				$m	= $2;
				$s	= $4;
				if ($h + $m + $s > 0) {
					$show_val	=  $obj->tell_time('time','0000-00-00T' . $val);
				} else {
					$show_val	=  '';
				}
			}			
			if ($KEY{_env} =~ /htm/i) {
				sprintf qq{%15.30s: <b>%s</b>}, 
					$KEY{_meta}{$fld}{label} || join( ' ' => map { ucfirst($_) } (split /_|\./ => $fld) ),
					$show_val;
			} else {
				sprintf qq{%s: %s}, 
					$KEY{_meta}{$fld}{label} || join( ' ' => map { ucfirst($_) } (split /_|\./ => $fld) ),
					$show_val;
			}
		} else {
			if ($KEY{_env} =~ /htm/i) {
				sprintf qq{%15.30s: <b>%s</b>}, 
					$KEY{_meta}{$fld}{label} || join( ' ' => map { ucfirst($_) } (split /_|\./ => $fld) ),
					$val
			} else {
				sprintf qq{%s: %s}, 
					$KEY{_meta}{$fld}{label} || join( ' ' => map { ucfirst($_) } (split /_|\./ => $fld) ),
					$val			
			}
		}
	} else {
		my $label	= join ' ' => map { ucfirst($_) } (split /_|\./ => $fld);
		if ($KEY{_env} =~ /htm/i) {
			sprintf qq{%15.30s: <b>%s</b>}, 
				$label, 
				$val
		} else {
			sprintf qq{%s: %s}, 
				$label, 
				$val		
		}
	}
}






sub uri_ok {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
    my $arg             = shift;
	my $out				= $KEY{"${arg}_out"} || $KEY{"${arg}.out"} || $KEY{_vals}->{$arg} || $KEY{$arg} || '';

	$obj->url_encode($out) || ''
}

sub value {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
    my $arg             = shift;
    my ($fld, $alt, $null_type) = split(/\s*,\s*/ => $arg );     
    defined($alt) or $alt = '';   
    defined($null_type) or $null_type = '';
    NULLTYPE: {
      local $_ = $null_type;
      /^[1-9][0-9]*$/     and $null_type = ' ' x $null_type, last NULLTYPE;

      /^eol$/     and $null_type = "\n"
        or
      /^tab$/     and $null_type = "t"
        or
      /^q$/       and $null_type = "\"\""
        or
      /^qs$/      and $null_type = "''"
        or
      /^zbt$/     and $null_type = "0 but true"
        or
      /^zz$/      and $null_type = '00'
        or
      /^z$/       and $null_type =  'zero'
        or
      /^zero$/    and $null_type =  '0'
        or
      /^n$/       and $null_type =  ''
    } 
   
     my $out		= $KEY{_vals}->{$fld} || $KEY{$fld} || $KEY{_vals}->{$alt} || $KEY{$alt} || (exists $KEY{$alt} ? "" : $alt) || $null_type;
#    my $out		= $KEY{_vals}->{$fld} || $KEY{$fld} || $KEY{_vals}->{$alt} || (exists $KEY{$alt} ? $KEY{$alt} : $alt) || $null_type;

    if ($KEY{_env} =~ /htm/i) {
		$out			=~ s{\\}{<br>\n}gs;
	} else {
		$out			=~ s{\\}{\n}gs;
	}
	
	# Protect "false" values
	$KEY{__DONE__}++ unless $out;

	$out

}

sub time {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my ($format, $time) = split(/\s*,\s*/ => $KEY{$arg} || $arg );
	$time				||= '*';
	($time =~ /\S+/ and $time !~ /^[0: \/-]+$/) ? $obj->tell_time($format, $time) : ' ';
}

sub when {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my ($format, $time) = split(/\s*,\s*/ => $KEY{$arg} || $arg );
	$time				||= 'now';
	$obj->tell_time($format, $time) || ' ';
}

sub date {
	my $obj			= shift;
	local *KEY		= ref($obj) ? $obj : *{ $obj };
	my $arg			= shift;
	my $mark		= $KEY{_mark};
	my ($name, $date, $label, $btn_style, $script)		= split(/\s*,\s*/ => $arg);	
	return '' unless $name;
  	my $this_year	= $obj->this_yyyy;
	
	my $new_date	= $KEY{_vals}{$name} || $KEY{$name} || '';
	$date			= ($date eq '0000-00-00' and $new_date) ?
						$new_date :
							$date eq '-' ? $this_year . '-00-00' : 
								$date || $new_date || $obj->tell_time('iso');
												
	$label			||= $KEY{_meta}{$name}{label} 
						|| join( ' ' => map { ucfirst($_) } split / |_/ => $name);
	
	my $label_style	= $obj->is_required($name) ? 'label_req' : 'label_std';		
	
	$btn_style						||= qq{background-color: #EED;}; # font-size: 8pt; 
	
	my ($year, $month, $day, $time)	= split/-| |T/ => $date || $KEY{$name} || '';

	my ($first_year, $last_year);
	
	$year			= $year && $year + 0 ? $year : 'Year';
	$month			= $month && $month + 0 ? ($obj->list_months)[$month-1] : 'Month';
	$day			= $day && $day + 0 ? $day + 0 : 'Day';
	
  	if ($year ne 'Year') {
  		$first_year		= $year > $this_year ? $this_year : $year;
  		$last_year		= $year > $this_year ? $year+3 : $this_year+3;
  	} else {
  		$first_year		= $this_year;
  		$last_year		= $this_year+3;
  	} 

	sprintf qq{<span class="%s">%20.20s%s</span> %s  %s  %s},
		$label_style,
		($label eq '*'? '' : $label), 
		($label eq '*'? '' : ':'), 
# 		pop_up3("${mark}_${name}_mon", [ $obj->list_months ],'', $month, $month, '', $btn_style ), 
# 		pop_up3("${mark}_${name}_day", [ $obj->list_monthday_nums ],'', $day, $day, '' , $btn_style ),
# 		pop_up3("${mark}_${name}_yr",  [ (($first_year-1) .. $last_year) ],'', $year, $year, '', $btn_style )
		pop_up3("${mark}_${name}_mon", [ 'Month', $obj->list_months ],'', $month, $month, $script, $btn_style ), 
		pop_up3("${mark}_${name}_day", [ 'Day', $obj->list_monthday_nums ],'', $day, $day, $script, $btn_style ),
		pop_up3("${mark}_${name}_yr",  [ 'Year', (($first_year-1) .. $last_year) ],'', $year, $year, $script, $btn_style )
}

sub clocktime {
	my $obj							= shift;
	local *KEY						= ref($obj) ? $obj : *{ $obj };
	my $arg							= shift;
	my $mark						= $KEY{_mark};
	my ($name, $time, $label, $show_secs, $use_now_time, $plain)		= split(/\s*,\s*/ => $arg);	
	return '' unless $name;
	$show_secs						||= '';
	$use_now_time					||= '0';
	
	my $new_time					= $KEY{_vals}{$name} || $KEY{$name} || '';
	$time							= ($time eq '00:00:00' and $new_time) ?
										$new_time :
											$time eq '-' ? '00:00:00' : 
												$time || $new_time || ($use_now_time ? $obj->tell_time('iso_time') : '');
												
	$label							||= $KEY{_meta}{$name}{label} || join( ' ' => map { ucfirst($_) } split / |_/ => $name);
	
	$plain							||= '';
	
	$label							= $plain ? '' : $label;
	
	my $label_style	= $obj->is_required($name) ? 'label_req' : 'label_std';		
	
	my ($hour, $minute, $second)	= split/:/ => $time || $KEY{$name} || '';

	$hour							= defined $hour ? $hour : '';
	$minute							= defined $minute ? $minute : '';
	$second							= defined $second ? $second : '';
	my $ap;
	if ($hour + $minute + $second == 0) {
		$ap 	= '';
	} elsif ($hour > 11) {
		$ap		= 'PM';
	} else {
		$ap 	= 'AM';
	}
	if ($hour > 12) {
		$hour	= $hour - 12;
	}
	
	
	sprintf qq{<span class="%s">%20.20s%s</span> %s:%s%s %s (%s)},
		$label_style,
		($label eq '*'? '' : $label), 
		($label eq '*'? '' : $plain ? '' : ':'), 
		qq{<input type="text" name="${mark}_${name}_hr" value="$hour" size="2" maxlength="2">\n},
		qq{<input type="text" name="${mark}_${name}_min" value="$minute" size="2" maxlength="2">\n},
		($show_secs ? qq{:<input type="text" name="${mark}_${name}_sec" value="$second" size="2" maxlength="2">\n} : ""),
		qq{<input type="text" name="${mark}_${name}_ap" value="$ap" size="3" maxlength="2">\n},
		($show_secs ? qq{HH:MM:SS AM/PM} : qq{HH:MM AM/PM});
}

sub file {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$arg				= $KEY{$arg} || $arg;
	local $/;
	open (FH, "<", $arg)
		and <FH>
			or qq{$KEY{_start}$KEY{_mark} ERROR=Couldn't include file $arg: $!. $KEY{_end}};
}

sub display {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($arg,$alt)		= split /\s*,\s*/ => shift(), 2;

	$arg				||= '';
	my $display			= '';
	
	# $arg may be '', to allow displays to be named _$KEY{_env} -- e.g., _htm
	$display			= $KEY{_displays}->{"${arg}_$KEY{_env}"}
							|| $KEY{_displays}->{"${arg}.$KEY{_env}"}
								|| $KEY{_displays}->{$arg}
									|| $obj->template("-${arg}_$KEY{_env}")
										|| $obj->template("-${arg}.$KEY{_env}")
											|| $obj->template("-$arg");
	# $alt may NOT be ''
	if (!$display && $alt) {
		$display			= $KEY{_displays}->{"${alt}_$KEY{_env}"}
								|| $KEY{_displays}->{"${alt}.$KEY{_env}"}
									|| $KEY{_displays}->{$alt}
										|| $obj->template("-${alt}_$KEY{_env}")
											|| $obj->template("-${alt}.$KEY{_env}")
												|| $obj->template("-$alt");
	}
							
	$KEY{__DONE__}++ unless $display;
	
	$display	
}

sub send_display {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($arg,$alt)		= split /\s*,\s*/ => shift(), 2;

	$arg				||= '';
	my $display			= '';
	
	# $arg may be '', to allow displays to be named _$KEY{_env} -- e.g., _htm
	$display			= $KEY{_displays}->{"${arg}_$KEY{_env}"}
							|| $KEY{_displays}->{"${arg}.$KEY{_env}"}
								|| $KEY{_displays}->{$arg}
									|| $obj->template("-${arg}_$KEY{_env}")
										|| $obj->template("-${arg}.$KEY{_env}")
											|| $obj->template("-$arg");
	# $alt may NOT be ''
	if (!$display && $alt) {
		$display			= $KEY{_displays}->{"${alt}_$KEY{_env}"}
								|| $KEY{_displays}->{"${alt}.$KEY{_env}"}
									|| $KEY{_displays}->{$alt}
										|| $obj->template("-${alt}_$KEY{_env}")
											|| $obj->template("-${alt}.$KEY{_env}")
												|| $obj->template("-$alt");
	}
							
	$display ||= $KEY;
	
	$display =~ s/[ \t\n]+/ /g;
	
	return $display;
}

sub display_if {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($if,$arg,$alt)	= split /\s*,\s*/ => shift(), 3;
	$if					||= '';
	$arg				||= '';

	my $condition		= $KEY{_vals}->{$if} || $KEY{$if} || '';
	# If not true so far, test for a field named for the display
	$condition			||= $KEY{_vals}->{$arg} || $KEY{$arg} || '';
	
	my $display			= '';
	
	if ($condition) {
	
		# $arg may be '', to allow displays to be named _$KEY{_env} -- e.g., _htm
		$display			= $KEY{_displays}->{"${arg}_$KEY{_env}"}
								|| $KEY{_displays}->{"${arg}.$KEY{_env}"}
									|| $KEY{_displays}->{$arg}
										|| $obj->template("-${arg}_$KEY{_env}")
											|| $obj->template("-${arg}.$KEY{_env}")
												|| $obj->template("-$arg");
		# $alt may NOT be ''
		if (!$display && $alt) {
			$display			= $KEY{_displays}->{"${alt}_$KEY{_env}"}
									|| $KEY{_displays}->{"${alt}.$KEY{_env}"}
										|| $KEY{_displays}->{$alt}
											|| $obj->template("-${alt}_$KEY{_env}")
												|| $obj->template("-${alt}.$KEY{_env}")
													|| $obj->template("-$alt");
		}
		
	}
		
	$KEY{__DONE__}++ unless $display;
	
	$display	
}

sub display_if_else {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($if,$arg,$alt)	= split /\s*,\s*/ => shift(), 3;
	$if					||= '';
	$arg				||= '';
	$alt				||= '';

	my $condition		= $KEY{_vals}->{$if} || $KEY{$if} || '';
	# If not true so far, test for a field named for the display
	$condition			||= $KEY{_vals}->{$arg} || $KEY{$arg} || '';
	
	my $display			= '';
	
	if ($condition) {
	
		# $arg may be '', to allow displays to be named _$KEY{_env} -- e.g., _htm
		$display			= $KEY{_displays}->{"${arg}_$KEY{_env}"}
								|| $KEY{_displays}->{"${arg}.$KEY{_env}"}
									|| $KEY{_displays}->{$arg}
										|| $obj->template("-${arg}_$KEY{_env}")
											|| $obj->template("-${arg}.$KEY{_env}")
												|| $obj->template("-$arg");
	} else {
		$display			= $KEY{_displays}->{"${alt}_$KEY{_env}"}
								|| $KEY{_displays}->{"${alt}.$KEY{_env}"}
									|| $KEY{_displays}->{$alt}
										|| $obj->template("-${alt}_$KEY{_env}")
											|| $obj->template("-${alt}.$KEY{_env}")
												|| $obj->template("-$alt");		
	}
		
	$KEY{__DONE__}++ unless $display;
	
	$display	
}

sub display_if_not {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($if,$arg,$alt)	= split /\s*,\s*/ => shift(), 3;
	$if					||= '';
	$arg				||= '';

	my $condition		= $KEY{_vals}->{$if} || $KEY{$if} || '';
	# If not true so far, test for a field named for the display
	$condition			||= $KEY{_vals}->{$arg} || $KEY{$arg} || '';
	
	my $display			= '';
	
	if (!$condition) {
	
		# $arg may be '', to allow displays to be named _$KEY{_env} -- e.g., _htm
		$display			= $KEY{_displays}->{"${arg}_$KEY{_env}"}
								|| $KEY{_displays}->{"${arg}.$KEY{_env}"}
									|| $KEY{_displays}->{$arg}
										|| $obj->template("-${arg}_$KEY{_env}")
											|| $obj->template("-${arg}.$KEY{_env}")
												|| $obj->template("-$arg");
		# $alt may NOT be ''
		if (!$display && $alt) {
			$display			= $KEY{_displays}->{"${alt}_$KEY{_env}"}
									|| $KEY{_displays}->{"${alt}.$KEY{_env}"}
										|| $KEY{_displays}->{$alt}
											|| $obj->template("-${alt}_$KEY{_env}")
												|| $obj->template("-${alt}.$KEY{_env}")
													|| $obj->template("-$alt");
		}
		
	}
		
	$KEY{__DONE__}++ unless $display;
	
	$display	
}

sub if {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($if,$arg)		= split /\s*,\s*/ => shift(), 2;
	$if					||= '';
	$arg				||= '';

	my $out				= ($KEY{_vals}->{$if} || $KEY{$if}) ? $arg : '';

	$KEY{__DONE__}++ unless $out;
	
	$out
}

sub if_else {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($if,$arg,$alt)		= split /\s*,\s*/ => shift(), 3;
	$if					||= '';
	$arg				= (defined $arg) ? $arg : '';
	$alt				= (defined $alt) ? $alt : '';

	my $out				= ($KEY{_vals}->{$if} || $KEY{$if}) ? $arg : $alt;
	
	$KEY{__DONE__}++ unless $out;
	
	$out
}

sub if_else_pipe {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my ($if,$arg,$alt)		= split /\s*\|\s*/ => shift(), 3;
	$if					||= '';
	$arg				= (defined $arg) ? $arg : '';
	$alt				= (defined $alt) ? $alt : '';

	my $out				= $KEY{_vals}->{$if} ? $arg : $KEY{$if} ? $arg : $alt;

	$KEY{__DONE__}++ unless $out;

	$out
}

sub if_value_else {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($if,$value,$arg,$alt)		= split /\s*,\s*/ => shift(), 4;
	$if					||= '';
	$arg				= (defined $arg) ? $arg : '';
	$alt				= (defined $alt) ? $alt : '';

	my $out				= ($KEY{_vals}->{$if} && $KEY{_vals}->{$if} eq $value) ? $arg :
							($KEY{$if} && $KEY{$if} eq $value) ? $arg : $alt;
	
	$KEY{__DONE__}++ unless $out;
	
	$out
}

sub unless {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
 	my ($if,$arg)		= split /\s*,\s*/ => shift(), 2;
	$if					||= '';
	$arg				||= '';

	my $out				= ($KEY{_vals}->{$if} || $KEY{$if}) ? '' : $arg;

	$KEY{__DONE__}++ unless $out;
	
	$out
}

sub trunc {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	return '' unless $arg;
	my ($fld, $len)		= split(/\s*,\s*/ => $arg);
	return '' unless $fld;
	substr( ($KEY{_vals}->{$fld} || $KEY{"${fld}_out"} || $KEY{"${fld}.out"} || $KEY{$fld} || ''), 0, ($len || 0) ) || ' ';
}


sub one_word {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $out				= $KEY{$arg} || $arg;
	$out				=~ s/[ ',]/_/g;
	$out				=~ s/_+/_/g;
	$out
}


sub error {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg					= shift;
	{
	local $_ = $KEY{_err_method}
	  or 			return '';
	  
	/b(lank)?/i		and return ' '
	  or
	/p(rint)?/i		and return $arg
	  or
	/r(aise)?/i		and $obj->oops($arg,'Error')
	  or
	(/l(og)?/i and $KEY{_file} ne '-')	
					and $obj->store( 'Error: ', $arg, "\n") 
					and return " "
	  or
	(/t(race)?/i and $KEY{_file} ne '-')
					and $obj->store('Error: ', $arg, "\n") 
					and return "$KEY{_start} Error logged in $KEY{_file}. $KEY{_end}"
	  or
					return ''	
	}
}

sub msg {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my ($arg,@suf)		= split /\s*,\s*/ => shift;
	$arg				||= '';
	$arg = "${arg}_msg" if exists $KEY{"${arg}_msg"};
	$arg = "${arg}_MSG" if exists $KEY{"${arg}_MSG"};
	
	my $msg = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->() :  
		$obj->message($arg) 
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
	$msg				= $msg ? join( '' => $msg, @suf ) : '';
	
	if ($KEY{_env} =~ /htm/i) {
		$msg			=~ s{\\}{<br>\n}gs;
	} else {
		$msg			=~ s{\\}{\n}gs;
	}
	
	$msg
}

sub msg_pat {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my ($arg,$pat)		= split /\s*,\s*/ => shift;
	$arg				||= '';
	$arg = "${arg}_msg" if exists $KEY{"${arg}_msg"};
	$arg = "${arg}_MSG" if exists $KEY{"${arg}_MSG"};
	
	my $msg = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->() :  
		$obj->message($arg) 
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
	$msg ? sprintf( $pat => $msg ) : ''
}


sub err_pat {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my ($arg,$pat)		= split /\s*,\s*/ => shift;
	$arg = "${arg}_err" if exists $KEY{"${arg}_err"};
	$arg = "${arg}_ERR" if exists $KEY{"${arg}_ERR"};
	
	my $err = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->() :  
		$obj->message($arg) 
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
	$err ? sprintf( $pat => $err ) : ''
}

sub msg_line {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$arg = "${arg}_msg" if exists $KEY{"${arg}_msg"};
	$arg = "${arg}_MSG" if exists $KEY{"${arg}_MSG"};
	
	my $msg = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->():  
		$obj->message( split(/\s*,\s*/ => $arg))
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
	$msg ? qq{\n<div class="msg_line">\n$msg\n</div>\n<br>\n} : ' '
}

sub msg_rule {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$arg = "${arg}_msg" if exists $KEY{"${arg}_msg"};
	$arg = "${arg}_MSG" if exists $KEY{"${arg}_MSG"};
	
	my $msg = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->():  
		$obj->message( split(/\s*,\s*/ => $arg))
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
	$msg ? qq{\n<div class="msg_line">\n$msg\n</div>\n<hr>\n} : ' '
}


sub err_line {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$arg = "${arg}_err" if exists $KEY{"${arg}_err"};
	$arg = "${arg}_ERR" if exists $KEY{"${arg}_ERR"};
	
	my $err = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->():  
		$obj->message( split(/\s*,\s*/ => $arg))
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
	if ($KEY{_env} =~ /htm/i) {
		$err			=~ s{\\}{<br>\n}gs;
	} else {
		$err			=~ s{\\}{\n}gs;
	}

	$err				= $err ? qq{\n<span class="err_line">\n$err\n</span>\n<br>\n} : ' ';
	
	$err;
}

sub err_rule {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$arg = "${arg}_err" if exists $KEY{"${arg}_err"};
	$arg = "${arg}_ERR" if exists $KEY{"${arg}_ERR"};
	
	my $err = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->():  
		$obj->message( split(/\s*,\s*/ => $arg))
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
					
	$err				= $err ? qq{\n<span class="err_line">\n$err\n</span>\n<hr>\n} : ' ';
	
	if ($KEY{_env} =~ /htm/i) {
		$err			=~ s{\\}{<br>\n}gs;
	} else {
		$err			=~ s{\\}{\n}gs;
	}
	
	$err
}

sub msg_box {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$arg = "${arg}_msg" if exists $KEY{"${arg}_msg"};
	$arg = "${arg}_MSG" if exists $KEY{"${arg}_MSG"};
	
	my $msg = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->():  
		$obj->message( split(/\s*,\s*/ => $arg))
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
	$msg ? qq{<pre class="msg_box">$msg</pre>} : ' '
}

sub err {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my ($arg,@suf)		= split /\s*,\s*/ => shift;
	
	$arg	||= '';
	$arg = "${arg}_err" if exists $KEY{"${arg}_err"};
	$arg = "${arg}_ERR" if exists $KEY{"${arg}_ERR"};
	
	my $err = (ref($KEY{$arg}) =~ /CODE/) ? $KEY{$arg}->():  
		$obj->message( split(/\s*,\s*/ => $arg))
			|| $KEY{_messages}->{$arg}
				|| $KEY{$arg}
					|| '';
					
	$err				= $err ? join( '' => $err, @suf ) : '';
	
	if ($KEY{_env} =~ /htm/i) {
		$err			=~ s{\\}{<br>\n}gs;
	} else {
		$err			=~ s{\\}{\n}gs;
	}
	
	$err

}

sub hdr {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$obj->out_header( split(/\s*,\s*/ => $KEY{$arg} || $arg || '' ) ) || "\n"
}

sub recall {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;

	qq{\n<pre>\n} . join( "\n" => map {
		my $r = $_;
		$r = ($r =~ /(=\s*\(|\);)/ ? "" : $r);
		$r =~ s/^\s*/ /;
		$r;
	} $obj->recall() ) . qq{\n</pre>\n}
}



# HTML Display Subroutines

sub style_htm {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	
	my $style			= qq{<style>\n};
	$style				.= $KEY{style_sheet_url} ?
				qq{\t\t\@import url("$KEY{style_sheet_url}");\n} :
						   $KEY{_style_sheet_url} ?
				qq{\t\t\@import url("$KEY{_style_sheet_url}");\n} : '';
					
	$style				.= $arg =~ /\S/ ? "\t\t$arg" : qq{};
	$style				.= qq{\n\t</style>\n};
}

sub stylesheet_htm {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	$arg =~ /\S/ ? 
		qq{<link rel="stylesheet" href="$arg">} :
			$KEY{style_sheet_url} ?
				qq{<link rel="stylesheet" href="$KEY{style_sheet_url}">} :
			$KEY{_style_sheet_url} ?
				qq{<link rel="stylesheet" href="$KEY{_style_sheet_url}">} :
					qq{<!-- No style sheet found -->}
}

sub bgcolor {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my ($color,$default)= split /\s*,\s*/ => $arg;
	my $bgc				= $KEY{"${color}_out"} || $KEY{"${color}.out"} || $KEY{$color} || $color || '';
	$default 			= $default ? $default =~ /^\s*(\#[a-fA-F0-9]{6})\s*$/i ? $1 
							: '#a9a9a9'
							: '#FFFFFF';
	
	$bgc =~ /^\s*(#[A-F0-9]{6})\s*$/i ? $1 : 
		$bgc =~ /^\s*(aqua|black|blue|fuchsia|gray|green|lime|maroon|navy|olive|purple|red|silver|teal|yellow|white)\s*$/i ? $1 :
			$bgc =~ /^\s*(lightblue|cornsilk|beige|pink|deeppink|deepskyblue|lightseagreen|peachpuff|powderblue|wheat)\s*$/i ? $1 :
				$default;
}


sub action {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my ($field,$script)	= split /\s*,\s*/ => $arg, 2;
	$script				= $script || "return true";
	sprintf qq/<input type=submit name='action' value="%s" onclick="form.onsubmit=function() { %s };" >/, $KEY{$field} || $field, $script; 
}

sub actions {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $buttons			= qq{<p class="btn_line">\n};
	my $btn_count		= 0;
	$arg				= $KEY{"${arg}_acts"} || $KEY{"${arg}.acts"} || $KEY{$arg} || $arg;
	my @acts			= ref($arg) ? @{ $arg } : split(/\s*,\s*/ => $arg);
	for (@acts) {
		$buttons .= sprintf qq{%s<input type=submit name='action' value="%s">%s\n},
					($btn_count + 1) % 4 ?  "" : qq{</p>\n<p class="btn_line">\n},
					$_ ,
					++$btn_count % 3 ? " &nbsp; " : ""
	}
	$buttons			.= '</p>';
}

sub input {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	sprintf qq{<input type=text name='%s' value='%s' size='%s' maxlength='%s'>}, 
		split(/\s*,\s*/ => $arg)
}

sub Xhidden {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	sprintf qq{<input type=hidden name='%s' value="%s">}, 
		split(/\s*,\s*/ => $KEY{$arg} || $arg )
}

sub hidden {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $mark			= $KEY{_mark};
	my ($fld,$value)	= split(/\s*,\s*/ => $arg );
	return "" unless $fld;
	$value ||= $KEY{_vals}->{$fld} || $KEY{$fld} || '';
	sprintf qq{<input type=hidden name='%s' value="%s">}, $fld, $value;
}


sub hidden_marked {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $mark			= $KEY{_mark};
	my ($fld,$value)	= split(/\s*,\s*/ => $arg );
	return "" unless $fld;
	$value ||= $KEY{"${arg}_out"} || $KEY{"${arg}.out"} ||$KEY{_vals}->{$fld} || $KEY{$fld} || '';
	sprintf qq{<input type=hidden name='%s:%s' id='%s:%s' value="%s">}, $mark, $fld, $mark, $fld, $value;
}

sub checkbox {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $mark			= $KEY{_mark};
	sprintf qq{<input type=checkbox name='%s' value="%s">}, split(/\s*,\s*/ => $KEY{$arg} || $arg)
}

sub radio {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $mark			= $KEY{_mark};
	sprintf qq{<input type=radio name='%s' value="%s">}, split(/\s*,\s*/ => $KEY{$arg} || $arg)
}

sub img {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my ($file, $title)	= split(/\s*,\s*/ => $KEY{$arg} || $arg );
	sprintf qq{<img src="%s" alt="%s">}, $file, $title || ''
}

sub collect {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $mark			= $KEY{_mark};
	# COLLECT=all  COLLECT=first_name, last_name, type1, type 2
	my @fields;
	my $RE				= $KEY{_type_RE};
	for my $n (split(/\s*,\s*/ => $KEY{$arg} || $arg ) ) {
		$n =~ /^\s*all\s*$/i
			and push @fields => @{ $KEY{_flds} }
		or
		$n =~ /^\s*$RE{1}\s*$/
			and push @fields => grep { $KEY{_meta}{$_}{type} eq $n } @{ $KEY{_flds} }
		or 
		push @fields => $n
	}
	join "\n\n" => map { input(\*KEY, $_, $mark) } @fields
}

sub collect_hidden {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $mark			= $KEY{_mark};
	# COLLECT=all  COLLECT=first_name, last_name,
	my @fields;
	for my $n (split(/\s*,\s*/ => $KEY{$arg} || $arg ) ) {
		$n =~ /^\s*all\s*$/i
			and push @fields => @{ $KEY{_flds} || [] }
				or push @fields => $n
	}
	join "\n\n" => map { hidden(*KEY, $_, $mark) } @fields
}

sub collect_hidden_marked {
	my $obj				= shift;
	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
	my $arg				= shift;
	my $mark			= $KEY{_mark};
	# COLLECT=all  COLLECT=first_name, last_name,
	my @fields;
	for my $n (split(/\s*,\s*/ => $KEY{$arg} || $arg ) ) {
		$n =~ /^\s*all\s*$/i
			and push @fields => @{ $KEY{_flds} || [] }
				or push @fields => $n
	}
	join "\n\n" => map { hidden_marked(*KEY, $_, $mark) } @fields
}


# display subroutine prototype
# sub XXX {
#	my $obj				= shift;
# 	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
#	my $arg				= shift;
#	my $mark			= $KEY{_mark};
#	
#}

## Alternative subroutine structure, with internal case handling
#sub OUT {
#	my $obj				= shift;
# 	local *KEY			= ref($obj) eq __PACKAGE__ ? $obj : *{ $obj };
#	my $value			= shift;
#
#	my ($sub, $name)	= split(/\s*=\s*/ => $value => 2);
#	
#	local $_			= $sub;
#	
#	/^data/i and $KEY{$name} || ''
#		or
#	/^output/i
#		and $KEY{"${name}_out"} || $KEY{"${name}.out"} || $KEY{$name} || ' '
#		or
#	''
#}



## Output utilities (exported)

## Make a select widget for data fields
sub pop_up {
	my $arg1	= shift;
	ref($arg1) or unshift @_, $arg1;
	my @idx		= @{ shift() || [] };
	my %data	= %{ shift() || {} };
	return '' unless @idx and %data;
	my $id		= shift() || '';
	my $name	= shift() || '_fields';
	my $initial	= shift() || 'Select One';
	my $pop		= qq{<select name="$name" size="1">\n};
	unless ($id) {
		$pop 	.= sprintf qq{<option value="%s">%27s</option>\n},
					'select_one', $initial;
	}

	my @out = map {
		sprintf qq{<option value="%s"%s>%24.24s</option>\n},
		 $_, ($id and /^$id$/i ? ' selected' : ''), $data{$_};
	} @idx;

	for (@out) {
		$pop .= $_
	}
	$pop .= qq{</select>};
	$pop;
}

## Make a select widget for data fields
sub pop_up2 {
	my $arg1			= shift;
	ref($arg1) or unshift @_, $arg1;
	my @options			= @{ shift() || [] };
	return '' unless @options;
	my @labels			= @{ shift() || [] };
	# make sure there's a label for each option
	my %labels;
	@labels{@options}	= map { $labels[$_] || $options[$_] } 0..$#options;
	my $name			= shift() || '_options';
	my $initial			= shift() || '';
	my $init_label		= shift() || $initial;

	my $pop		= qq{<select name="$name" size="1">\n};
	
	unless ($initial) {
		$pop 	.= sprintf qq{<option value="%s" selected>%27s</option>\n},
					'select_one', 'Select One';
	} elsif (!exists $labels{$initial}) {
		$pop 	.= sprintf qq{<option value="%s" selected>%27s</option>\n},
					$initial, $init_label;
	}
	
	$pop .= join '' => map {
		sprintf qq{<option value="%s"%s>%30.30s</option>\n},
		 $_, (/^$initial$/i ? ' selected' : ''), $labels{$_};
	} @options;

	$pop .= qq{</select>};
	$pop;
}

## Make a set of options for a select widget for data fields 
	# pop_up3_options(name, option_list_ref, label_list_ref, initial, initial_label)
sub pop_up3_options {
	my $arg1			= shift;
	ref($arg1) or unshift @_, $arg1;
	my $name			= shift() || '_options';
	my $id				= "${name}Select";
	$id					=~ s/[: -]/_/g;
	my @options			= @{ shift() || [] };
	return 'No Options' unless @options;
	my @labels			= @{ shift() || [] };
	# make sure there's a label for each option
	my %labels;
	@labels{@options}	= map { $labels[$_] || $options[$_] } 0..$#options;
	my $initial			= shift() || '';
	my $init_label		= shift() || $initial;
	
	my $pop		= '';
	
	unless ($initial) {
		$pop 	.= sprintf qq{<option value="%s" selected>%s</option>\n},
					'', 'Select One';
	} elsif (!exists $labels{$initial}) {
		$pop 	.= sprintf qq{<option value="%s" selected>%s</option>\n},
					$initial, $init_label;
	}
	
	$pop .= join '' => map {
		sprintf qq{<option value="%s"%s>%s</option>\n},
		 $_, (/^$initial$/i ? ' selected' : ''), $labels{$_};
	} @options;

	$pop;
}


## Make a select widget for data fields 
	# pop_up3(name, option_list_ref, label_list_ref, initial, initial_label, go, style)
sub pop_up3 {
	my $arg1			= shift;
	ref($arg1) or unshift @_, $arg1;
	my $name			= shift() || '_options';
	my $id				= "${name}Select";
	$id					=~ s/[: -]/_/g;
	my @options			= @{ shift() || [] };
	return 'No Options' unless @options;
	my @labels			= @{ shift() || [] };
	# make sure there's a label for each option
	my %labels;
	@labels{@options}	= map { $labels[$_] || $options[$_] } 0..$#options;
	my $initial			= shift() || '';
	my $init_label		= shift() || $initial;
	my $go				= shift() || 0;
	my $style			= shift() || '';
	
	my $pop		= qq{<select name="$name" id="$id" size="1" style="$style"} . ( $go ? $go eq '1' ? qq{ onChange="this.form.submit()"} : " $go" : "" ) . qq{>\n}; #  
	
	if (!$initial) {
		$pop 	.= sprintf qq{<option value="%s" selected>%s</option>\n},
					'', 'Select One';
	} elsif (!exists $labels{$initial}) {
		$pop 	.= sprintf qq{<option value="%s" selected>%s</option>\n},
					$initial, $init_label;
	}
	
	$pop .= join '' => map {
		sprintf qq{<option value="%s"%s>%s</option>\n},
		 $_, (/^\$initial$/i ? ' selected' : ''), $labels{$_}; # \ before $initial protects against /^*...
	} @options;

	$pop .= qq{</select>};
	$pop;
}


## Make a select widget for data fields 
	# pop_up4(name, option_list_ref, label_list_ref, initial, initial_label, )
sub pop_up4 {
	my $arg1			= shift;
	ref($arg1) or unshift @_, $arg1;
	my $name			= shift() || '_options';
	my $id				= "${name}Select";
	$id					=~ s/[: -]/_/g;
	my $changeFlagID	= "${id}_changed";
	my @options			= @{ shift() || [] };
	return 'No Options' unless @options;
	my @labels			= @{ shift() || [] };
	# make sure there's a label for each option
	my %labels;
	@labels{@options}	= map { $labels[$_] || $options[$_] } 0..$#options;
	my $initial			= shift() || '';
	my $init_label		= shift() || $initial;
	my $go				= shift() || 0;

	my $pop		= qq{<select name="$name" id="$id" size="1" onChange="document.getElementById('$changeFlagID').value=1;">\n}; #  
	
	unless ($initial) {
		$pop 	.= sprintf qq{<option value="%s" selected>%s</option>\n},
					'', 'Select One';
	} elsif (!exists $labels{$initial}) {
		$pop 	.= sprintf qq{<option value="%s" selected>%s</option>\n},
					$initial, $init_label;
	}
	
	$pop .= join '' => map {
		sprintf qq{<option value="%s"%s>%s</option>\n},
		 $_, (/^$initial$/i ? ' selected' : ''), $labels{$_};
	} @options;

	$pop .= qq{</select><input type="hidden" name="$changeFlagID" id="$changeFlagID" value="0">};
	$pop;
}



## Make a select widget for multiple selections
	# pick(name, option_list_ref, label_list_ref, initials_ref, initial_labels_ref, size, width, notMulti)
sub pick {
	my $arg1			= shift;
	ref($arg1) or unshift @_, $arg1;
	my $name			= shift() || '_options';
	my @options			= @{ shift() || [] };
	#return '' unless @options;
	my @labels			= @{ shift() || [] };
	# make sure there's a label for each option
	my %labels;
	@labels{@options}	= map { $labels[$_] || $options[$_] } 0..$#options;
	my @initials		= @{ shift() || [] };
	my @init_labels		= @{ shift() };
	@init_labels or @init_labels = @initials;
	my %init_labels;
	if (@initials) {
		@init_labels{@initials}	= map { $init_labels[$_] || $initials[$_] } 0..$#initials;
	}
	my $size			= shift() || '3';
	
	my $width			= shift() || '24';
	
	my $not_multi		= shift() || 0;

	my $pop 			= join '' => map {
		sprintf qq{<option value="%s"%s>%${width}.${width}s</option>\n},
		 $_, (exists $init_labels{$_} and delete $init_labels{$_} ? ' selected' : ''), $labels{$_};
	} @options;
	
	unless (@initials) {
		$pop 			= sprintf qq{<option value="%s" selected>%27s</option>\n%s},
					'', ' -&gt; Select One' . ($not_multi ? '' : ' or More') . ' Below: ', $pop;
	} else {
		$pop			= join( '' => map {
			exists $init_labels{$_} ?
			sprintf( qq{<option value="%s"%s>%${width}.${width}s</option>\n},
			 $_, ' selected', $init_labels{$_}) : ()
		} @initials) . $pop
	}
	qq{<select name="$name" size="$size"} . ($not_multi ? '' : 'multiple') . qq{>\n${pop}</select>};
}



1;

