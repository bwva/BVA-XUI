#!/usr/bin/env perl

# Date:		2013-03-22

use strict;
use warnings;

use lib '../';

use Make_Parser;

Make_Parser->build_parser()
  	or die "Couldn't rebuild parser: $@\n$!";

__END__
