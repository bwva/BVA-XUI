package Make_Parser;

# Date:		2013-03-31

use strict;
use warnings;

use Parse::RecDescent;
	
sub build_parser {
	my $self	= shift;
	$self->build_RD_parser()
		or return;
	1;
}

sub build_RD_parser {
	my $self	= shift;
	my $parser_name	= shift || 'Parser';

	my $grammar = q {
	  list: <leftop: item /(AND|OR)/ item>
	  item: '(' list ')'  {  $item[2]  }
			| word /(AND|OR)/ '(' list ')' { [ @item[1,2], @{$item[4]} ]  }
			| word /(?<! [[:punct:]])\(/ list ')'  { [ @item[1,3] ] }
			| word /(AND|OR)/ list  { [ @item[1,2], (@{$item[3]}==1 ? $item[3][0] : @{$item[3]}) ] }
				| /(?<! [[:punct:]])\(/ list ')'  { [ $item{list} ] }
			| word /(AND|OR)/ word  { [ @item[1,2,3] ] }
				| word { $item{word} }
		word:  /[^()]+[:=><~!^#*@]{1,2}\([^()]+\)/ | /[^()]+/ 

	 };

	Parse::RecDescent->Precompile({ '-standalone' => 1, },
				$grammar, "BVA::XUI::DATA::WHERE::Parser")
				and print "Parser created.\n"
				or die "Couldn't create parser: $@, $!";
				
	return 1;
}

1;