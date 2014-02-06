#!/usr/bin/env perl

# Date:		2013-03-11

# Tests for DATA::WHERE::make_selector

use Carp qw( confess );
$SIG{__DIE__} =  \&confess;
$SIG{__WARN__} = \&confess;

use strict;
use warnings;

use Test::More;

use Data::Dumper;

use Class::Inspector;

$|++;

use lib '/Library/WebServer/CGI-Executables';
use lib '/Library/WebServer/CGI-Executables/BVA/XUI';
use lib '/Library/WebServer/CGI-Executables/BVA/XUI/DATA';
use lib '/Library/WebServer/CGI-Executables/BVA/XUI/DATA/WHERE';

use BVA::XUI;
use BVA::XUI::DATA;
use BVA::XUI::DATA::WHERE;

use Parse::RecDescent;
$::RD_HINT	= 1;
# my $grammar = q {
#   list: <leftop: item /(AND|OR)/ item>
#   item: '(' list ')'  { [ $item[2] ] }
#   		| word /(AND|OR)/ '(' list ')' { [ @item[1,2,4] ] }
#   		| word /(?<! [[:punct:]])\(/ list ')'  { [ @item[1,3] ] }
#   		| word /(AND|OR)/ list  { [ @item[1,2,3] ] }
# 	  		| /(?<! [[:punct:]])\(/ list ')'  { [ $item{list} ] }
#   				| word /(AND|OR)/ word  { [ @item[1,2,3] ] }
# 	      			| word { $item{word} }
# 	word:  /[^()]+[:=><~!^#*@]{1,2}\([^()]+\)/ | /[^()]+/ 
# 
#  };    #word: /([^()](?!AND))+/    #

#   		| word /(AND|OR)/ '(' list ')' { [ @item[1,2,4] ] }

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


my $parser = new Parse::RecDescent ($grammar) or die "Bad grammar!";

my $textnum;
while (my $testline = <DATA>) {

	next if $testline =~ /^\#/;

	chomp $testline;
	
	last unless $testline;

	$textnum++;
	
	my ($ok, $text)	= split /\s*,\s*/ => $testline, 2;
	
	my $err_test	= $ok ? 0 : 1;
	my $err_phrase	= $err_test ? "Expected error on invalid statement #$textnum." : "Should be no errors on valid statement $textnum.";
	
	print "\n** Text $textnum:\n", $text, "\n";
	
	our @parts;

	@parts	= $parser->list($text);
	print Dumper(\@parts), "\n\n";

}

# Parse::RecDescent->Precompile({ '-standalone' => 1, },
#             $grammar, "BVA::XUI::DATA::WHERE::Parser")
#             and print "Parser created.\n"
#             or die "Couldn't create parser: $@, $!";



__END__
1, home_city=~(San .*|Los .*) OR work_city=~(San .*|Los .*) OR alt_city=~(San .*|Los .*)
1,city+~(san|gil|wat)
1,last=smith AND (city=Santa Cruz OR zip=95060) OR last=jones AND (city=Capitola OR zip=95010) OR ?=whatever
1,(last=smith AND (city=Santa Cruz OR zip=95060) OR last=jones AND (city=Capitola OR zip=95010) OR ?=whatever)
1,city+~(san|gil|wat) OR rec_id*
1,last=smith AND (city+=santa Cruz OR city+=capitola)
1,last=smith AND ((home_city=santa Cruz OR work_city=santa Cruz OR alt_city=santa Cruz) OR (home_city=capitola OR work_city=capitola OR alt_city=capitola))
1,blah1,blah2(blah3,blah4(blah5,blah6(blah7))),blah8
1,blah1,blah2(blah3,blah4(blah5,blah6(blah7))),blah8
