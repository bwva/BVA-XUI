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

use BVA::XUI;
use BVA::XUI::DATA;
use BVA::XUI::DATA::WHERE::Parser;

my $ui		= BVA::XUI->init();
my $sup	= $ui->new({'_mark' => 'SUP', '_start' => '[', '_end' => ']'});

if ($sup->input_data('show_subs')) {
	print 'DATA::WHERE', "\n===========\n";
	print join("\n" => "Functions:\n--------", @{ Class::Inspector->functions( 'DATA::WHERE' ) }), "\n---\n\n";
	print join("\n" => "Methods:\n--------", @{ Class::Inspector->methods( 'DATA::WHERE', 'full') }), "\n---\n\n";	
#	print join("\n" => "Methods:\n--------", map { join("\n\t" => @{$_},"\n") } @{ Class::Inspector->methods( 'DATA::WHERE', 'public', 'expanded') }), "\n---\n";

	print 'BVA::XUI::DATA', "\n===========\n";
	print join("\n" => "Functions:\n--------", @{ Class::Inspector->functions( 'BVA::XUI::DATA' ) }), "\n---\n";
	print join("\n" => "Methods:\n--------", @{ Class::Inspector->methods( 'BVA::XUI::DATA', 'full') }), "\n---\n";	
#	print join("\n" => "Methods:\n--------", map { join("\n\t" => @{$_},"\n") } @{ Class::Inspector->methods( 'BVA::XUI::DATA', 'public', 'expanded') }), "\n---\n";
}

is($sup->data('_mark'), 'SUP', 'mark should be \'SUP\'');

ok($sup->dbx_connect({db => '/Users/bwva/Documents/Development/KALE/supporters.txt',input_sep => "\t"}), 'dbx_connect should load db' );


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

my $verbose				= $sup->input_data('verbose') || 0;
my $show_first_round	= $sup->input_data('show_first_round') || 0;
my $textnum	= 0;
my @tests;

my $parser = BVA::XUI::DATA::WHERE::Parser->new() or die "Bad grammar!\n";

while (my $testline = <DATA>) {

	next if $testline =~ /^\#/;

	chomp $testline;
	
	last unless $testline;

	$textnum++;
	
	my ($ok, $text)	= split /\s*,\s*/ => $testline, 2;
	
	my $err_test	= $ok ? 0 : 1;
	my $err_phrase	= $err_test ? "Expected error on invalid statement #$textnum." : "Should be no errors on valid statement $textnum.";
	
	print "\n** Text $textnum:\n", $text, "\n";
	
	if ($show_first_round) {
		my $where	= $text;
	
# 		my @parts	= $sup->dbh->_parser($where);
 		my @parts	= $parser->list($text);
#		my $tree	= $parser->list($text);
 		print "a. Parts:\n", Dumper(\@parts),"\n--\n" if $verbose;
#		print "a. Parts:\n", Dumper($tree),"\n--\n" if $verbose;

 		my @chunks	= $sup->dbh->_chunker(@parts);
#  		my @chunks	= $sup->dbh->_chunker($tree);
 		print "b. Chunks:\n", Dumper(\@chunks),"\n----\n" if $verbose;

 		my @groups	= $sup->dbh->_grouper(@chunks);
		print "c. Groups:\n", Dumper(\@groups),"\n----\n" if $verbose;

		my @tagged_groups	= $sup->dbh->_tagger( @groups );
		print "d. Tagged Groups:\n", Dumper(\@tagged_groups),"\n----\n" if $verbose;
	
	# 	my $selector1		= $sup->dbh->_parse_conditions( @tagged_groups );
	# 	print "e. Selector:\n", $sup->dumpit($selector1),"\n----\n" if $verbose;
	}

 	my $selector1	= $sup->dbh->make_selector($text);	
	print "e. Selector:\n", $sup->dumpit($selector1),"\n----\n" if $verbose && $show_first_round;

	is( $selector1->{error}, $err_test, $err_phrase);
	
	my $selector2	= $sup->dbh->make_selector($selector1->{where});
	
	SKIP: {
		if ($selector1->{error}) {	
			skip( "because original selector had errors for statement $textnum.", 2);
		}
		
		is( $selector2->{error}, 0, "Should be no errors in re-done selector for statement $textnum.");
		
		SKIP: {
			if ($selector2->{error}) { 
				print $selector1->{where}, "\n";

# 				my @parts	= $sup->dbh->_parser($selector1->{where});
 				my @parts	= $parser->list($text);
#				my $tree	= $parser->list($text);
 				print "a. Parts:\n", Dumper(\@parts),"\n--\n" if $verbose;
#				print "a. Parts:\n", Dumper($tree),"\n--\n" if $verbose;

  				my @chunks	= $sup->dbh->_chunker(@parts);
#  				my @chunks	= $sup->dbh->_chunker($tree);
 				print "b. Chunks:\n", Dumper(\@chunks),"\n----\n" if $verbose;

 				my @groups	= $sup->dbh->_grouper(@chunks);
				print "c. Groups:\n", Dumper(\@groups),"\n----\n" if $verbose;

				my @tagged_groups	= $sup->dbh->_tagger( @groups );
				print "d. Tagged Groups:\n", Dumper(\@tagged_groups),"\n----\n" if $verbose;

				skip("Because the re-done selector had errors for statement $textnum.", 1);

			}

			is($selector2->{where},$selector1->{where}, "re-done \'where\' should be same as original, at statement $textnum");
		}
	}

	push @tests => [$selector1,$selector2];	
}

done_testing();

#print join( "\n" => map { $_->[0]{where}||"Sumpin'" . "\nTO\n" . $_->[1]{where}||"_Nuttin'" . "\n-----\n" } @tests) if $verbose;


__END__
1,last ^ van AND city+~santa cruz OR last #Allen AND zip+^950cd 
1,city+=~(San .*|Los .*)
1,city+~(san|gil|wat) OR rec_id*
1,home_city=~(San .*|Los .*) OR work_city=~(San .*|Los .*) OR alt_city=~(San .*|Los .*)
1,(home_city~(san|gil|wat) OR (work_city~(san|gil|wat) OR alt_city~(san|gil|wat))) OR rec_id*
1,(home_city~(san|gil|wat) OR work_city~(san|gil|wat) OR alt_city~(san|gil|wat)) OR rec_id*'
1,last ^ van AND ((home_city = santa Cruz OR work_city = santa Cruz OR alt_city = santa Cruz) OR (home_city = capitola OR work_city = capitola OR alt_city = capitola))
1,last=smith AND (city+=santa Cruz OR city+=capitola)
1,last=smith AND ((home_city=santa Cruz OR work_city=santa Cruz OR alt_city=santa Cruz) OR (home_city=capitola OR work_city=capitola OR alt_city=capitola))
1,((home_city=santa Cruz OR work_city=santa Cruz OR alt_city=santa Cruz) OR (home_city=capitola OR work_city=capitola OR alt_city=capitola)) AND last=smith
1,(city+=santa Cruz OR city+=capitola) AND last=smith
0,(city+=santa Cruz OR city+=capitola) AND lastname=smith
0,last=smith AND (city=Santa Cruz OR zip=95060) OR last=jones AND (city=Capitola OR zip=95010) OR ?=whatever
1,(home_city=Santa Cruz OR home_zip=95060)
0,(last=smith AND (city=Santa Cruz OR zip=95060)) OR (last=jones AND (city=Capitola OR zip=95010)) OR ?=whatever
1,(home_city=Santa Cruz)
0,last=jones AND (city=Capitola OR zip=95010) OR ?=whatever
0,(city=Santa Cruz OR zip=95060) OR last=jones AND (city=Capitola OR zip=95010)
1,rec_id !* AND (first=Tex OR mid=Tex) AND (home_city=santa cruz OR work_city=santa cruz) AND (home_zip ^ 950 OR work_zip ^ 95)
1,(home_city = santa Cruz OR work_city = santa Cruz OR alt_city = santa Cruz) AND (home_city = capitola OR work_city = capitola OR alt_city = capitola)
1,((home_city = santa Cruz OR work_city = santa Cruz OR alt_city = santa Cruz) OR (home_city = capitola OR work_city = capitola OR alt_city = capitola))
1,city+ ^ san AND zip+ ^ 95
0,city+ ^ san AND zip+ ^ 95)
0,(city+ ^ san AND zip+ ^ 95
1,alerts=~(Yes|No|Maybe)
1,( home_city~(san|gil|wat) OR work_city~(san|gil|wat) OR alt_city~(san|gil|wat) ) OR rec_id*
1,(home_city^san OR work_city^san OR alt_city^san) AND (email* OR alt_email* OR bad_email*)
1,(home_city=~(San .*|Los .*) OR work_city=~(San .*|Los .*) OR alt_city=~(San .*|Los .*))
1,home_city=~(San .*|Los .*) OR work_city=~(San .*|Los .*) OR alt_city=~(San .*|Los .*)
1,city+ ^ san
1,city+ ^ san AND first ^ B
0,(city+ ^ san AND zip+ ^ 95
1,last=smith
1,last=smith,first^b
0,last=smith AND city=santa Cruz AND address+ ^ H 
1,(home_city ^san OR work_city ^san OR alt_city ^san) AND (home_zip ^ 95 OR home_zip_four ^ 95 OR home_zip_plus_four ^ 95 OR work_zip ^ 95 OR work_zip_four ^ 95 OR work_zip_plus_four ^ 95 OR alt_zip ^ 95 OR alt_zip_four ^ 95 OR alt_zip_plus_four ^ 95 OR mail_zip ^ 95) 
1,((home_city ^san OR work_city ^san OR alt_city ^san) AND (home_zip ^ 95 OR home_zip_four ^ 95 OR home_zip_plus_four ^ 95 OR work_zip ^ 95 OR work_zip_four ^ 95 OR work_zip_plus_four ^ 95 OR alt_zip ^ 95 OR alt_zip_four ^ 95 OR alt_zip_plus_four ^ 95 OR mail_zip ^ 95))
1,last=smith AND first^W AND (home_city=santa cruz OR work_city=santa cruz OR alt_city=santa cruz) 
1,(first=Tex OR mid=Tex) AND (home_city=santa cruz OR work_city=santa cruz OR alt_city=santa cruz) 
0,((last = smith AND (city = Santa Cruz OR zip = 95060)) OR (last = jones AND (city = Capitola OR zip = 95010)) OR ? = whatever)
0,(last=smith AND (city=Santa Cruz AND (zip=95060 OR zip=95062))) OR (last=jones AND (city=Capitola OR zip=95010)) OR ?=whatever
0,(last=smith AND (city=Santa Cruz AND (zip=95060 OR zip=95062))) OR (last=jones AND (city=Capitola OR zip=95010)) OR ?=whatever
0,(last=smith AND (city=Santa Cruz OR (zip=95060 OR zip=95062))) AND (last=jones AND (city=Capitola OR zip=95010)) OR ?=whatever
0,(last=smith AND (city=Santa Cruz OR zip=95060)) OR (last=jones AND (city=Capitola OR zip=95010)) OR ?=whatever
0,(last=smith AND (city=Santa Cruz OR zip=95060)) OR (last=jones AND city=Capitola) OR ?=whatever
0,last=smith AND (city=Santa Cruz OR zip=95060)
0,last=smith AND (city=Santa Cruz OR zip=95060) OR ?=whatever
