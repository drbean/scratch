#!/usr/bin/perl

use strict;
use warnings;

use MyStream;

my $i = upto( 3 , 7 );

# for (0..7) { my $rest = tail($i); print $rest->[0]; $i = $rest }

print "\n";
my $j = upfrom(3);

# print head tail tail tail tail tail tail tail tail tail tail tail tail tail $j;

# show($j,20);

my $powers;
$powers = sub {
	my $x = shift;
	warn $x;
	cons ( $x, promise { $powers->($x * 2) } );
};

# show $powers->(2), 4;

my $powers2;

my $n;
$powers2 = cons ( 1, promise { maps {  warn $n++.$_[0]."\n";$_[0] * 2 } $powers2 } );

# show $powers2, 10;

my $hamming;
#$hamming = cons( 1, promise {
#		merge ( ( maps { $_*2 } $hamming ),
#		merge( ( maps { $_*3 } $hamming ),
#		( maps { $_*5 } $hamming )) ) } );

$hamming = cons( 1, promise {
		merge ( scale( $hamming, 2 ),
		merge( scale( $hamming, 3 ),
			scale( $hamming, 5 )) ) } );

show $hamming, 24;

# print show(scale( $powers2, 3 ), 3)
