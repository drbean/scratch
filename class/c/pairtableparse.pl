#!/usr/bin/perl

use strict;
use warnings;

use IO::All;
use YAML qw/DumpFile/;

my $io = io 'round2.pairingtable';
my @r = $io->slurp;
my (%opponents, %points);
for my $r ( @r ) {
	my ( $id, $opponent, $role, $score ) = split ' ', $r;
	chomp $score;
	$opponents{$id} = $opponent;
	$points{$id} = $score;
}

DumpFile 'first/1.yaml', \%opponents, \%points;


