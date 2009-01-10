#!/usr/bin/perl 

use strict;
use warnings;

use SVK;
use SVK::XD;
my $xd = SVK::XD->new (depotmap => { '' => '/home/drbean/.svk/local'});

$::output = undef;
my $svk = SVK->new (xd => $xd, output => \$::output);

$svk->ls ('//'); # check $output for its output

use RepoSearch;

$svk = RepoSearch->new;
my $i = iterator($svk, '//', sub { shift =~ m/perl/x });

while ( my $node = $i->() ) { print $node, "\n"; }
