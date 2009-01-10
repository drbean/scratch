#!/usr/bin/perl

use FileHandle;

sub it { $fh = shift; return sub {<$fh>} }

open (FH, "bingo.pl");
$fit = it (*FH);

while (defined($line = &$fit )) { print $line }
