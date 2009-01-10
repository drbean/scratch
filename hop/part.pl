#!/usr/bin/perl

use strict;
use warnings;

use Partition;

my $i = make_partitioner(6,[2,3,4,6]);

while (my $share = $i->() ) { print @$share; }

sub imap {
	my ($sub, $it) = @_;
	return sub { 
		my ($sub, $it) = @_;
		return $sub->($it->());
	}
}
