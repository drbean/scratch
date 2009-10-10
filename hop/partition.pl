#!/usr/bin/perl 

use strict;
use warnings;

sub partitioner {
	my $number = shift;
	my @agenda = [$number];
	return sub {
		return unless @agenda;
		my ($head, @rest) = @{ pop @agenda };
		my $min = $rest[0] || 1;
		my $max = $head / 2;
		for my $j ( $min .. $max ) {
			unshift @agenda, [ $head - $j, $j, @rest ];
		}
		return [$head, @rest];
	}
}

my $part = partitioner(7);
print map { @{ $part->() }, "\n" } 1..15;

sub dfs {
	my ($agenda, $children) = @_;
	return sub {
		my $node = pop @$agenda;
		push @$agenda, $children->($node);
		return $node;
	}
}

sub dfspartitioner {
	my $number = shift;
	my $root = [ $number ];
	my $callback = sub {
		my @newnodes;
		my ( $head, @rest ) = @{ shift() };
		my $min = $rest[0] || 1;
		my $max = $head / 2;
		for my $j ( $min .. $max ) {
			unshift @newnodes, [ $head - $j, $j, @rest ];
		}
		return @newnodes;
	};
	return dfs($root, $callback);
}
