package Partition;

use strict;
use warnings;

BEGIN {
	use Exporter ();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);
               $VERSION     = 1.00;
               @ISA         = qw(Exporter);
               @EXPORT      = qw(p make_partitioner);
               %EXPORT_TAGS = ( );
               @EXPORT_OK   = qw();
           }
	
sub make_partitioner {
	my ($target, $treasures) = @_;
	my @todo = ( [$target, $treasures, []] );
	return sub {
		while ( @todo ) {
			my $cur = pop @todo;
			my ($target, $pool, $share) = @$cur;
			if ($target == 0) { return $share; }
			next if $target < 0 || @$pool == 0;
			my ($first, @rest) = @$pool;
			push @todo, [$target-$first, \@rest, [@$share, $first]],
					[$target, \@rest, $share];
		}
		return undef;
	};
}

sub partition {
	my ($target, $treasures) = @_;
	return [] if $target == 0;
	return () if $target < 0 || @$treasures == 0;
	my ($first, @rest) = @$treasures;
	my @solutions = partition( $target-$first, \@rest);
	return ((map {[$first, @$_]} @solutions), partition($target, \@rest));
}

sub find_share {
	my ($target, $treasures) = @_;
	return [] if $target == 0;
	return if $target < 0 || @$treasures == 0;
	my ($first, @rest) = @$treasures;
	my $solution = find_share( $target-$first, \@rest);
	return [$first, @$solution] if $solution;
	return find_share($target, \@rest);
}

sub hackfind_share {
	my ($target, $treasures, @solutions) = @_;
	return [] if $target == 0;
	return if $target < 0 || @$treasures == 0;
	my ($first, @rest) = @$treasures;
	my $solution = find_share( $target-$first, \@rest, @solutions);
	return ([$first, @$solution], @solutions) if $solution;
	return find_share($target, \@rest, @solutions);
}

1;
