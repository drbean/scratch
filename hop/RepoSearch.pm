package RepoSearch;

use strict;
use warnings;

BEGIN {
	use Exporter ();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);
               $VERSION     = 1.00;
               @ISA         = qw(Exporter);
               @EXPORT      = qw(search iterator iteratorer);
               %EXPORT_TAGS = ( );
               @EXPORT_OK   = qw();
           }

use SVK;
use SVK::XD;

use lib 'Function/lib';
use Function;

sub new {
	my $self = shift;
	my $xd = SVK::XD->new (depotmap => { '' => '/home/drbean/.svk/local'});
	my $path = '//';
	return SVK->new (xd => $xd, output => \$::output);
}

1;

#sub ls
#{
#	my $self = shift;
#	$self->SUPER::ls(shift);
#	print $output;
#}

sub iteratorer
{
	my $svk = shift;
	my $path = shift;
	my $search = shift;
	$svk->ls ( $path, '-f');
	$path .=  $::output;
	my $nodeiterator = sub { $path =~ m/^(.*)$/gm; return $1 };
	return imap
		{
			my $node = shift;
			if ( $node =~ m/\/$/ ) {
				$svk->ls( $node, '-f');
				$path .=  $::output;
			}
			return $node if $search->($node);
		} $nodeiterator;
}

sub iterator
{
	my $svk = shift;
	my $path = shift;
	my $search = shift;
	$svk->ls ( $path);
	chomp $::output;
	my @nodes = map { $path . $_ } grep m/\/$/, split "\n", $::output;
	return sub {
		while ( @nodes )
		{
			my $node = shift @nodes;
			if ( $node =~ m/\/$/ ) {
				$svk->ls( $node );
				chomp $::output;
				my @subnodes = split "\n", $::output;
				@subnodes = map { $node . $_ } @subnodes;
				push @nodes, @subnodes;
			}
			return $node if $search->($node);
		}
		return;
	}
}

sub search
{
	my $svk = shift;
	my $path = shift;
	my $search = shift;
	$svk->ls ( $path);
	chomp $::output;
	my @nodes = split "\n", $::output;
	for my $node (@nodes)
	{
		print $path . $node, "\n" if $search->($node);
	}
	return map { search( $svk, $path . $_, $search ) } (grep (m/\/$/, @nodes));
}

1;
