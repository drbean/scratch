#!perl

use strict;
use warnings;
use lib 'lib';

use Config::General;
use Cwd;
use File::Spec;
use List::MoreUtils qw/all/;
use YAML qw/LoadFile/;

BEGIN {
	my @MyAppConf = glob( '*.conf' );
	die "Which of @MyAppConf is the configuration file?"
				unless @MyAppConf == 1;
	my %config = Config::General->new($MyAppConf[0])->getall;
	$::name = $config{name};
	require "$::name.pm"; $::name->import;
	require "$::name/Schema.pm"; $::name->import;
}

my @leagueids = qw/GL00003 GL00022 GL00031 CLA0013 FLA0015 FLB0002 MIA0012 access visitors/;
my $dir = ( File::Spec->splitdir(getcwd) )[-1];
$dir = qr/^(GL000|CLA|FL|MIA)/ if $dir eq 'dic';
@leagueids = grep m/$dir/, @leagueids;

no strict qw/subs refs/;
my $connect_info = "${::name}::Model::DB"->config->{connect_info};
# my $connect_info = [ 'dbi:SQLite:db/demo','','' ];
my $schema = "${::name}::Schema"->connect( @$connect_info );
use strict;

my $books = [
		[ qw/id name booklet cd/ ],
	[ "justright", "Just Right Pre-Intermediate American Edition", "yes", "yes" ],
	[ "marketleader", "New Edition--Market Leader Pre-intermediate Business English", "no", "yes" ],
	[ "beyondlanguage", "Beyond Language: Cross-cultural communication (Second edition)", "no", "no" ],
	];

uptodatepopulate( 'Book', $books );

sub uptodatepopulate
{
	my $class = $schema->resultset(shift);
	my $entries = shift;
	my $columns = shift @$entries;
	foreach my $row ( @$entries )
	{
		my %hash;
		@hash{@$columns} = @$row;
		$class->update_or_create(\%hash);
	}
}


=head1 NAME

script_files/books.pl.pl - populate books tables with id, name, booklet, cd

=head1 SYNOPSIS

perl script_files/books.pl

=head1 DESCRIPTION

INSERT INTO books (id, name, booklet, cd) VALUES (?, ?, ?, ?)

=head1 AUTHOR

Dr Bean, C<drbean at (@) cpan dot, yes a dot, org>

=head1 COPYRIGHT


This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
