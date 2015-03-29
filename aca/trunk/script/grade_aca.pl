#!perl

use strict;
use warnings;
use lib 'lib';
use FindBin '$Bin';
use Pod::Usage;

use Config::General;
use YAML qw/Bless Dump LoadFile/;
use List::Util qw/reduce/;
use Moose::Autobox;
use Aca::Model::DB;
use Aca::Schema;

use Grades;

package Script;

use Moose;
with 'MooseX::Getopt';

has 'man'  => ( is => 'ro', isa => 'Bool' );
has 'help' => ( is => 'ro', isa => 'Bool' );
has 'league' => (
    traits => ['Getopt'], is => 'ro', isa => 'Str', required => 0,
    cmd_aliases => 'l',);
has 'exercise' => (
    traits => ['Getopt'], is => 'ro', isa => 'Str', required => 0,
    cmd_aliases => 'x',);
has 'quitter' => (
    traits => ['Getopt'], is => 'ro', isa => 'Int', required => 0,
    cmd_aliases => 'q',);
has 'loser' => (
    traits => ['Getopt'], is => 'ro', isa => 'Int', required => 0,
    cmd_aliases => 'o',);
has 'winner' => (
    traits => ['Getopt'], is => 'ro', isa => 'Int', required => 0,
    cmd_aliases => 'w',);

package main;

my $config = LoadFile "aca.yaml";
my $connect_info = Aca::Model::DB->config->{connect_info};
my $schema = Aca::Schema->connect( $connect_info );

my $script = Script->new_with_options;
my $id = $script->league;
my $exercise = $script->exercise;
my $man = $script->man;
my $help = $script->help;

pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

# ( my $leagueid = $id ) =~ s/^([[:alpha:]]+[[:digit:]]+).*$/$1/;
my $leagueid = $id;
my $league = League->new( id => $leagueid );
my $members = $league->members;
my %members = map { $_->{id} => $_ } @$members;
my ($report, $card);
$report->{exercise} = $exercise;
my $words = $schema->resultset("Word")->search({
		exercise => "academic" });
my $answers = $schema->resultset("Play")->search({
		league => $id });
my $score_spread = 0;
for my $player ( keys %members ) {
	my $standing = $answers->search({ player => $player, exercise => $exercise });
	my $base = $answers->search({ player => $player, exercise => 'computing' });
	my $improvement;
	if ( $standing and $standing != 0 ) {
		my $post_total = $standing->count;
		my $pre_total= $base->count;
		$words->reset;
		my ($pre_correct, $post_correct, $targeted, $improvement) = (0) x 4;
		my $target_flag;
		while ( my $word = $words->next ) {
		    my $head = $word->head;
		    next if $head eq 'shift' or $head eq 'course';
		    my $pre = $base->find({word => $head});
# $DB::single=1 unless $pre and $pre->answer;
		    if ( $pre and $pre->answer eq $word->answer ) {
				$pre_correct++;
			}
		    elsif ( $pre and $pre->answer ne $word->answer ) {
				$targeted++;
				$target_flag = 1;
			}
		    my $post = $standing->find({word => $head});
		    if ( $post and $post->answer eq $word->answer ) {
				$post_correct++;
				$improvement++ if $target_flag;
			}
			$target_flag = 0;
		}
		$report->{points}->{$player}->{pre_test}->{attempted} = $pre_total;
		$report->{points}->{$player}->{pre_test}->{correct} = $pre_correct;
		$report->{points}->{$player}->{post_test}->{attempted} = $post_total;
		$report->{points}->{$player}->{post_test}->{correct} = $post_correct;
		$report->{points}->{$player}->{post_test}->{targeted} = $targeted;
		$report->{points}->{$player}->{post_test}->{improvement} =$improvement;
	}
	else {
		$report->{points}->{$player}->{answers} = 0;
		$report->{points}->{$player}->{correct} = 0;
	}
	$score_spread = $report->{points}->{$player}->{post_test}->{improvement} if
		$report->{points}->{$player}->{post_test}->{improvement} > $score_spread;
}

for my $player ( keys %members ) {
	$report->{exercise} = $exercise;
	if ( $report->{points}->{$player}->{post_test}->{attempted} ) {
		$report->{grade}->{$player} = sprintf( "%.2f", 3 + 2 *
			$report->{points}->{$player}->{post_test}->{improvement} / $score_spread )
	}
	else {
		$report->{grade}->{$player} = 0;
	}
	Bless( $report->{points}->{$player} )->keys(
		[ qw/pre_test post_test/ ] );
}

print Dump $report, $report->{grade};

=head1 NAME

grade_aca.pl - record results from aca DB

=head1 SYNOPSIS

perl script_files/grade_aca.pl -l GL00016 -x sports-test > ../001/GL00016/exam/g.yaml

=cut

=head1 DESCRIPTION

Above 20 percent, grade of hwMax/2. Above 85 percent of the letters, a (perfect) grade of hwMax. No roles. Uses play table, rather than words. If no -o or -t (one and two) options, then correct/total percent of hwMax.

=cut

=head1 AUTHOR

Dr Bean, C<drbean at (@) cpan dot, yes a dot, org>

=head1 COPYRIGHT


This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
