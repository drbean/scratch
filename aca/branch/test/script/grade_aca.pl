#!perl

use strict;
use warnings;
use lib 'lib';
use FindBin '$Bin';
use Pod::Usage;

use Config::General;
use YAML qw/Bless Dump LoadFile/;
use List::Util qw/reduce max/;
use List::MoreUtils qw/all any/;
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
has 'base' => (
    traits => ['Getopt'], is => 'ro', isa => 'Str', required => 0,
    cmd_aliases => 'b',);

package main;

my $config = LoadFile "aca.yaml";
my $connect_info = Aca::Model::DB->config->{connect_info};
my $schema = Aca::Schema->connect( $connect_info );

my $script = Script->new_with_options;
my $id = $script->league;
my $exercise = $script->exercise;
my $base = $script->base or $exercise . "_base";
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
my $word_bank = $schema->resultset("Word")->search({
		exercise => $exercise });
my @heads = $word_bank->get_column('head')->func('DISTINCT');
my $alternative = $schema->resultset("Alternative")->search({
	exercise => $exercise});
my $wc = @heads;
my $answers = $schema->resultset("Play")->search({
		league => $id });
my $score_spread = 1;
my $participants = 0;
my $class_total = { pre_test => { attempted => 0, correct => 0 }
			, post_test => {
				attempted => 0, correct => 0, targeted => 0, improvement => 0 }
			, grade => 0
		};
for my $player ( keys %members ) {
	my $standing = $answers->search({ player => $player, exercise => $exercise });
	my $base = $answers->search({ player => $player, exercise => $base });
	my $improvement;
	if ( $standing and $standing != 0 ) {
		my $post_total = $standing->count;
		my $pre_total= $base->count;
		$word_bank->reset;
		my ($pre_correct, $post_correct, $targeted, $improvement) = (0) x 4;
		my $target_flag;
		for my $head ( @heads ) {
		    next if $head eq 'shift' or $head eq 'course' or $head eq 'first';
		    my $pre = $base->find({word => $head});
# $DB::single=1 unless $pre and $pre->answer;
			my ( @the_answers, @alt_answers );
			my $words = $word_bank->search({ head => $head });
			while ( my $word = $words->next ) {
				push @the_answers, $word->answer;
			}
			my $alt = $alternative->search({ head => $head });
			while ( my $word = $alt->next ) {
				push @alt_answers, $word->answer;
			}
			$alternative->reset;
			if ( $pre and $pre->answer and all { $_ eq $pre->answer } @the_answers ) {
				$pre_correct++;
			}
		    elsif ( $pre and $pre->answer and all { $_ ne $pre->answer } @the_answers ) {
				$targeted++;
				$target_flag = 1;
			}
		    my $post = $standing->find({word => $head});
		    if ( $post and $post->answer and ( any { $_ eq $post->answer } @the_answers
					or any { $_ eq $post->answer } @alt_answers ) ) {
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
		$class_total->{pre_test}->{attempted} += $pre_total;
		$class_total->{pre_test}->{correct} += $pre_correct;
		$class_total->{post_test}->{attempted} += $post_total;
		$class_total->{post_test}->{correct} += $post_correct;
		$class_total->{post_test}->{targeted} += $targeted;
		$class_total->{post_test}->{improvement} +=$improvement;
		$card->{$player} = $improvement;
		$participants++ if $targeted;
	}
	else {
		$report->{points}->{$player}->{pre_test}->{attempted} = 0;
		$report->{points}->{$player}->{pre_test}->{correct} = 0;
		$report->{points}->{$player}->{post_test}->{attempted} = 0;
		$report->{points}->{$player}->{post_test}->{correct} = 0;
		$report->{points}->{$player}->{post_test}->{targeted} = 0;
		$report->{points}->{$player}->{post_test}->{improvement} = 0;
		$card->{$player} = 0;
	}
	$score_spread = $report->{points}->{$player}->{post_test}->{improvement} if
		$report->{points}->{$player}->{post_test}->{improvement} > $score_spread;
}

my $median = (sort {$a<=>$b} grep {$_ != 0} values %$card)[ $participants/2 ];
my $max_points = max values %$card;

for my $player ( keys %members ) {
	$report->{exercise} = $exercise;
	if ( $report->{points}->{$player}->{post_test}->{attempted} ) {
		if ( $card->{$player} > $median ) {
			$report->{grade}->{$player} =  sprintf( "%.2f", 4 + 1 * ($card->{$player} - $median) / ($max_points - $median) );
		}
		elsif ( $card->{$player} <= $median ) {
			$report->{grade}->{$player} = sprintf( "%.2f", 3 + 1 * $card->{$player} / $median );
		}
		else {
			die "No card.player, no report.grade.player?\n";
		}
		$class_total->{grade} += $report->{grade}->{$player};
	}
	else {
		$report->{grade}->{$player} = 0;
	}
	Bless( $report->{points}->{$player} )->keys(
		[ qw/pre_test post_test/ ] );
}

print Dump $report, $report->{grade};

print "report: |+\n";

STDOUT->autoflush;
$^L='';

format STDOUT_TOP =
  Stages     Pre-test               Post-test
  Player     Attempted Correct   Targeted Attempted Correct Improvement   Grade
.

for my $member (sort keys %members) {

format STDOUT =
@<@<<<<<<<<<< @###      @##       @##       @<<<<<    @<<<<     @<<<<     @<<
{ "  ", $member
		, $report->{points}->{$member}->{pre_test}->{attempted}
		, $report->{points}->{$member}->{pre_test}->{correct}
		, $report->{points}->{$member}->{post_test}->{targeted}
		, $report->{points}->{$member}->{post_test}->{attempted}
		, $report->{points}->{$member}->{post_test}->{correct}
		, $report->{points}->{$member}->{post_test}->{improvement}
		, $report->{grade}->{$member}
    }
.

    write;
}

$^='TOTAL_TOP';
$~='TOTAL';
$^L="\f";

format TOTAL_TOP =
  Class Totals
             Pre-test               Post-test
             Attempted Correct   Targeted Attempted Correct Improvement   Grade
.

format TOTAL =
  Class Totals
             Pre-test               Post-test
             Attempted Correct   Targeted Attempted Correct Improvement   Grade
@<@<<<<<<<<<< @###      @####     @####     @<<<<     @<<<<    @<<<<   @<<<<
{ "", "",
	, $class_total->{pre_test}->{attempted} / $participants
	, $class_total->{pre_test}->{correct} / $participants
	, $class_total->{post_test}->{targeted} / $participants
	, $class_total->{post_test}->{attempted} / $participants
	, $class_total->{post_test}->{correct} / $participants
	, $class_total->{post_test}->{improvement} / $participants
	, $class_total->{grade} / $participants
    }
.
write;

print Dump $report->{grade};


=head1 NAME

grade_aca.pl - record results from aca DB

=head1 SYNOPSIS

perl script_files/grade_aca.pl -l GL00016 -x sports-test -b sports-base > ../001/GL00016/exam/g.yaml

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
