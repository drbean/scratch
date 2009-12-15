#!/usr/bin/perl 

use strict;
use warnings;

use Grades;

my $script = Grades::Script->new_with_options;
my $exercise = $script->exercise;
my $two = $script->two;
my $one = $script->one;

use Cwd;
use File::Basename;
use YAML qw/LoadFile DumpFile/;

my $cwd = getcwd;
my $id = basename($cwd);

my $l = League->new( id => $cwd );
my $g = Grades->new( league => $l );
my $members = $league->members;
my %m = map { $_->{id} => $_ } @$members;

my $standings = LoadFile '/var/www/cgi-bin/target/standings.yaml';

my %p = map { $_ => $standings->{$id}->{$_}->{$exercise} } keys %m;
$p{one} = $one;
$p{two} = $two;
$p{exercise} = $exercise;

my %g = map { $_ => $p{$_} >= $two? 2: $p{$_} > $one? 1: 0 } keys %m;
