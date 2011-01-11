#!/usr/bin/perl 

# Created: 西元2010年12月18日 10時28分17秒
# Last Edit: 2011  1月 11, 13時36分46秒
# $Id$

=head1 NAME

comp.pl - Generate, upload soundfiles for CompComp listening questions

=cut

use strict;
use warnings;

use Getopt::Long;
use Pod::Usage;
use FindBin;
use lib "$FindBin::Bin/../lib";

=head1 SYNOPSIS

perl ../sound/comp.pl -g business -a companies -t ikea -f 0

=cut

my $help = 0;
my ( $genre, $area, $topic, $form );
 
GetOptions(
    'genre|l=s'      => \$genre,
    'area|l=s'  => \$area,
    'topic|l=s'  => \$topic,
    'form|l=s'  => \$form,
);

=head1 DESCRIPTION

Uses Festival to generate readings of questions in cards.yaml compcomp quiz. Getopt::Long rather than Grades::Script for args.

=cut

use Net::FTP;
my $ftp = Net::FTP->new('web.nuu.edu.tw') or
			die "web.nuu.edu.tw? $@";
$ftp->login('greg', '1514') or die "web.nuu.edu.tw login? $@";
$ftp->cwd("public_html/$genre") or die
	"web.nuu.edu.tw/~greg/public_html? $@";
$ftp->binary;

use YAML qw/LoadFile DumpFile/;
my ($texts) = LoadFile "$area/cards.yaml";
my @questions;
push @questions, (
			 [ 0, "voice_nitech_us_rms_arctic_hts", "1" ],
			 [ 1, "voice_nitech_us_awb_arctic_hts", "2" ],
			 [ 2, "voice_nitech_us_clb_arctic_hts", "3" ],
			 [ 3, "voice_nitech_us_slt_arctic_hts", "4" ],
			 [ 4, "voice_nitech_us_rms_arctic_hts", "5" ],
			 [ 5, "voice_nitech_us_awb_arctic_hts", "6" ],
			 [ 6, "voice_nitech_us_clb_arctic_hts", "7" ],
			 [ 7, "voice_nitech_us_slt_arctic_hts", "8" ],
			 [ 8, "voice_nitech_us_rms_arctic_hts", "9" ],
			 [ 9, "voice_nitech_us_awb_arctic_hts", "10" ],
			 # [ 10, "voice_nitech_us_awb_arctic_hts", "11" ],
	);

for my $topicform (
	[ $topic, $form ],
) {

	my $topic = $topicform->[0];
	my $form = $topicform->[1];
	for my $q ( @questions ) {
		my ( $text, $voice, $id ) = @$q;
		my $text = $texts->{$topic}->{compcomp}->{$form}->{quiz}->[$text]->{question};
		my $local = "/home/drbean/soundfiles/$genre/${topic}_${form}_$id.mp3";
		system( "echo \"$text\" |
			text2wave -eval \"($voice)\" -otype wav -o /tmp/$genre.wav"
			) == 0 or die "text2wave? $@,";
		system(	"lame -h -V 0 /tmp/$genre.wav $local" ) == 0 or
			die "lame? $@,";
		$ftp->put($local) or die "put $_->[2].mp3 on web.nuu.edu.tw? $@";
	}
}



=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 COPYRIGHT & LICENSE

Copyright 2010 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

# End of comp.pl

# vim: set ts=8 sts=4 sw=4 noet:
