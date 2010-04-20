#!/usr/bin/perl

use strict;
use warnings;

my $genre = 'business';

use Net::FTP;
my $ftp = Net::FTP->new('web.nuu.edu.tw') or
			die "web.nuu.edu.tw? $@";
$ftp->login('greg', '1514') or die "web.nuu.edu.tw login? $@";
$ftp->cwd("public_html/$genre") or die
	"web.nuu.edu.tw/~greg/public_html? $@";
$ftp->binary;

use YAML qw/LoadFile DumpFile/;
my ($texts, $questions) = LoadFile 'dic.yaml';
my @soundfiles = ( [ 90,
						"voice_nitech_us_rms_arctic_hts",
	"compA" ], [ 91,
						"voice_nitech_us_awb_arctic_hts",
	"compB" ], [ 92,
						"voice_nitech_us_clb_arctic_hts",
	"compC" ], [ 93,
						"voice_nitech_us_slt_arctic_hts",
	"compD" ],
	);

for ( @soundfiles ) {
	# my ( $text, $voice, $local ) = @$_;
	my $text = $texts->[$_->[0]]->[4];
	my $voice = $_->[1];
	my $local = "/home/drbean/soundfiles/$genre/$_->[2].mp3";
	system( "echo \"$text\" |
		text2wave -eval \"($voice)\" -otype wav -o /tmp/$genre.wav"
		) == 0 or die "text2wave? $@,";
	system(	"lame -h -V 0 /tmp/$genre.wav $local" ) == 0 or
		die "lame? $@,";
	$ftp->put($local) or die "put $_->[2].mp3 on web.nuu.edu.tw? $@";
}

