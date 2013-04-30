#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;
use Pod::Usage;
use FindBin;
use lib "$FindBin::Bin/../lib";

my $help = 0;
my $area = '.';
my ( $genre, $story, );
 
GetOptions(
    'genre|l=s'      => \$genre,
    'area|l=s'  => \$area,
    'story|l=s'  => \$story,
);

use Net::FTP;
my $ftp = Net::FTP->new('web.nuu.edu.tw') or
			die "web.nuu.edu.tw? $@";
$ftp->login('greg', '6y6t6y6t') or die "web.nuu.edu.tw login? $@";
$ftp->cwd("public_html/$genre/$area") or die
	"web.nuu.edu.tw/~greg/public_html? $@";
$ftp->binary;

# my @targets = ( 'A' .. 'D' );
my @targets = ( "all");
my $n = 0;
my $order = { map { $_ => $n++ } @targets };
my @voices = (
			"voice_nitech_us_clb_arctic_hts",
			"voice_nitech_us_slt_arctic_hts",
			"voice_nitech_us_rms_arctic_hts",
			"voice_nitech_us_awb_arctic_hts",
		);

use YAML qw/LoadFile DumpFile/;
my ($texts, $questions) = LoadFile "$area/dic.yaml";
my $i = 0;
for my $text ( @$texts ) {
	next unless $text->[0] eq $story;
	my $targetindex = $i++ % @targets;
	my $target = $targets[ $targetindex ];
	my $voice = $voices[ $targetindex ];
	my $content = $text->[4];
	my $local = "/home/drbean/soundfiles/$genre/$area/${story}_$target.mp3";
	system( "echo \"$content\" |
		text2wave -eval \"($voice)\" -otype wav -o /tmp/$genre.wav"
		) == 0 or die "text2wave? $@,";
	system(	"lame -h -V 0 /tmp/$genre.wav $local" ) == 0 or
		die "lame? $@,";
	$ftp->put($local) or die "put $_->[2].mp3 on web.nuu.edu.tw? $@";
}
