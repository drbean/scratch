#!/usr/bin/perl

use strict;
use warnings;

use Getopt::Long;
use Pod::Usage;
use FindBin;
use lib "$FindBin::Bin/../lib";

my $help = 0;
my ( $genre, $area, $topic, $form );
 
GetOptions(
    'genre|l=s'      => \$genre,
    'area|l=s'  => \$area,
    'topic|l=s'  => \$topic,
    'form|l=s'  => \$form,
);

use Net::FTP;
my $ftp = Net::FTP->new('web.nuu.edu.tw') or
			die "web.nuu.edu.tw? $@";
$ftp->login('greg', '1514') or die "web.nuu.edu.tw login? $@";
$ftp->cwd("public_html/$genre") or die
	"web.nuu.edu.tw/~greg/public_html? $@";
$ftp->binary;

use YAML qw/LoadFile DumpFile/;
my ($texts) = LoadFile "$area/cards.yaml";
my @soundfiles;
push @soundfiles, (
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
	for ( @soundfiles ) {
		# my ( $text, $voice, $local ) = @$_;
		my $text = $texts->{$topic}->{compcomp}->{$form}->{quiz}->[$_->[0]]->{question};
		my $voice = $_->[1];
		my $local = "/home/drbean/soundfiles/$genre/${topic}_${form}_$_->[2].mp3";
		system( "echo \"$text\" |
			text2wave -eval \"($voice)\" -otype wav -o /tmp/$genre.wav"
			) == 0 or die "text2wave? $@,";
		system(	"lame -h -V 0 /tmp/$genre.wav $local" ) == 0 or
			die "lame? $@,";
		$ftp->put($local) or die "put $_->[2].mp3 on web.nuu.edu.tw? $@";
	}
}

