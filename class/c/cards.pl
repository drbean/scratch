#!/usr/bin/perl

use strict;
use warnings;

use YAML qw/LoadFile DumpFile/;
use IO::All;
use Text::Template;

my $cards = LoadFile "$ARGV[0]/cards.yaml";

for my $t ( keys %$cards ) {
	my $topic = $cards->{$t};
	next unless ref $topic eq 'HASH';
	my $compcomp = $topic->{compcomp};
	for my $f ( keys %$compcomp ) {
		my $form = $compcomp->{$f};
                my $pairtmpl = Text::Template->new( type => 'file',
                        source =>  '/home/drbean/class/tmpl/compcompA4.tmpl' ,
                        delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $quiztmpl = Text::Template->new( type => 'file',
			source =>  '/home/drbean/class/tmpl/namequestionsB7.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
                my $cio = io "$ARGV[0]/compcomp_$t" . "_$f.tex";
		my $qio = io "$ARGV[0]/quiz$t$f.tex";
		my $hio = io "$ARGV[0]/quiz$t$f.html";
		my $n = 1;
		my $questions = $form->{quiz};
		for my $qa ( @$questions ) {
			$form->{ "q$n" } = $qa->{question};
			$n++;
		}
                $cio->print( $pairtmpl->fill_in( hash=> $form ) );
		$qio->print( $quiztmpl->fill_in( hash=> $form ) );
		my @htmlq = map { $form->{"q$_"} } 1 .. $n-1;
		$,="\n<li>";
		$hio->print("<h2>$form->{identifier}</h2><ol>", @htmlq);
	}
	my $jigsaw = $topic->{jigsaw};
	for my $f ( keys %$jigsaw ) {
		my $form = $jigsaw->{$f};
		my $fourtmpl = Text::Template->new( type => 'file',
			source =>  '/home/drbean/class/tmpl/jigsaw.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $quiztmpl = Text::Template->new( type => 'file',
            source =>  '/home/drbean/class/tmpl/namequestionsA6.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $fio = io "$ARGV[0]/jigsaw_$t" . "_$f.tex";
		my $qio = io "$ARGV[0]/quiz_$t" . "_$f.tex";
		my $hio = io "$ARGV[0]/quiz$t$f.html";
		my $n = 1;
		my $questions = $form->{quiz};
		for my $qa ( @$questions ) {
			$form->{ "q$n" } = $qa->{question};
			$n++;
		}
		$fio->print( $fourtmpl->fill_in( hash=> $form ) );
		$qio->print( $quiztmpl->fill_in( hash=> $form ) );
		my @htmlq = map { $form->{"q$_"} } 1 .. $n-1;
		$,="\n<h1><li>";
		$hio->print("<h2>$form->{identifier}</h2><ol>", @htmlq);
	}
}
