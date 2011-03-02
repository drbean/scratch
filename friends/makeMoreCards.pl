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
                        source =>  'oneA4fourpairs.tmpl' ,
                        delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $quiztmpl = Text::Template->new( type => 'file',
			source =>  '../tmpl/questionsB5.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
                my $cio = io "$ARGV[0]/compcomp$t$f.tex";
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
			source =>  'oneA4twogroups.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $quiztmpl = Text::Template->new( type => 'file',
            source =>  '../tmpl/namequestionsB5.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $fio = io "$ARGV[0]/jigsaw$t$f.tex";
		my $qio = io "$ARGV[0]/quiz$t$f.tex";
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
		$,="\n<li>";
		$hio->print("<h2>$form->{identifier}</h2><ol>", @htmlq);
	}
}
