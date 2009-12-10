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
	for my $f ( keys %$topic ) {
		my $form = $topic->{$f};
                my $pairtmpl = Text::Template->new( type => 'file',
                        source =>  'B5twopairs.tmpl' ,
                        delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $fourtmpl = Text::Template->new( type => 'file',
			source =>  'oneA4twogroups.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
		my $quiztmpl = Text::Template->new( type => 'file',
			source =>  'namequestionsB5.tmpl' ,
			delimiters => [ '<TMPL>', '</TMPL>' ]);
                my $cio = io "$ARGV[0]/pair$t$f.tex";
		my $fio = io "$ARGV[0]/four$t$f.tex";
		my $qio = io "$ARGV[0]/quiz$t$f.tex";
		my $hio = io "$ARGV[0]/quiz$t$f.html";
		my $n = 1;
		my $questions = $form->{quiz};
		for my $qa ( @$questions ) {
			$form->{ "q$n" } = $qa->{question};
			$n++;
		}
                $cio->print( $pairtmpl->fill_in( hash=> $form ) );
		$fio->print( $fourtmpl->fill_in( hash=> $form ) );
		$qio->print( $quiztmpl->fill_in( hash=> $form ) );
		my @htmlq = map { $form->{"q$_"} } 1 .. $n-1;
		$,="\n<li>";
		$hio->print("<h2>$cards->{identifier}</h2><ol>", @htmlq);
	}
}
