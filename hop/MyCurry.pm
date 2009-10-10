package MyCurry;

use strict;
use warnings;

use base qw/Exporter/;

our @EXPORT = qw/currymap currygrep curry imap fold foldn/;

sub currymap {
	my $f = shift;
	sub {
		my @list;
		push @list, $f->($_) for @_;
		return @list
	}
}

sub currygrep {
	my $f = shift;
	sub {
		my @list;
		for ( @_ ) { push @list, $_ if $f->($_); }
		return @list;
	}
}

sub curry {
	my $f = shift;
	sub {
		my $first = shift;
		my $r = sub { $f->($first, @_) };
		# return @_? $r->(@_): $r;
	}
}

sub imap (&$) {
	my $f = shift;
	my $iterator = shift;
	my $n = 0;
	sub {
		my $next = $iterator->();
		return $f->($next) if defined $next;
		return;
	}
}

sub foldn {
	my $f = shift;
	my $fold;
	$fold = sub {
		my $x = shift;
		sub {
			return $x unless @_;
			my $first = shift;
			$fold->($f->($x, $first))->(@_);
		}
	}
}

sub fold {
	my $f = shift;
	sub {
		my $x = shift;
		sub {
			my $r = $x;
			while ( @_ ) {
				$r = $f->($r, shift());
			}
			return $r;
		}
	}
}

1;
