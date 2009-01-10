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

sub fold {
	my $code = shift;
	my $fold;
	$fold = sub {
		my $x = shift;
		sub {
			return $x unless @_;
			my $first = shift;
			$fold->($code->($x, $first), @_);
		}
	}
}

sub foldn {
	my $code = shift;
	sub {
		my $x = shift;
		sub {
			return $x unless @_;
			while ( @_ ) {
				my $next = shift;
				print $next;
				$x = $code->($x, $next);
			}
			return $x;
		}
	}
}

1;
