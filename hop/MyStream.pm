package MyStream;

# use strict;
# use warnings;

use base qw/Exporter/;

use Stream qw/transform node/;

our @EXPORT = qw/$hamming $powers upterator upto upfrom promise uptoerator
	cons head tail show index maps filter iterate merge scale xormerge
	literal union unionordered concat star plus charclass balanced
	streamlist combine currymap currygrep/;

sub cons {
	my ( $list, $last ) = @_;
	return [$list, $last];
}

sub head { $_[0]->[0]; }

#sub tail {
#	my $list = shift;
#	my $tail = $list->[1];
#	if (UNIVERSAL::isa($tail, 'CODE') ) { return $tail->(); }
#	# if (ref($tail) eq 'CODE') { return $tail->(); }
#	else { return $tail; }
#}

sub tail {
  my ($s) = @_;
  if (is_promise($s->[1])) {
    return $s->[1]->();
  }
  $s->[1];
}

sub is_promise {
  UNIVERSAL::isa($_[0], 'CODE');
}

sub promise (&) { $_[0] }

my $ones;
$ones = cons( 1, promise { $ones } );

sub upto {
	my ($m, $n) = @_;
	return if $m > $n;
	cons( $m, promise {warn "\$m,\$n: $m,$n\n"; return (upto($m+1, $n) ) } );
};

sub upfrom {
	my ($m) = @_;
	cons( $m, promise {return (upfrom($m+1) ) } );
};

sub show {
  my ($s, $n) = @_;
  while ($s and (not defined $n or $n-- > 0)) {
    print head($s), $";
    $s = tail($s);
  }
  print $/;
}

sub index {
	my ($s, $n) = @_;
	while ( $n-- > 0 )
	{
		$s = tail $s;
	}
	head $s;
}

sub maps (&$) {
	my $fn = shift;
	my $stream = shift;
	return unless $stream;
	cons( $fn->(head $stream), promise { maps($fn, tail($stream)) } );
}

# sub scale { maps { $_[0] * shift } shift; }
#sub scale {
#	my $c = shift;
#	maps { $_[0] * $c } shift;
#}
#sub scale {
#	my ($s, $c) = @_;
#	maps { $_[0] * $c } $s;
#}
sub scale {
	my $c = shift;
	sub {
		my $s = shift;
		maps { $_[0] * $c } $s;
	}
}

sub drop {
  my $h = head($_[0]);
  $_[0] = tail($_[0]);
  return $h;
}

sub filter (&$) {
	my $fn = shift;
	my $stream = shift;
	# drop($stream) until ( (not $stream) or $fn->(head $stream));
	$stream = tail $stream until ( not $stream or $fn->(head $stream));
	return if not $stream;
	cons (head ($stream), promise { filter( $fn, tail($stream) ) } );
}

#sub iterate (&$) {
#	my $f = shift;
#	my $x = shift;
#	# cons( $x, promise { iterate( $f, $f->($x) ) } );
#	my $s;
#	$s = cons( $x, promise { &maps( $f, $s ) } );
#}
sub iterate {
	my $f = shift;
	return sub {
		my $x = shift;
		my $s;
		$s = cons($x, promise{ &maps( $f, $s) } );
	}
}

sub merge {
	my $left = shift;
	my $right = shift;
	if ( head($left) < head($right) ) {
		cons ( head($left), promise { merge( tail($left), $right ) } );
	}
	elsif ( head($left) == head($right) ) {
		cons(head($left), promise { merge(tail($left), tail($right)) } );
	}
	else {
		cons(head($right), promise { merge($left, tail($right)) } );
	}
}

sub xormerge {
	my $left = shift;
	my $right = shift;
	if ( head($left) == head $right) {
		until ( head($left) != head $right) {
			$left = tail $left;
			$right = tail $right;
		}
	}
	if ( head($left) < head $right) {
		cons ( head($left), promise { xormerge( tail($left), $right) });
	}
	if ( head($left) > head $right) {
		cons ( head($right), promise { xormerge( $left, tail $right) });
	}
}

my $hamming;
$hamming = cons( 1, promise {
		merge ( scale( $hamming, 2 ),
		merge( scale( $hamming, 3 ),
			scale( $hamming, 5 )) ) } );

my $powers;
$powers = sub {
	my $x = shift;
	cons ( $x, promise { $powers->($x*2) } );
};

sub literal {
	my $regex = shift;
	cons( $regex, undef );
}

sub union {
	my ($h, @s) = grep $_, @_;
	return unless $h;
	return $h unless @s;
	cons( head($h), promise { union( @s, tail $h ) } );
}

sub unionordered {
	my @s = grep $_, @_;
	return $s[0] if @s == 1;
	my $shortest=0;
	for my $index ( 1..$#s )
	{
		$shortest = $index if length (head $s[$index]) <
					length (head $s[$shortest]);
	}
	cons( head($s[$shortest]), promise { unionordered(
				map $_==$shortest? tail($s[$_]): $s[$_], 
			0..$#s ) } );
}

sub concat {
	my ($s, $t) = @_;
	return unless $s && $t;
	return $s unless $t;
	return $t unless $s;
	my ($shead, $thead) = (head($s), head($t));
	cons( $shead . $thead, promise { unionordered(
				( maps { $_[0] . $thead } tail $s ),
				( maps { $shead . $_[0] } tail $t ),
				concat( tail($s), tail($t) ) ) } );
}

sub star {
	my $s = shift;
	my %r;
	$r = cons( "", promise { concat( $s , $r ) } );
}

sub charclass {
	my @s = split //, $_[0];
	my @lits = map { literal($_) } @s;
	union ( @lits );
}

sub plus {
	my $s = shift;
	concat( $s, star($s) );
}

sub balanced {
	my $regex = shift;
	my $balanced;
	$balanced =
	cons( '', promise { concat( $balanced, union( $regex,
					# maps { "($_[0])" } $balanced
					concat( literal('('),
					concat( $balanced, literal(')') ))
				) ) } );
}

sub streamlist {
	my $stream = pop;
	while ( @_ ) {
		$stream = cons ( pop, $stream );
	}
	return $stream;
}

sub combine {
	my $op = shift;
	my $curried;
	$curried = sub { 
		my @streams = @_;
		my @heads = map { head($_) } @streams;
		my @tails = map { tail($_) } @streams;
		cons( $op->(@heads), promise {
			$curried->(@tails)});
	}
}

1;

__DATA__

sub upterator {
	my ($m, $n) = @_;
	my $i = $m;
	return sub {
		return if $i == $n;
		$i++;
		return $i;
	}
}

# my $x;
# print $x while $x = $i->();

sub uptoerator {
	my ($m, $n) = @_;
	return undef if $m > $n;
	cons $m, uptoerator( $m+1, $n);
}
