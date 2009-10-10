#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Shop' );
}

diag( "Testing Shop $Shop::VERSION, Perl $], $^X" );
