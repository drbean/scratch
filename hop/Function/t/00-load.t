#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Function' );
}

diag( "Testing Function $Function::VERSION, Perl $], $^X" );
