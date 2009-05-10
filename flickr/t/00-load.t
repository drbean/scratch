#!perl -T

use Test::More tests => 1;

BEGIN {
	use_ok( 'Flickr' );
}

diag( "Testing Flickr $Flickr::VERSION, Perl $], $^X" );
