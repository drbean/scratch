#!/usr/bin/perl

use strict;
use warnings;

my $fact = sub {
       my ( $f1 ) = @_;
       sub {
           my ( $n ) = @_;
           $n < 2 ? 1 : $n * $f1->( $f1 )->( $n - 1 );
       }
   }->( sub {
       my ( $f2 ) = @_;
       sub {
           my ( $n ) = @_;
           $n < 2 ? 1 : $n * $f2->( $f2 )->( $n - 1 );
       }
   } );

print $fact->(10);

sleep;
