package Function;

use warnings;
use strict;

=head1 NAME

Function - The great new Function!

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

sub it { my @list = @_; return sub { shift @list } };
my $it = it(0..20);
my $mapit = imap sub { 2 * shift }, $it;

=head1 EXPORT

A list of functions that can be exported.  imap igrep concat, by default.

=cut

BEGIN {
	use Exporter ();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);
               $VERSION     = 1.00;
               @ISA         = qw(Exporter);
               @EXPORT      = qw(imap igrep concat);
               %EXPORT_TAGS = ( );
               @EXPORT_OK   = qw();
           }

=head1 FUNCTIONS

=head2 imap

sub it { my @list = @_; return sub { shift @list } };
my $it = it(0..20,3);
my $mapit = imap sub { 2 * shift }, $it;

=cut

sub imap (&$) {
	my ( $sub, $it ) = @_;
	return sub {
		my $n = $it->();
		return unless defined $n;
		return $sub->($n);
		};
}

=head2 igrep

my $it1 = it(0...40);
my $mapit1 = imap sub { 2 * shift }, $it1;
my $grepit = grepit sub { (shift)%3 == 0 }, $mapit1;

=cut

sub igrep {
	my ( $sub, $it ) = @_;
	return sub {
		while ( defined( my $n = $it->() ) ) { return $n if $sub->($n) }
		return;
	}
}

=head2 concat

my $concat = concat $mapit, $grepit;

=cut

sub concat {
	my @its = @_;
	return sub { 
		while ( @its )
		{
			my $next = $its[0]->();
			return $next if defined $next;
			shift @its;
		}
		return;
	}
}

=head1 AUTHOR

Dr Bean, C<< <drbean  at  (an 'at' sign) cpan . (a '.' dot) org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-function at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Function>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Function


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Function>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Function>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Function>

=item * Search CPAN

L<http://search.cpan.org/dist/Function/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT & LICENSE

Copyright 2008 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut

1; # End of Function
