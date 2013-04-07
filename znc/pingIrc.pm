package pingIrc;

# Created: 04/07/2013 01:33:27 PM
# Last Edit: 2013 Apr 07, 03:18:41 PM
# $Id$

=head1 NAME

pingIrc.pm - Send PING to IRC

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

use strict;
use warnings;

=head1 DESCRIPTION

Znc replies to PING from IRC, but doesn't send PING to IRC. CLI sends PING once every minute. May be this is the reason for Ping Timeout on otfc, perl networks when client is offline.

=cut

use base 'ZNC::Module';

sub module_types { $ZNC::CModInfo::UserModule }

my $server = "203.64.184.141";

sub OnModCommand {
    my $self = shift;
    my $cmd = shift;
    $self->CreateTimer( task => \&ping, interval => 10, cycles => 2, context => $server,
	    description => "Ping Irc");
}

sub ping {
    my $self = shift;
    my %arg = @_;
    $self->PutIRC( "PING $arg{context}" );
}

sub description { "Sends PING to IRC" }

1;


=head1 AUTHOR

Dr Bean C<< <drbean at cpan, then a dot, (.), and org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-/home/drbean/znc/pingpingIrc.pm at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=/home/drbean/znc/pingpingIrc.pm>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

	perldoc /home/drbean/znc/pingIrc.pm

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist//home/drbean/znc/pingpingIrc.pm>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d//home/drbean/znc/pingpingIrc.pm>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=/home/drbean/znc/pingpingIrc.pm>

=item * Search CPAN

L<http://search.cpan.org/dist//home/drbean/znc/pingpingIrc.pm>

=back

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2013 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of /home/drbean/znc/pingIrc.pm

# vim: set ts=8 sts=4 sw=4 noet:

