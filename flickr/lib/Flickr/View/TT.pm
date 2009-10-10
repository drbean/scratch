package Flickr::View::TT;

use strict;
use base 'Catalyst::View::TT';

# __PACKAGE__->config(TEMPLATE_EXTENSION => '.tt');
__PACKAGE__->config(TEMPLATE_EXTENSION => '.tt2',
	               # Set the location for TT files
               INCLUDE_PATH => [
                       Flickr->path_to( 'root/src' ),
                   ],
		TIMER => 0,
		WRAPPER => 'wrapper.tt2',
           );


=head1 NAME

Flickr::View::TT - TT View for Flickr

=head1 DESCRIPTION

TT View for Flickr. 

=head1 AUTHOR

=head1 SEE ALSO

L<Flickr>

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
