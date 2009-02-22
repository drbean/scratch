package Shop::View::TT;

use strict;
use base 'Catalyst::View::TT';

# __PACKAGE__->config(TEMPLATE_EXTENSION => '.tt');
__PACKAGE__->config(TEMPLATE_EXTENSION => '.tt2',
	               # Set the location for TT files
               INCLUDE_PATH => [
                       Shop->path_to( 'root/src' ),
                   ],
		TIMER => 0,
		WRAPPER => 'wrapper.tt2',
           );


=head1 NAME

Shop::View::TT - TT View for Shop

=head1 DESCRIPTION

TT View for Shop. 

=head1 AUTHOR

=head1 SEE ALSO

L<Shop>

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
