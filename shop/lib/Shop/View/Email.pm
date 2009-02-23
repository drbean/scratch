package Shop::View::Email;

use strict;
use base 'Catalyst::View::Email';

__PACKAGE__->config(
    stash_key => 'email',
	#default => {
	#    # Defines the default content type (mime type). Mandatory
	#    content_type => 'text/plain',
	#    # Defines the default charset for every MIME part with the
	#    # content type text.
	#    # According to RFC2049 a MIME part without a charset should
	#    # be treated as US-ASCII by the mail client.
	#    # If the charset is not set it won't be set for all MIME parts
	#    # without an overridden one.
	#    # Default: none
	#    charset => 'utf-8'
	#},
	## Setup how to send the email
	## all those options are passed directly to Email::Send
	#sender => {
	#    mailer => 'SMTP',
	#    # mailer_args is passed directly into Email::Send
	#    mailer_args => {
	#       Host     => 'email.nuu.edu.tw', # defaults to localhost
	#       username => 'greg',
	#       password => '1949',
	#   }
	#}
);

=head1 NAME

Shop::View::Email - Email View for Shop

=head1 DESCRIPTION

View for sending email from Shop. 

=head1 AUTHOR

Dr Bean

=head1 SEE ALSO

L<Shop>

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
