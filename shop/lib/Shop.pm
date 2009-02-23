package Shop;

# $Id: /loc/Shop/trunk/lib/Shop.pm 529 2008-09-21T08:33:21.835922Z drbean  $

use strict;
use warnings;

use Catalyst::Runtime '5.70';

# Set flags and add plugins for the application
#
#         -Debug: activates the debug mode for very useful log messages
#   ConfigLoader: will load the configuration from a Config::General file in the
#                 application's home directory
# Static::Simple: will serve static files from the application's root 
#                 directory

use Catalyst qw/
    -Debug 
    ConfigLoader 
    Static::Simple
    
    StackTrace

    Authentication
    Authorization::Roles
            
    Session
    Session::Store::FastMmap
    Session::State::Cookie
    /;

    # Authorization::ACL

#BEGIN { my @plugins = qw/ConfigLoader Static::Simple 
#                   Authentication
#                   Authentication::Credential::Password
#		   Authorization::Roles
#                   Session
#                   Session::State::Cookie
#			/;
#		   # Authentication::Store::DBIC
#	if ( $^O eq 'linux' ) { push @plugins, 'Session::Store::FastMmap'; }
#	# else { push @plugins, 'Session::Store::DBIC'; }
#	require Catalyst; Catalyst->import(@plugins);
#}

our $VERSION = '0.04';

# Configure the application. 
#
# Note that settings in Shop.conf (or other external
# configuration file that you set up manually) take precedence
# over this when using ConfigLoader. Thus configuration
# details given here can function as a default configuration,
# with a external configuration file acting as an override for
# local deployment.

# __PACKAGE__->config( name => 'Shop' );

   __PACKAGE__->config(
	name => 'Shop' ,
	default_view => 'TT',
	'View::Email' => {
	   # Where to look in the stash for the email information.
	   # 'email' is the default, so you don't have to specify it.
	   stash_key => 'email',
	   # Define the defaults for the mail
	   default => {
	       # Defines the default content type (mime type). Mandatory
	       content_type => 'text/plain',
	       # Defines the default charset for every MIME part with the
	       # content type text.
	       # According to RFC2049 a MIME part without a charset should
	       # be treated as US-ASCII by the mail client.
	       # If the charset is not set it won't be set for all MIME parts
	       # without an overridden one.
	       # Default: none
	       charset => 'utf-8'
	   },
	   # Setup how to send the email
	   # all those options are passed directly to Email::Send
	   sender => {
	       mailer => 'SMTP',
	       # mailer_args is passed directly into Email::Send
	       mailer_args => {
		   Host     => 'email.nuu.edu.tw', # defaults to localhost
		   username => 'greg',
		   password => '1949',
	      }
	   }
	}
   );


# Start the application
__PACKAGE__->setup;

## Authorization::ACL Rules
#__PACKAGE__->deny_access_unless(
#        "/books/form_create",
#        [qw/admin/],
#    );
#__PACKAGE__->deny_access_unless(
#        "/books/form_create_do",
#        [qw/admin/],
#    );
#__PACKAGE__->deny_access_unless(
#        "/books/delete",
#        [qw/user admin/],
#    );




=head1 NAME

Shop - Catalyst based application

=head1 SYNOPSIS

    script/Shop_server.pl

=head1 DESCRIPTION

[enter your description here]

=head1 SEE ALSO

L<Shop::Controller::Root>, L<Catalyst>

=head1 AUTHOR

Catalyst developer

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
