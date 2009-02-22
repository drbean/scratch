package Shop::Controller::Root;

use strict;
use warnings;
use parent 'Catalyst::Controller';

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config->{namespace} = '';

=head1 NAME

Shop::Controller::Root - Root Controller for Shop

=head1 DESCRIPTION

[enter your description here]

=head1 METHODS

=cut

=head2 index

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    # Hello World
    # $c->response->body( $c->welcome_message );
}

sub default :Path {
    my ( $self, $c ) = @_;
    $c->response->body( 'Page not found' );
    $c->response->status(404);
    
}

=head2 end

Attempt to render a view, if needed.

=cut 

sub end : ActionClass('RenderView') {}


=head2 seller_signup

Serve seller form

=cut 

sub seller_signup : Local
{
	my ($self, $c) = @_;
}


=head2 seller_create

Create db entry

=cut 

sub seller_create : Local
{
	my ($self, $c) = @_;
	my $seller = $c->request->params;
	$c->model('DB::Seller')->update_or_create({
			email => $seller->{email},
			contact => $seller->{contact},
			condition => $seller->{condition},
			booklet => $seller->{booklet},
			cd => $seller->{cd},
			price => $seller->{price},
			password => $seller->{price},
		});
	$c->detach( 'seller_list' );
}


=head2 seller_list

Books available

=cut 

sub seller_list : Local
{
	my ($self, $c) = @_;
	my $sellers = [$c->model('DB::Seller')->all];
	$c->stash->{sellers} = $sellers;
}


=head2 buyer_signup

Serve buyer form

=cut 

sub buyer_signup : Local
{
	my ($self, $c) = @_;
}


=head2 buyer_create

Create db entry

=cut 

sub buyer_create : Local
{
	my ($self, $c) = @_;
	my $buyer = $c->request->params;
	$c->model('DB::Buyer')->update_or_create({
			email => $buyer->{email},
			contact => $buyer->{contact},
			condition => $buyer->{condition},
			accessories => $buyer->{accessories},
			price => $buyer->{price},
			password => $buyer->{price},
		});
	$c->detach( 'buyer_list' );
}


=head2 buyer_list

Books sought

=cut 

sub buyer_list : Local
{
	my ($self, $c) = @_;
	my $buyers = [$c->model('DB::Buyer')->all];
	$c->stash->{buyers} = $buyers;
}

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
