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
    $c->stash->{template} = 'index.tt2'
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
	$c->stash->{template} = 'seller_signup.tt2';
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
			password => int(rand(1000000)),
			deleted => 0,
		});
	$c->stash->{template} = 'seller_list.tt2';
	$c->detach( 'seller_list' );
}


=head2 seller_list

Books available

=cut 

sub seller_list : Local
{
	my ($self, $c) = @_;
	my $sellers = [$c->model('DB::Seller')->search({deleted => 0})];
	$c->stash->{sellers} = $sellers;
	$c->stash->{template} = 'seller_list.tt2';
}


=head2 seller_signoff

Seller deleting entry

=cut 

sub seller_signoff : Local
{
	my ($self, $c, $id) = @_;
	my $seller = $c->model('DB::Seller')->find({id => $id});
	$c->stash->{email} = {
		to => $seller->email,
		from => "greg\@nuu.edu.tw",
		subject => "Deletion from Just Right Sellers List",
		body => "Somebody is deleting a book from the Just Right Sellers List.
If that person is you, that is, if you are no longer selling your book,
and you want it to be removed from the web page, please confirm the request
at " . $c->uri_for('/seller_delete/') . $seller->password,
		};
	$c->forward( $c->view('Email') );
	if ( scalar( @{ $c->error } ) ) {
		$c->error(0); # Reset the error condition if you need to
		$c->response->body('The entry cannot be deleted. Are you sure you used a valid email address when you entered the book in the list. Contact Dr Bean at drbean(a)freeshell.org.');
	} else {
	$c->stash->{status_msg} = 'The book is being deleted. But before it is deleted, we will<br>
check that you really want to remove it from the site.<br>
<br>
To do this, we are sending an email to the email address you<br>
used when you put the book on the site. When you get the email,<br>
click on the URL in it to confirm you really want the entry deleted.';
	$c->forward('index');
	}
}


=head2 seller_delete

Entry deleted

=cut 

sub seller_delete : Local
{
	my ($self, $c, $passwd) = @_;
	my @deleted = $c->model('DB::Seller')->search({password => $passwd});
	die "Two entries with same $passwd password" if @deleted > 1;
	$deleted[0]->update({deleted => 1}) if @deleted;
	$c->stash->{status_msg} = "Entry deleted.";
	$c->response->redirect($c->uri_for('/',
		{status_msg => "Entry deleted."}));
}


=head2 buyer_signup

Serve buyer form

=cut 

sub buyer_signup : Local
{
	my ($self, $c) = @_;
	$c->stash->{template} = 'buyer_signup.tt2';
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
			password => int(rand(1000000)),
			deleted => 0,
		});
	$c->detach( 'buyer_list' );
}


=head2 buyer_list

Books sought

=cut 

sub buyer_list : Local
{
	my ($self, $c) = @_;
	my $buyers = [$c->model('DB::Buyer')->search({deleted => 0})];
	$c->stash->{buyers} = $buyers;
	$c->stash->{template} = 'buyer_list.tt2';
}


=head2 buyer_signoff

Buyer deleting entry

=cut 

sub buyer_signoff : Local
{
	my ($self, $c, $id) = @_;
	my $buyer = $c->model('DB::Buyer')->find({id => $id});
	$c->stash->{email} = {
		to => $buyer->email,
		from => "greg\@nuu.edu.tw",
		subject => "Deletion from Just Right Buyers List",
		body => "Somebody is leaving the Just Right buyers List.
If that person is you, that is, if you are no longer looking for a book,
and you want your info to be removed from the web page, please confirm the
request at " . $c->uri_for('/buyer_delete/') . $buyer->password,
		};
	$c->forward( $c->view('Email') );
	if ( scalar( @{ $c->error } ) ) {
		$c->error(0); # Reset the error condition if you need to
		$c->response->body('The entry cannot be deleted. Are you sure you used a valid email address when you entered your information in the list. Contact Dr Bean at drbean(a)freeshell.org.');
	} else {
	$c->stash->{status_msg} = 'You are being deleted. But before you are deleted, we will check that you really want to remove your information from the site. To do this, we are sending an email to the email address you used when you put your information on the site. When you get the email, click on the URL in it to confirm you really want your entry deleted.';
	$c->forward('index');
	}
}


=head2 buyer_delete

Entry deleted

=cut 

sub buyer_delete : Local
{
	my ($self, $c, $passwd) = @_;
	my @deleted = $c->model('DB::Buyer')->search({password => $passwd});
	die "Two entries with same $passwd password" if @deleted > 1;
	$deleted[0]->update({deleted => 1}) if @deleted;
	$c->stash->{status_msg} = "Entry deleted.";
	$c->response->redirect($c->uri_for('/',
		{status_msg => "Entry deleted."}));
}

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
