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


=head2 shop

Which book are they dealing with?

=cut

sub shop :Local {
	my ($self, $c) = @_;
	my $book;
	$book  = defined $c->request->params->{book}?
		$c->request->params->{book}: $c->session->{book};
	$c->session->{book} = $book;
	$c->stash->{book} = $book;
	$c->stash->{template} = "shop.tt2";
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
	$c->stash->{book} = $c->session->{book};
	$c->stash->{template} = 'seller_signup.tt2';
}


=head2 seller_create

Create db entry

=cut 

sub seller_create : Local
{
	my ($self, $c) = @_;
	my $seller = $c->request->params;
	my $book = $c->session->{book};
	$c->model('DB::Seller')->update_or_create({
			book => $book,
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
	$c->stash->{book} = $c->session->{book};
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
		body =>
"Somebody is deleting a book from Dr Bean's Textbook Swap Shop.
If that person is you, that is, if you are no longer selling your book,
and you want your entry to be removed from the web page, please confirm
the request at " . $c->uri_for('/seller_delete/') . $seller->password,
		};
	$c->forward( $c->view('Email') );
	if ( scalar( @{ $c->error } ) ) {
		$c->error(0); # Reset the error condition if you need to
		$c->response->body(
'The entry cannot be deleted. Are you sure ', $seller->email . 
', the email address you used when you entered the book in the list is valid. Contact Dr Bean at drbean(a)freeshell.org.');
	} else {
	$c->stash->{status_msg} =
'The book is being deleted. But before it is deleted, we will<br>
check that you really want to remove it from the site.<br>
<br>
To do this, we are sending an email to the email address you<br>
used when you put the book on the site. When you get the email,<br>
click on the URL in it to confirm you really want the entry deleted.';
	$c->forward('shop');
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
	my $id = $deleted[0]->id;
	$c->stash->{status_msg} = "Book $id deleted.";
	$c->forward('shop');
}


=head2 buyer_signup

Serve buyer form

=cut 

sub buyer_signup : Local
{
	my ($self, $c) = @_;
	$c->stash->{book} = $c->session->{book};
	$c->stash->{template} = 'buyer_signup.tt2';
}


=head2 buyer_create

Create db entry

=cut 

sub buyer_create : Local
{
	my ($self, $c) = @_;
	my $buyer = $c->request->params;
	my $book = $c->session->{book};
	$c->model('DB::Buyer')->update_or_create({
			book => $book,
			email => $buyer->{email},
			contact => $buyer->{contact},
			condition => $buyer->{condition},
			booklet => $buyer->{booklet},
			cd => $buyer->{cd},
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
	$c->stash->{book} = $c->session->{book};
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
		body => "Somebody is leaving Dr Bean's Textbook Swap Shop.
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
	$c->forward('shop');
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
	$c->stash->{status_msg} = "Your entry has been deleted.";
	$c->forward('index');
}

=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
