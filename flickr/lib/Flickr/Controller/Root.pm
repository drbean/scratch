package Flickr::Controller::Root;

use Moose;
BEGIN { extends 'Catalyst::Controller'; }

use Flickr::API;
use YAML qw/DumpFile/;

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config->{namespace} = '';

=head1 NAME

Flickr::Controller::Root - Root Controller for Flickr

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

=head2 find

Find a Flickr picture. Would be good to be able to hit database only once for all the pictures with one tag. Perhaps I should do that when building the exercise. Doing it JIT, need to instantiate API object each time.
	$c->stash->{url} = 'http://farm4.static.flickr.com/3515/3470432168_8e8509962d.jpg';

=cut
 
sub find : Local {
	my ($self, $c, $word) = @_;
	my $pics = $c->model('DB::Pic');
	$c->stash->{template} = 'list.tt2';
	$c->stash->{tag} = $word;
	my $total = 1;
	my @oldurls = $pics->search({ word => $word });
	unless ( @oldurls ) {
		my $api = Flickr::API->new({key =>
			'ea697995b421c0532215e4a2cbadbe1e',
			secret => 'ab2024b750a9d1f2' });
		my $r = $api->execute_method('flickr.photos.search',
			{ tags => $word, per_page => $total, api_key =>
				'ea697995b421c0532215e4a2cbadbe1e' });
		unless ( $r->{success} ) {
			$c->stash->{error_msg} = $r->{error_message};
			return;
		}
		DumpFile $word . 'info.yaml', $r;
		my @newurls;
		for my $n ( 0 .. $total-1 ) {
			my $photo = $r->{tree}->{children}->[1]->
				{children}->[2*$n+1]->{attributes};
			unless ( defined $photo->{title} ) {
				$c->stash->{error_msg} = "No picture";
				return;
			}
			my %row;
			$row{title} = $photo->{title};
			$row{id} = undef;
			$row{word} = $word;
			$row{url} = 'http://farm' . $photo->{farm} .
				'.static.flickr.com/'.  $photo->
				{server} .  '/'.  $photo->{id} . '_' .
				$photo->{secret} . '_t.jpg';
			push @newurls, \%row;
		}
		$pics->populate(\@newurls);
		$c->stash->{urls} = \@newurls;
	}
	$c->stash->{urls} = \@oldurls;
	$c->stash->{urls} ||= [];
}


=head2 tagtitle

Find a Flickr picture by tag, but accept it only if tag is in title.
	$c->stash->{url} = 'http://farm4.static.flickr.com/3515/3470432168_8e8509962d.jpg';

=cut
 
sub tagtitle : Local {
	my ($self, $c, $word) = @_;
	my $pics = $c->model('DB::Pic');
	$c->stash->{template} = 'list.tt2';
	$c->stash->{tag} = $word;
	my $fetched = 500;
	my $needed = 100;
	my $page = 0;
	my @oldurls = $pics->search({ word => $word });
	unless ( @oldurls ) {
		my $api = Flickr::API->new({key =>
			'ea697995b421c0532215e4a2cbadbe1e',
			secret => 'ab2024b750a9d1f2' });
		my (@yaml, @newurls);
		while ( $needed >= 0 ) {
			my $r = $api->execute_method('flickr.photos.search',
				{ tags => $word, per_page => $fetched, page => $page++,
					api_key => 'ea697995b421c0532215e4a2cbadbe1e' });
			unless ( $r->{success} ) {
				$c->stash->{error_msg} = $r->{error_message};
				return;
			}
			for my $n ( 0 .. $fetched-1 ) {
				my $photo = $r->{tree}->{children}->[1]->
					{children}->[2*$n+1]->{attributes};
				next unless $photo->{title} =~ m/$word/i;
				push @yaml, $photo;
				my %row;
				$row{title} = $photo->{title};
				$row{id} = undef;
				$row{word} = $word;
				$row{url} = 'http://farm' . $photo->{farm} .
					'.static.flickr.com/'.  $photo->
					{server} .  '/'.  $photo->{id} . '_' .
					$photo->{secret} . '_t.jpg';
				push @newurls, \%row;
				$needed--;
			}
		}
		DumpFile $word . 'info.yaml', \@yaml;
		$pics->populate(\@newurls);
		$c->stash->{urls} = \@newurls;
	}
	$c->stash->{urls} = \@oldurls;
	$c->stash->{urls} ||= [];
}


=head2 info

Find a Flickr picture. Would be good to be able to hit database only once for all the pictures with one tag. Perhaps I should do that when building the exercise. Doing it JIT, need to instantiate API object each time.
	$c->stash->{url} = 'http://farm4.static.flickr.com/3515/3470432168_8e8509962d.jpg';

=cut
 
sub info : Local {
	my ($self, $c, $id) = @_;
	$c->stash->{template} = 'info.tt2';
	use LWP::Debug;
	my $api = Flickr::API->new({key =>
		'ea697995b421c0532215e4a2cbadbe1e',
		secret => 'ab2024b750a9d1f2' });
	my $r = $api->execute_method('flickr.photos.getInfo',
		{ photo_id => $id, api_key =>
			'ea697995b421c0532215e4a2cbadbe1e' });
	unless ( $r->{success} ) {
		$c->stash->{error_msg} = $r->{error_message};
	#	return;
	}
	else { $c->stash->{status_msg} = $r->{_msg}; }
	#DumpFile $id . 'info.yaml', $r;
	$c->stash->{response} = $r;
	$c->stash->{request} = $r->request;
}


=head2 echo

Echo from flickr server

=cut
 
sub echo : Local {
	my ($self, $c) = @_;
	$c->stash->{template} = 'info.tt2';
	use LWP::Debug;
	my $api = Flickr::API->new({key =>
		'ea697995b421c0532215e4a2cbadbe1e',
		secret => 'ab2024b750a9d1f2' });
	my $r = $api->execute_method('flickr.test.echo',
		{ api_key => 'ea697995b421c0532215e4a2cbadbe1e' });
	unless ( $r->{success} ) {
		$c->stash->{error_msg} = $r->{error_message};
	#	return;
	}
	else { $c->stash->{status_msg} = $r->{_msg}; }
	DumpFile '/tmp/echo.yaml', $r, $r->request;
	$c->stash->{response} = $r;
	$c->stash->{request} = $r->request;
}


=head2 test

Connect to a server

=cut
 
sub test : Local {
	my ($self, $c, $url) = @_;
	$c->stash->{template} = 'info.tt2';
	use LWP::Debug;
	my $ua = LWP::UserAgent->new;
	my $r = $ua->get("http://$url");
	unless ( $r->{is_success} ) {
		$c->stash->{error_msg} = $r->{error_message};
	#	return;
	}
	else { $c->stash->{status_msg} = $r->{_msg}; }
	DumpFile '/tmp/test.yaml', $r, $r->request;
	$c->stash->{response} = $r;
	$c->stash->{request} = $r->request;
}


=head1 AUTHOR

Dr Bean

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
