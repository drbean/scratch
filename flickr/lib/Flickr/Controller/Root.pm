package Flickr::Controller::Root;

use Moose;
BEGIN { extends 'Catalyst::Controller'; }

use Flickr::API;
use Net::FTP;
use YAML qw/DumpFile/;
use IO::All;
use Lingua::Stem;
use Lingua::EN::Infinitive;
use Lingua::EN::Conjugate qw/past participle gerund/;

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
			my $stem = Lingua::Stem::stem($word);
			$row{word} = $stem;
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
    my ( $self, $c, $word ) = @_;
    my $pics = $c->model('DB::Pic');
    $c->stash->{template} = 'list.tt2';
    $c->stash->{tag}      = $word;
    my $fetched = 300;
    my $needed  = 20;
    my $page    = 1;
    my $lctag   = lc $word;
    my $canon   = Lingua::EN::Infinitive->new;
    my @infinit;
    for ( ( $canon->stem($lctag) )[ 0 .. 1 ] ) { push @infinit, $_ if $_ }
    my @conjugates = map { ( past($_), participle($_), gerund($_) ) } @infinit;
    my $tags        = join ',', $word, $lctag, @infinit, @conjugates;
    my $titlesearch = join '|', $word, $lctag, @infinit, @conjugates;
    my $titleregex  = qr/$titlesearch/i;
    my @oldurls = $pics->search( { word => $word } );

    unless (@oldurls) {
        my $api = Flickr::API->new(
            {
                key    => 'ea697995b421c0532215e4a2cbadbe1e',
                secret => 'ab2024b750a9d1f2'
            }
        );
        my $ftp = Net::FTP->new('web.nuu.edu.tw') or die "web.nuu? $@";
        $ftp->login( 'greg', '1514' ) or die "web.nuu.edu.tw login? $@";
        $ftp->cwd('public_html/pics') or die "web/~greg/pics? $@";
        my ( @yaml, @newurls );
        while ( $needed >= 0 ) {
            my $r = $api->execute_method(
                'flickr.photos.search',
                {
                    tags     => $tags,
                    per_page => $fetched,
                    page     => $page++,
                    api_key  => 'ea697995b421c0532215e4a2cbadbe1e'
                }
            );
            unless ( $r->{success} ) {
                $c->stash->{error_msg} = $r->{error_message};
                return;
            }
            for my $n ( 0 .. $fetched - 1 ) {
                my $photo =
                  $r->{tree}->{children}->[1]->{children}->[ 2 * $n + 1 ]
                  ->{attributes};
                next unless $photo->{title} =~ m/$titleregex/;
                my $owner = $photo->{owner};
                next if $pics->search( { owner => $owner } )->count;
                push @yaml, $photo;
                my %row;
                $row{title} = $photo->{title};
                $row{id}    = undef;
                $row{word}  = $word;
                $row{owner} = $owner;
                $row{url} = 'http://farm'
                  . $photo->{farm}
                  . '.static.flickr.com/'
                  . $photo->{server} . '/'
                  . $photo->{id} . '_'
                  . $photo->{secret}
                  . '_t.jpg';
		my $remote = "$photo->{id}_$photo->{secret}_t.jpg";
		my $local = "/tmp/pics/$remote";
                my $pic = $api->get( $row{url} );
                io( $local )->print( $pic->decoded_content ).
                $ftp->put( $local ) or die "$pic pic on web.nuu? $@";
                $row{url} = "http://web.nuu.edu.tw/~greg/pics/$remote";
                $pics->update_or_create( \%row );
                push @newurls, \%row;
                $needed--;
            }
        }
        $c->stash->{urls} = \@newurls;
    }
    else { $c->stash->{urls} = \@oldurls; }
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

# vim: set ts=8 sts=4 sw=4 noet:
#

1;
