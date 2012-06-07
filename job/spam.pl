#!/usr/bin/perl

use strict;
use warnings;

use MIME::QuotedPrint ();
use MIME::Base64 ();
use IO::All;

use Mail::Box::Manager;
use Mail::Message::Field::Full;
use Text::Template;

my $text;
io("copyediting") > $text;

my $schools;
io("langatms") > $schools;

my $template = Text::Template->new(TYPE => 'STRING', SOURCE => $text
                                , DELIMITERS => [ '<TMPL>', '</TMPL>' ] );

my $mgr = Mail::Box::Manager->new(
	trace => 'DEBUG'
);

my @schools = split /\n/, $schools;
for my $school ( @schools )
{
	my ($alias, $number, $data) = split ' ', $school, 3;
	# $data =~ m/^(.*) http[^>]+$/;
	# # die "addresses not parsed in $data" unless $1;
	# # my @addresses = split /, /, $1;
	my @addresses = split /, /, $data;
	my @structures;
	for my $address ( @addresses )
	{
		die "phrase, address not parsed right in $address" unless
			$address =~ m/^(.*) <(.*)>$/;
		push @structures, ["$1", $2];
	}
	my $to = Mail::Message::Field::Addresses->new('To');
	for my $structure ( @structures )
	{
		my ($phrase, $email) =  @$structure;
		$to->addAddress(
			phrase => $phrase, address => $email,
			group => '', charset => 'utf-8' );
	}
	my @groups = $to->groups;
	die "@groups groups in To: field" unless @groups == 1;
	my $group = $groups[0];
	my @recipients = $group->addresses;
	my $tostring;
	my $tmpl;
	for my $recipient ( @recipients )
	{
		my $phrase = $recipient->phrase;
		$tmpl->{school} ||= substr( $phrase, 6, 6) if $phrase;
		my $encoded =
			'=?utf-8?b?' . MIME::Base64::encode_base64($phrase, "") .  '?=';
		my $address = $recipient->address;
		# my $aasstring = join ' ', @encoded, "<$address>";
		my $string = $recipient->string;
		# push @tostring, $aasstring;
		$tostring .= $encoded . " <$address>, ";
	}
	my $letter = $template->fill_in( HASH => $tmpl );
	my $msg = Mail::Message->build(
		From => 'Greg Matheson <lang@ms.chinmin.edu.tw>',
		'Content-Type' => 'text/plain; charset=zh_TW.UTF-8',
		To => $tostring,
		'X-School-ID' => $number,
		Subject => 'Part-time job application',
		'Reply-To' => 'Greg Matheson <lang@ms.chinmin.edu.tw>',
		data => [$letter]);
	if ($tostring =~ m/^(?:(?:[^<>]+<[\w-]+@[\w-]+(?:\.[\w-]+)+>[ ,]*)+)$/i )
	{
		print "OK, $tostring\n";
		$mgr->appendMessage('/home/greg/Mail/sent', $msg);
		$msg->send(via => 'exim');
	}
	else {
		print "NOK, $tostring\n";
		$mgr->appendMessage('/home/greg/Mail/broken', $msg);
	}
}
