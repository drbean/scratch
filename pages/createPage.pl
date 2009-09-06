our $VERSION = 0.01;

package Script;

use Moose;
with 'MooseX::Getopt';

has 'man'  => ( is => 'ro', isa => 'Bool' );
has 'help' => ( is => 'ro', isa => 'Bool' );
has 'area' => (
    traits      => ['Getopt'],
    is          => 'ro',
    isa         => 'ArrayRef',
    cmd_aliases => 'a',
);

package main;

use YAML qw/LoadFile/;
use IO::All;
use Text::Template;
use Pod::Usage;

my $io   = io "access.html";
my $starthtml =
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title> AccessDictations - </title>
</head>
<body>
<h1>Self-Access Listening</h1>
<p>Links and commentary about sound files on the Internet. Listen to the 
soundfiles and write down what you hear.</p>
';
my $areastring = '<div class=area id="<TMPL> $id </TMPL>">
<h2><a name=<TMPL> $id </TMPL>><TMPL> $title </TMPL></a></h2>
<p>
<TMPL> $blurb </TMPL>
';
my $topicstring = '<div class=topic id="<TMPL> $id </TMPL>">
<h3><a name=<TMPL> $id </TMPL>><TMPL> $title </TMPL></a></h3>
';
my $storystring = '<div class=story id="<TMPL> $id </TMPL>">
<h4><a name=<TMPL> $id </TMPL>><TMPL> $title </TMPL></a></h4>
<p>
<TMPL> $blurb </TMPL>
</p>
<p>
<TMPL> $comment </TMPL>
</p>
<p>
Fill in the blanks in the <a 
href=http://203.64.184.141/cgi-bin/dic/script_files/dic_cgi.pl/login?exercise=<TMPL> $id </TMPL>><TMPL> $id </TMPL></a> exercise.
</p>
</div>
';
my $areatmpl = Text::Template->new(
    type       => 'string',
    source     => $areastring,
    delimiters => [ '<TMPL>', '</TMPL>' ]
);
my $topictmpl = Text::Template->new(
    type       => 'string',
    source     => $topicstring,
    delimiters => [ '<TMPL>', '</TMPL>' ]
);
my $storytmpl = Text::Template->new(
    type       => 'string',
    source     => $storystring,
    delimiters => [ '<TMPL>', '</TMPL>' ]
);

run() unless caller;
sub run {
    my $script = Script->new_with_options();
    pod2usage(1) if $script->help;
    pod2usage( -exitstatus => 0, -verbose => 2 ) if $script->man;
    my $areas = $script->area;
    $io->print($starthtml);

    for my $area ( @$areas ) {
	my $listening = LoadFile "$area/content.yaml";
	$io->append( $areatmpl->fill_in( hash => $listening ) );
	$io->append( topicsAndStories( $listening ) );
    }
    $io->append( "\n</body>\n</html>\n" );
}

sub topicsAndStories {
    my $area = shift;
    for my $topic ( @{ $area->{topic} } ) {
	$io->append( $topictmpl->fill_in( hash => $topic ) );
	for my $story ( @{ $topic->{story} } ) {
	    $io->append( $storytmpl->fill_in( hash => $story ) );
	}
	$io->append("\n</div>\n");
    }
}

# vim: set ts=8 sts=4 sw=4 noet:
