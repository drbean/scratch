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
    required => 1,
    cmd_aliases => 'a',
);
has 'story' => (
    traits      => ['Getopt'],
    is          => 'ro',
    isa         => 'Str',
    required => 0,
    cmd_aliases => 's',
);
has 'location' => (
    traits      => ['Getopt'],
    is          => 'ro',
    isa         => 'Str',
    required => 0,
    cmd_aliases => 'l',
);

package main;

use YAML qw/LoadFile/;
use IO::All;
use Text::Template;
use Pod::Usage;
use List::Util qw/first/;
use List::MoreUtils qw/any all/;

my $io   = io "-";
my $areastring = '<div class=area id="<TMPL> $id </TMPL>">
<h2><a name=<TMPL> $id </TMPL>><TMPL> $title </TMPL></a></h2>
<p>
<TMPL> $blurb </TMPL>

';
my $topicstring = '<div class=topic id="<TMPL> $id </TMPL>">
<h3><a name=<TMPL> $id </TMPL>><TMPL> $title </TMPL></a></h3>
<p>
<TMPL> $intro </TMPL>
</p>

';
my $storystring = '
<div class=story id="<TMPL> $id </TMPL>">
<h4><a name=<TMPL> $id </TMPL>><TMPL> $title </TMPL></a></h4>
<p>
<TMPL> $blurb </TMPL>
</p>
<p>
<TMPL> $comment </TMPL>
</p>
<p>
Fill in the blanks in the <a 
href=http://203.64.184.141/<TMPL> $location </TMPL>/login?exercise=<TMPL> $id </TMPL>><TMPL> $id </TMPL></a> exercise. (You will be asked to log in.)
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
    my $stories = $script->story;
    my ( $area, $Area );
    my $location = $script->location;
    if ( not $location ) {
	if ( any { $_ eq 'business' } @$areas ) { $location = 'target'; }
	if ( @$areas > 1 ) { $location = 'access' }
	else { $location = 'dic' }
    }
    if ( @$areas == 1 ) {
	$area = $areas->[0];
	$Area = ucfirst $area;
    }
    else { $area = 'access'; $Area = 'Access'; }

    my $starthtml =
'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>' . $Area . ' Dictations</title>
</head>
<body>
<h1>' . $Area . ' Listening</h1>
<p>Links and commentary about sound files on the Internet. Listen to the 
soundfiles and write down what you hear.</p>
';
    $io->print($starthtml);

    for my $area ( @$areas ) {
	my $listening = LoadFile "$area/content.yaml";
	$io->append( $areatmpl->fill_in( hash => $listening ) );
	$io->append( topicsAndStories( $stories, $listening, $location ) );
    }

    my $endhtml =
"<p>
<h2>Old Exercises</h2>
</p>

<p>
Can't find an exercise you didn't finish in a previous week? You can't do that exercise now for credit. But you may be able to find it on the <a href=http://web.nuu.edu.tw/~greg/Access.html#$area>Self-access</a> page.
</p>

<p>
<h2>Logging in</h2>

</p>

<p>
Note: You need to login to transcribe the conversation.
<ul>
	<li>Your name is your Chinese name in Chinese characters. Your ID is your school ID. 
	<li>The first letter of the ID, eg U or N, is a capital (big) letter. 
	<li>Your password is the first half of your first name. It is in English, not the Chinese character. Thus JiaQi\'s pasword is Jia. See <a href=http://humanum.arts.cuhk.edu.hk/Lexis/Lindict/syllabary>Lin YuTang's Chinese-English dictionary</a> for the Hanyu Pinyin (漢語拼音) spelling.
	<li>If I was not able to find the first character in your name, your password may be a question mark. Eg, 賴?岑. Other people whose names my computer does not have Chinese characters for are 黃慧? and ?琮婷, but their passwords should be okay.
</ul>
</p>

<h2>Contacting Dr Bean</h2>
If you have any problem email me at drbean at (@) freeshell dot (.) org, or come and see me in the Self-Access Learning Room.
</body>
</html
";
    $io->append( $endhtml );
}

sub topicsAndStories {
    my $stories = shift;
    my $area = shift;
    my $location = shift;
    my @storyids = split /,/, $stories;
    my %seen; @seen{ @storyids } = ( undef ) x @storyids;
    my $topicLibrary = $area->{topic};
    my $presentTopic = '';
    for my $id ( @storyids ) {
	for my $topic ( @$topicLibrary ) {
	    my $storyLibrary = $topic->{story};
	    my $story = first { $_->{id} eq $id } @$storyLibrary;
	    next unless $story;
	    unless ( $presentTopic eq $topic->{id} ) {
		$io->append("\n</div>\n\n");
		$io->append( $topictmpl->fill_in( hash => $topic ) )
	    }
	    $presentTopic = $topic->{id};
	    $story->{location} = $location;
	    $io->append( $storytmpl->fill_in( hash => $story ) );
	    $seen{$id}++;
	}
    }
    die "Missing $_ story," for grep { not $seen{$_} } keys %seen;
}

# vim: set ts=8 sts=4 sw=4 noet:

