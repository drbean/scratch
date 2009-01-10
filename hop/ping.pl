#!perl

use warnings;
use strict;

use Net::Ping;

my @host_array;
# @host_array = map { "pl00$_.nas921.nara.nttpc.ne.jp" } 1 .. 9;
# push @host_array, map { "pl0$_.nas921.nara.nttpc.ne.jp" } 10 .. 99;
# push @host_array, map { "pl$_.nas921.nara.nttpc.ne.jp" } 100 .. 254;
# push @host_array, map { "pl$_.nas921.nara.nttpc.ne.jp" } 254 .. 767;

@host_array = map { "61.197.71.$_" } 1 .. 254;
# @host_array = map { "61.197.72.$_" } 1 .. 254;

# @host_array = map { "pl00$_.nas931.nara.nttpc.ne.jp" } 1 .. 9;
# push @host_array, map { "pl0$_.nas931.nara.nttpc.ne.jp" } 10 .. 99;
# push @host_array, map { "pl$_.nas931.nara.nttpc.ne.jp" } 100 .. 254;
# push @host_array, map { "pl$_.nas931.nara.nttpc.ne.jp" } 254 .. 383;

# @host_array = map { "pl00$_.nas911.nara.nttpc.ne.jp" } 1 .. 9;
# push @host_array, map { "pl0$_.nas911.nara.nttpc.ne.jp" } 10 .. 99;
# push @host_array, map { "pl$_.nas911.nara.nttpc.ne.jp" } 100 .. 127;


my $p = Net::Ping->new("icmp");
foreach my $host (@host_array)
{
	print "$host is reachable.\n" if $p->ping($host, 2);;
	sleep(1);
}
$p->close();


