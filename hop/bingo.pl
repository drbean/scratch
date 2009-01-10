#!perl

$c = 'aa bb aa bb aa bb';
$i++ while $c =~ s/aa/bb/;
print $i;
