use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Shop',
    license             => 'perl',
    dist_author         => 'Dr Bean <drbean at (a) freeshell dot (,) org>',
    dist_version_from   => 'lib/Shop.pm',
    build_requires => {
        'Test::More' => 0,
    },
    add_to_cleanup      => [ 'Shop-*' ],
    create_makefile_pl => 'traditional',
);

$builder->create_build_script();
