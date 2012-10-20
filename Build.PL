#!/usr/bin/env perl
use Module::Build;
use strict; use warnings;

my $build = Module::Build->new(
  module_name => 'MooX::Role::Pluggable',
  license     => 'perl',

  dist_abstract => 'Add a plugin pipeline to your cows',
  dist_author   => 'Jon Portnoy <avenj@cobaltirc.org>',

  recursive_test_files => 1,
  create_readme        => 1,
  create_makefile_pl   => 'small',

  configure_requires => {
    'Module::Build' => '0.40',
  },

  build_requires => {
    'Test::More'      => 0,
#    'Test::Exception' => 0,
  },

  requires => {
    'perl'     => '5.10.1',

    'Carp'          => 0,
    'Exporter'      => 0,
    'Scalar::Util'  => 0,

    'strictures'              => 1,
    'namespace::clean'        => '0.23',
    'Moo'                     => 1,
    'Moo::Role'               => 0,
    'Try::Tiny'               => 0,
  },
);

$build->create_build_script;

__END__
Snacks.
