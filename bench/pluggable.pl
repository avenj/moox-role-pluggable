#!/usr/bin/env perl
use strict; use warnings FATAL => 'all';

my $count = 30_000;

{
  package
   Plug::O::P;
  use strict; use warnings FATAL => 'all';

  sub new { bless [], shift }

  sub plugin_register {
    my ($self, $core) = @_;
    $core->plugin_register( $self, 'SERVER', 'test' );
    1
  }
  sub plugin_unregister {
    1
  }
  sub S_test {
    1
  }
}

{
  package
   Plug::MX::P;
  use strict; use warnings FATAL => 'all';

  sub new { bless [], shift }

  sub plugin_register {
    my ($self, $core) = @_;
    $core->subscribe( $self, 'NOTIFY', 'test' );
    1
  }
  sub plugin_unregister {
    1
  }
  sub N_test {
    1
  }
}

{
  package
   Disp::O::P;
  use strictures 1;
  use base 'Object::Pluggable';

  sub new {
    my $self = {};
    bless $self, shift;
    $self->_pluggable_init(
      types => { SERVER => 'S' },
    );
    $self
  }

  sub process {
    my ($self, $event, @args) = @_;
    $self->_pluggable_process( 'SERVER', $event, \@args )
  }
}

{
  package
   Disp::MX::P;
  use Moo;
  with 'MooX::Role::Pluggable';

  sub process {
    my ($self, $event, @args) = @_;
    $self->_pluggable_process( 'NOTIFY', $event, \@args )
  }
}

use Benchmark ':all';

my $op_disp = Disp::O::P->new;
my $mx_disp = Disp::MX::P->new;
$op_disp->plugin_add( 'A'.$_ => Plug::O::P->new )
      for 1 .. 20;
$mx_disp->plugin_add( 'B'.$_ => Plug::MX::P->new )
      for 1 .. 20;

my %stuff = ( $count, +{
  'object-pluggable' => sub {
    $op_disp->process( 'test', 'things' );
  },
  'moox-role-pluggable' => sub {
    $mx_disp->process( 'test', 'things' );
  },
} );

timethese(%stuff);
cmpthese(%stuff);
