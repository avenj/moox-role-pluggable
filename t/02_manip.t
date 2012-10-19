use Test::More tests => 45;
use strict; use warnings;

{
  package
    MyDispatcher;
  use strict; use warnings;
  use Test::More;

  use Moo;
  with 'MooX::Role::Pluggable';

  use MooX::Role::Pluggable::Constants;

  sub process {
    my ($self, $event, @args) = @_;
    my $retval = $self->_pluggable_process( 'PROCESS', $event, \@args );
  }

  sub shutdown {
    my ($self) = @_;
    $self->_pluggable_destroy;
  }

  sub do_test_events {
    my ($self) = @_;
    $self->process( 'test', 0 );
    $self->process( 'not_handled' );
  }

  around '_pluggable_event' => sub {
    my ($orig, $self) = splice @_, 0, 2;
    $self->process( @_ )
  };

  sub P_test {
    my ($self, undef) = splice @_, 0, 2;
    ++${ $_[0] };
    ## We should be first.
    cmp_ok( ${ $_[0] }, '==', 1, 'P_test dispatch order correct' );
  }

  sub P_plugin_added {
    ## +6 tests for reloads
    my ($self, undef) = splice @_, 0, 2;
    my $alias = ${ $_[0] };
    my $obj   = ${ $_[1] };
    ok( ($alias && ref $obj), 'got plugin_added');
    EAT_ALL
  }

  sub P_plugin_removed {
    pass("got plugin_removed");
    EAT_ALL
  }

  sub _default {
    my ($self, undef) = splice @_, 0, 2;
    my $event = $_[0];

    cmp_ok( $event, 'eq', 'P_not_handled', '_default triggered' );
    EAT_ALL
  }
}

{
  package
    MyPlugin::A;
  use strict; use warnings;
  use Test::More;

  use MooX::Role::Pluggable::Constants;

  sub new { bless [], shift }

  sub plugin_register {
    my ($self, $core) = splice @_, 0, 2;
    pass( __PACKAGE__ . ' plug registered' );
    $core->subscribe( $self, 'PROCESS', 'all' );
    EAT_NONE
  }

  sub plugin_unregister {
    pass( "Plugin::A unregistered" );
  }

  sub P_test {
    my ($self, $core) = splice @_, 0, 2;
    pass( "Plugin::A got P_test" );
    ++${ $_[0] };
    cmp_ok( ${ $_[0] }, '>', 1, 'P_test dispatch order correct' );
    EAT_NONE
  }

  sub _default {
    my ($self, undef) = splice @_, 0, 2;
    my $event = $_[0];

    ## Should have been EATen by dispatcher
    fail("_default should not have triggered in plugin");
  }
}

{
  package
    MyPlugin::B;
  use strict; use warnings;
  use Test::More;

  use MooX::Role::Pluggable::Constants;

  sub new { bless [], shift }

  sub plugin_register {
    my ($self, $core) = splice @_, 0, 2;
    pass( __PACKAGE__ . ' plug registered' );
    $core->subscribe( $self, 'PROCESS', 'test' );
    EAT_NONE
  }

  sub plugin_unregister {
    pass( "Plugin::B unregistered" );
  }

  sub P_test {
    my ($self, $core) = splice @_, 0, 2;
    pass( "Plugin::B got P_test" );
    EAT_NONE
  }
}

my $disp = MyDispatcher->new;

ok( $disp->does('MooX::Role::Pluggable'), 'Class does Role' );


## plugin_add()
ok( $disp->plugin_add( 'MyPlugA', MyPlugin::A->new ), 'plugin_add()' );
ok( $disp->plugin_add( 'MyPlugB', MyPlugin::B->new ), 'plugin_add() 2' );

## test events
$disp->do_test_events;

## plugin_get()
{
 my $retrieved;
 ok( $retrieved = $disp->plugin_get('MyPlugA'), 'scalar plugin_get()' );
 isa_ok( $retrieved, 'MyPlugin::A' );

 my($obj, $alias);
 cmp_ok(
   ($obj, $alias) = $disp->plugin_get($retrieved),
   '==', 2,
   'list plugin_get()' 
 );
 cmp_ok( $alias, 'eq', 'MyPlugA', 'plugin_get returns correct alias' );
 isa_ok( $obj, 'MyPlugin::A', 'plugin_get returns correct obj' );
}

## plugin_alias_list()
cmp_ok( $disp->plugin_alias_list, '==', 2, 'plugin_alias_list has 2 plugs' );

## plugin_pipe_bump_up()
$disp->plugin_pipe_bump_up( 'MyPlugB', 1 );
cmp_ok( $disp->plugin_pipe_get_index('MyPlugB'), '==', 0, 'PlugB at pos 0' );

## plugin_pipe_bump_down()
$disp->plugin_pipe_bump_down( 'MyPlugB', 1 );
cmp_ok( $disp->plugin_pipe_get_index('MyPlugB'), '==', 1, 'PlugB at pos 1' );

{
## plugin_pipe_shift()
  my $thisplug;
  ok( $thisplug = $disp->plugin_pipe_shift, 'plugin_pipe_shift()' );
  isa_ok( $thisplug, 'MyPlugin::A' );

  cmp_ok( $disp->plugin_pipe_get_index('MyPlugB'), '==', 0, 'PlugB at pos 0' );

## plugin_pipe_unshift()
  ok( $disp->plugin_pipe_unshift(
    'MyPlugA', $thisplug
    ), 'plugin_pipe_unshift'
  );
  cmp_ok( $disp->plugin_pipe_get_index('MyPlugA'), '==', 0, 'PlugA at pos 0' );
}

{
  package
    MyPlugin::Bare;
  use Test::More;
  use strict; use warnings;
  use MooX::Role::Pluggable::Constants;

  sub new { bless [], shift }

  sub plugin_register {
    EAT_NONE
  }

  sub plugin_unregister {
    EAT_NONE
  }
}

## plugin_replace()
ok( $disp->plugin_replace(
    old    => 'MyPlugA',
    alias  => 'NewPlugA',
    plugin => MyPlugin::Bare->new,
  ), 'plugin_replace'
);
cmp_ok($disp->plugin_pipe_get_index('NewPlugA'), '==', 0, 'NewPlug at pos 0' );

## plugin_pipe_insert_after()
ok( $disp->plugin_pipe_insert_after(
    after  => 'NewPlugA',
    alias  => 'NewPlugB',
    plugin => MyPlugin::Bare->new,
  ), 'plugin_pipe_insert_after'
);
cmp_ok($disp->plugin_pipe_get_index('NewPlugB'), '==', 1, 'NewPlugB at pos 1' );

## plugin_pipe_insert_before()
ok( $disp->plugin_pipe_insert_before(
    before => 'NewPlugB',
    alias  => 'NewPlugC',
    plugin => MyPlugin::Bare->new,
  ), 'plugin_pipe_insert_before'
);
cmp_ok($disp->plugin_pipe_get_index('NewPlugC'), '==', 1, 'NewPlugC at pos 1' );

$disp->shutdown;
