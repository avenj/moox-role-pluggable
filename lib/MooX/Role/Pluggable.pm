package MooX::Role::Pluggable;
our $VERSION = '0.06';

use Moo::Role;

use Carp;
use strictures 1;

use Scalar::Util 'blessed';
use Try::Tiny;

use MooX::Role::Pluggable::Constants;

##
use namespace::clean -except => 'meta';


has '__pluggable_opts' => (
  is  => 'ro',
  default => sub {
    {
      reg_prefix => 'plugin_',
      ev_prefix  => 'plugin_ev_',
      types      => { PROCESS => 'P',  NOTIFY => 'N' },
    },
  },
);

has '__pluggable_loaded' => (
  is      => 'ro',
  default => sub {
    ALIAS  => {},  ## Objs keyed by aliases
    OBJ    => {},  ## Aliases keyed by obj
    HANDLE => {},  ## Type/event map hashes keyed by obj
  },
);

has '__pluggable_pipeline' => (
  is      => 'ro',
  default => sub { [] },
);


sub _pluggable_destroy {
  my ($self) = @_;
  $self->plugin_del($_) for $self->plugin_alias_list;
}

sub _pluggable_event {
  ## This should be overriden to handle Pluggable events
  ##  ( plugin_{added, removed, error} )
  return
}

sub _pluggable_init {
  my ($self, %params) = @_;
  $params{lc $_} = delete $params{$_} for keys %params;

  $self->__pluggable_opts->{reg_prefix} = $params{reg_prefix}
    if defined $params{reg_prefix};

  $self->__pluggable_opts->{ev_prefix} = $params{event_prefix}
    if defined $params{event_prefix};

  if (defined $params{types}) {
    if (ref $params{types} eq 'ARRAY') {
      $self->__pluggable_opts->{types} = {
        map {;
          $_ => $_
        } @{ $params{types} }
      };
    } elsif (ref $params{types} eq 'HASH') {
      $self->__pluggable_opts->{types} = $params{types}
    } else {
      confess "Expected types to be an ARRAY or HASH"
    }

  }

  $self
}

sub _pluggable_process {
  my ($self, $type, $event, $args) = @_;

  ## This is essentially the same logic as Object::Pluggable.
  ## Profiled and tightened up a bit, but the accessors are a bit
  ## of a hit.
  ## I'm open to optimization ideas . . .
  unless (ref $args) {
    confess "Expected a type, event, and (possibly empty) args ARRAY"
  }

  my $prefix = $self->__pluggable_opts->{ev_prefix};
  substr($event, 0, length($prefix), '')
    if index($event, $prefix) == 0;

  my $meth = join '_', $self->__pluggable_opts->{types}->{$type}, $event;

  my $retval = my $self_ret = EAT_NONE;

  my @extra;

  local $@;

  if      ( $self->can($meth) ) {
    ## Dispatch to ourself
    eval { $self_ret = $self->$meth($self, \(@$args), \@extra) };
    $self->__plugin_process_chk($self, $meth, $self_ret);
  } elsif ( $self->can('_default') ) {
    ## Dispatch to _default
    eval { $self_ret = $self->_default($self, $meth, \(@$args), \@extra) };
    $self->__plugin_process_chk($self, '_default', $self_ret);
  }

  if      (! defined $self_ret ) {
    ## No-op.
  } elsif ( $self_ret == EAT_PLUGIN ) {
     ## Don't plugin-process, just return EAT_NONE.
     ## (Higher levels like Emitter can still pick this up.)
    return $retval
  } elsif ($self_ret == EAT_CLIENT ) {
     ## Plugin process, but return EAT_ALL after.
    $retval = EAT_ALL
  } elsif ($self_ret == EAT_ALL ) {
    return EAT_ALL
  }

  if (@extra) {
    push @$args, @extra;
    @extra = ();
  }

  my $handle_ref = $self->__pluggable_loaded->{HANDLE};

  PLUG: for my $thisplug (@{ $self->__pluggable_pipeline }) {

    if ( 
         $self == $thisplug
      || !exists $handle_ref->{$thisplug}->{$type}
      || (
             !exists $handle_ref->{$thisplug}->{$type}->{$event}
          && !exists $handle_ref->{$thisplug}->{$type}->{all}
         )
    ) {
      next PLUG
    }

    my $plug_ret   = EAT_NONE;
    my $this_alias = ($self->__plugin_get_plug_any($thisplug))[0];

    if      ( $thisplug->can($meth) ) {
      eval { $plug_ret = $thisplug->$meth($self, \(@$args), \@extra) };
      $self->__plugin_process_chk($thisplug, $meth, $plug_ret, $this_alias);
    } elsif ( $thisplug->can('_default') ) {
      eval { $plug_ret = $thisplug->$meth($self, \(@$args), \@extra) };
      $self->__plugin_process_chk($thisplug, '_default', $plug_ret, $this_alias);
    }

    if      (! defined $plug_ret) {
       ## No-op.
    } elsif ($plug_ret == EAT_PLUGIN) {
       ## Stop plugin-processing.
       ## Return EAT_ALL if we previously had a EAT_CLIENT
       ## Return EAT_NONE otherwise
      return $retval
    } elsif ($plug_ret == EAT_CLIENT) {
       ## Set a pending EAT_ALL.
       ## If another plugin in the pipeline returns EAT_PLUGIN,
       ## we'll tell higher layers like Emitter to EAT_ALL
      $retval = EAT_ALL
    } elsif ($plug_ret == EAT_ALL) {
      return EAT_ALL
    }

    if (@extra) {
      push @$args, @extra;
      @extra = ();
    }

  }  ## PLUG

  $retval
}

sub __plugin_process_chk {
  my ($self, $obj, $meth, $retval, $src) = @_;
  $src = defined $src ? "plugin '$src'" : "self" ;

  if ($@) {
    chomp $@;
    my $err = "$meth call on $src failed: $@";

    warn "$err\n";

    $self->_pluggable_event(
      $self->__pluggable_opts->{ev_prefix} . "plugin_error",
      $err,
      ( $obj == $self ? ($obj, $src) : () ),
    );

    return
  }

  if (! defined $retval ||
   (
        $retval != EAT_NONE
     && $retval != EAT_PLUGIN
     && $retval != EAT_CLIENT
     && $retval != EAT_ALL
   ) ) {

    my $err = "$meth call on $src did not return a valid EAT_ constant";

    warn "$err\n";

    $self->_pluggable_event(
      $self->__pluggable_opts->{ev_prefix} . "plugin_error",
      $err,
      ( $obj == $self ? ($obj, $src) : () ),
    );

    return
  }
}


### Basic plugin manipulation (add/del/get/replace ...)

sub plugin_add {
  my ($self, $alias, $plugin, @args) = @_;

  confess "Expected a plugin alias and object"
    unless defined $alias and blessed $plugin;

  $self->plugin_pipe_push($alias, $plugin, @args)
}

sub plugin_alias_list {
  my ($self) = @_;
  keys %{ $self->__pluggable_loaded->{ALIAS} }
}

sub plugin_del {
  my ($self, $alias_or_plug, @args) = @_;

  confess "Expected a plugin alias"
    unless defined $alias_or_plug;

  scalar( $self->__plugin_pipe_remove($alias_or_plug, @args) )
}

sub plugin_get {
  my ($self, $item) = @_;

  my ($item_alias, $item_plug) = $self->__plugin_get_plug_any($item);

  unless (defined $item_plug) {
    carp ($@ = "No such plugin: $item_alias");
    return
  }

  wantarray ? ($item_plug, $item_alias) : $item_plug
}

sub plugin_replace {
  my ($self, %params) = @_;
  $params{lc $_} = delete $params{$_} for keys %params;

  ## ->plugin_replace(
  ##   old    => $obj || $alias,
  ##   alias  => $newalias,
  ##   plugin => $newplug,
  ## # optional:
  ##   unregister_args => ARRAY
  ##   register_args   => ARRAY
  ## )

  for (qw/old alias plugin/) {
    confess "Missing required param $_"
      unless defined $params{$_}
  }

  my ($old_alias, $old_plug)
    = $self->__plugin_get_plug_any( $params{old} );

  unless (defined $old_plug) {
    $@ = "No such plugin: $old_alias";
    carp $@;
    return
  }

  my @unreg_args = ref $params{unregister_args} eq 'ARRAY' ?
    @{ $params{unregister_args} } : () ;

  $self->__plug_pipe_unregister( $old_alias, $old_plug, @unreg_args );

  my ($new_alias, $new_plug) = @params{'alias','plugin'};

  return unless $self->__plug_pipe_register( $new_alias, $new_plug,
    (
      ref $params{register_args} eq 'ARRAY' ?
        @{ $params{register_args} } : ()
    ),
  );

  for my $thisplug (@{ $self->__pluggable_pipeline }) {
    if ($thisplug == $old_plug) {
      $thisplug = $params{plugin};
      last
    }
  }

  $old_plug
}


### Event registration.

sub subscribe {
  my ($self, $plugin, $type, @events) = @_;

  if (!grep { $_ eq $type } keys %{ $self->__pluggable_opts->{types} }) {
    carp "Cannot subscribe; event type $type not supported";
    return
  }

  unless (@events) {
    carp
      "Expected a plugin object, a type, and a list of events";
    return
  }

  unless (blessed $plugin) {
    carp "Expected a blessed plugin object";
    return
  }

  my $handles
    = $self->__pluggable_loaded->{HANDLE}->{$plugin}->{$type} //= {};

  for my $ev (@events) {
    if (ref $ev eq 'ARRAY') {
      $handles->{$_} = 1 for @$ev;
    } else {
      $handles->{$ev} = 1;
    }
  }

  1
}

sub unsubscribe {
  my ($self, $plugin, $type, @events) = @_;

  if (!grep { $_ eq $type } keys %{ $self->__pluggable_opts->{types} }) {
    carp "Cannot unsubscribe; event type $type not supported";
    return
  }

  unless (blessed $plugin && defined $type) {
    carp
      "Expected a blessed plugin obj, event type, and events to unsubscribe";
    return
  }

  unless (@events) {
    carp "No events specified; did you mean to plugin_del instead?";
    return
  }

  my $handles
    = $self->__pluggable_loaded->{HANDLE}->{$plugin}->{$type} || {};

  for my $ev (@events) {

    if (ref $ev eq 'ARRAY') {
      for my $this_ev (@$ev) {
        unless (delete $handles->{$this_ev}) {
          carp "Nonexistant event $this_ev cannot be unsubscribed from";
        }
      }
    } else {
      unless (delete $handles->{$ev}) {
        carp "Nonexistant event $ev cannot be unsubscribed from";
      }
    }

  }

  1
}


### Pipeline methods.

sub plugin_pipe_push {
  my ($self, $alias, $plug, @args) = @_;

  if (my $existing = $self->__plugin_by_alias($alias) ) {
    $@ = "Already have plugin $alias : $existing";
    carp $@;
    return
  }

  return unless $self->__plug_pipe_register($alias, $plug, @args);

  push @{ $self->__pluggable_pipeline }, $plug;

  scalar @{ $self->__pluggable_pipeline }
}

sub plugin_pipe_pop {
  my ($self, @args) = @_;

  return unless @{ $self->__pluggable_pipeline };

  my $plug  = pop @{ $self->__pluggable_pipeline };
  my $alias = $self->__plugin_by_ref($plug);

  $self->__plug_pipe_unregister($alias, $plug, @args);

  wantarray ? ($plug, $alias) : $plug
}

sub plugin_pipe_unshift {
  my ($self, $alias, $plug, @args) = @_;

  if (my $existing = $self->__plugin_by_alias($alias) ) {
    carp "Already have plugin $alias : $existing";
    return
  }

  return unless $self->__plug_pipe_register($alias, $plug, @args);

  unshift @{ $self->__pluggable_pipeline }, $plug;

  scalar @{ $self->__pluggable_pipeline }
}

sub plugin_pipe_shift {
  my ($self, @args) = @_;

  return unless @{ $self->__pluggable_pipeline };

  my $plug  = shift @{ $self->__pluggable_pipeline };
  my $alias = $self->__plugin_by_ref($plug);

  $self->__plug_pipe_unregister($alias, $plug, @args);

  wantarray ? ($plug, $alias) : $plug
}

sub __plugin_pipe_remove {
  my ($self, $old, @unreg_args) = @_;

  my ($old_alias, $old_plug) = $self->__plugin_get_plug_any($old);

  unless (defined $old_plug) {
    $@ = "No such plugin: $old_alias";
    carp $@;
    return
  }

  my $idx = 0;
  for my $thisplug (@{ $self->__pluggable_pipeline }) {
    if ($thisplug == $old_plug) {
      splice @{ $self->__pluggable_pipeline }, $idx, 1;
      last
    }
    ++$idx;
  }

  $self->__plug_pipe_unregister( $old_alias, $old_plug, @unreg_args );

  wantarray ? ($old_plug, $old_alias) : $old_plug
}

sub plugin_pipe_get_index {
  my ($self, $item) = @_;

  my ($item_alias, $item_plug) = $self->__plugin_get_plug_any($item);

  unless (defined $item_plug) {
    $@ = "No such plugin: $item_alias";
    carp $@;
    return -1
  }

  my $idx = 0;
  for my $thisplug (@{ $self->__pluggable_pipeline }) {
    return $idx if $thisplug == $item_plug;
    $idx++;
  }

  return -1
}

sub plugin_pipe_insert_before {
  my ($self, %params) = @_;
  $params{lc $_} = delete $params{$_} for keys %params;
  ## ->insert_before(
  ##   before =>
  ##   alias  =>
  ##   plugin =>
  ##   register_args =>
  ## );

  for (qw/before alias plugin/) {
    confess "Missing required param $_"
      unless defined $params{$_}
  }

  my ($prev_alias, $prev_plug)
    = $self->__plugin_get_plug_any( $params{before} );

  unless (defined $prev_plug) {
    $@ = "No such plugin: $prev_alias";
    carp $@;
    return
  }

  if ( my $existing = $self->__plugin_by_alias($params{alias}) ) {
    $@ = "Already have plugin $params{alias} : $existing";
    carp $@;
    return
  }

  return unless $self->__plug_pipe_register(
    $params{alias}, $params{plugin},
    (
      ref $params{register_args} eq 'ARRAY' ?
        @{ $params{register_args} } : ()
    )
  );

  my $idx = 0;
  for my $thisplug (@{ $self->__pluggable_pipeline }) {
    if ($thisplug == $prev_plug) {
      splice @{ $self->__pluggable_pipeline }, $idx, 0, $params{plugin};
      last
    }
    $idx++;
  }

  1
}

sub plugin_pipe_insert_after {
  my ($self, %params) = @_;
  $params{lc $_} = delete $params{$_} for keys %params;

  for (qw/after alias plugin/) {
    confess "Missing required param $_"
      unless defined $params{$_}
  }

  my ($next_alias, $next_plug)
    = $self->__plugin_get_plug_any( $params{after} );

  unless (defined $next_plug) {
    $@ = "No such plugin: $next_alias";
    carp $@;
    return
  }

  if ( my $existing = $self->__plugin_by_alias($params{alias}) ) {
    $@ = "Already have plugin $params{alias} : $existing";
    carp $@;
    return
  }

  return unless $self->__plug_pipe_register(
    $params{alias}, $params{plugin},
    (
      ref $params{register_args} eq 'ARRAY' ?
        @{ $params{register_args} } : ()
    ),
  );

  my $idx = 0;
  for my $thisplug (@{ $self->__pluggable_pipeline }) {
    if ($thisplug == $next_plug) {
      splice @{ $self->__pluggable_pipeline }, $idx+1, 0, $params{plugin};
      last
    }
    $idx++;
  }

  1
}

sub plugin_pipe_bump_up {
  my ($self, $item, $delta) = @_;

  my $idx = $self->plugin_pipe_get_index($item);
  return -1 unless $idx >= 0;

  my $pos = $idx - ($delta || 1);

  unless ($pos >= 0) {
    carp "Negative position ($idx - $delta is $pos), bumping to head"
  }

  splice @{ $self->__pluggable_pipeline }, $pos, 0,
    splice @{ $self->__pluggable_pipeline }, $idx, 1;

  $pos
}

sub plugin_pipe_bump_down {
  my ($self, $item, $delta) = @_;

  my $idx = $self->plugin_pipe_get_index($item);
  return -1 unless $idx >= 0;

  my $pos = $idx + ($delta || 1);

  if ($pos >= @{ $self->__pluggable_pipeline }) {
    carp "Cannot bump below end of pipeline, bumping to end"
  }

  splice @{ $self->__pluggable_pipeline }, $pos, 0,
    splice @{ $self->__pluggable_pipeline }, $idx, 1;

  $pos
}

sub __plug_pipe_register {
  my ($self, $new_alias, $new_plug, @args) = @_;

  ## Register this as a known plugin.
  ## Try to call $reg_prefix . "register"

  my ($retval, $err);
  my $meth = $self->__pluggable_opts->{reg_prefix} . "register" ;

  try {
    $retval = $new_plug->$meth( $self, @args )
  } catch {
    chomp;
    $err = "$meth call on '$new_alias' failed: $_";
  };

  unless ($retval) {
    $err = "$meth call on '$new_alias' returned false";
  }

  if ($err) {
    $self->__plug_pipe_handle_err( $err, $new_plug, $new_alias );
    return
  }

  $self->__pluggable_loaded->{ALIAS}->{$new_alias} = $new_plug;
  $self->__pluggable_loaded->{OBJ}->{$new_plug}    = $new_alias;

  $self->_pluggable_event(
    $self->__pluggable_opts->{ev_prefix} . "plugin_added",
    $new_alias,
    $new_plug
  );

  $retval
}

sub __plug_pipe_unregister {
  my ($self, $old_alias, $old_plug, @args) = @_;

  my ($retval, $err);
  my $meth = $self->__pluggable_opts->{reg_prefix} . "unregister" ;

  try {
    $retval = $old_plug->$meth( $self, @args )
  } catch {
    chomp;
    $err = "$meth call on '$old_alias' failed: $_";
  };

  unless ($retval) {
    $err = "$meth called on '$old_alias' returned false";
  }

  if ($err) {
    $self->__plug_pipe_handle_err( $err, $old_plug, $old_alias );
  }

  delete $self->__pluggable_loaded->{ALIAS}->{$old_alias};
  delete $self->__pluggable_loaded->{OBJ}->{$old_plug};
  delete $self->__pluggable_loaded->{HANDLE}->{$old_plug};

  $self->_pluggable_event(
    $self->__pluggable_opts->{ev_prefix} . "plugin_removed",
    $old_alias,
    $old_plug
  );

  $retval
}

sub __plug_pipe_handle_err {
  my ($self, $err, $plugin, $alias) = @_;

  warn "$err\n";

  $self->_pluggable_event(
    $self->__pluggable_opts->{ev_prefix} . "plugin_error",
    $err,
    $plugin,
    $alias
  );
}

sub __plugin_by_alias {
  my ($self, $item) = @_;

  $self->__pluggable_loaded->{ALIAS}->{$item}
}

sub __plugin_by_ref {
  my ($self, $item) = @_;

  $self->__pluggable_loaded->{OBJ}->{$item}
}

sub __plugin_get_plug_any {
  my ($self, $item) = @_;

  ref $item ?
    ( $self->__pluggable_loaded->{OBJ}->{$item}, $item )
    : ( $item, $self->__pluggable_loaded->{ALIAS}->{$item} );
}


1;

=pod

=head1 NAME

MooX::Role::Pluggable - Add a plugin pipeline to your cows

=head1 SYNOPSIS

  ## A simple pluggable dispatcher.
  package MyDispatcher;
  use Moo;

  use MooX::Role::Pluggable::Constants;

  with 'MooX::Role::Pluggable';

  sub BUILD {
    my ($self) = @_;

    ## (optionally) Configure our plugin pipeline
    $self->_pluggable_init(
      reg_prefix => 'Plug_',
      ev_prefix  => 'Event_',
      types      => {
        NOTIFY  => 'N',
        PROCESS => 'P',
      },
    );
  }

  around '_pluggable_event' => sub {
    ## Override redirecting internal events to process()
    my ($orig, $self) = splice @_, 0, 2;
    $self->process( @_ )
  };

  sub process {
    my ($self, $event, @args) = @_;

    ## Dispatch to 'P_' prefixed "PROCESS" type handlers:
    my $retval = $self->_pluggable_process( 'PROCESS',
      $event,
      \@args
    );

    unless ($retval == EAT_ALL) {
      ## The pipeline allowed the event to continue.
      ## A dispatcher might re-dispatch elsewhere, etc.
    }
  }

  sub shutdown {
    my ($self) = @_;
    ## Unregister all of our plugins.
    $self->_pluggable_destroy;
  }


  ## A Plugin object.
  package MyPlugin;

  use MooX::Role::Pluggable::Constants;

  sub new { bless {}, shift }

  sub Plug_register {
    my ($self, $core) = @_;

    ## Subscribe to events:
    $core->subscribe( $self, 'PROCESS',
      qw/
        my_event
      /,
    );

    ## Log that we're here, do some initialization, etc.

    return EAT_NONE
  }

  sub Plug_unregister {
    my ($self, $core) = @_;
    ## Called at unregister-time.
    return EAT_NONE
  }

  sub P_my_event {
    ## Handle a dispatched "PROCESS"-type event

    my ($self, $core) = splice @_, 0, 2;

    ## Arguments are references and can be modified:
    my $arg = ${ $_[0] };

    . . .

    ## Return an EAT constant to control event lifetime
    ## EAT_NONE allows this event to continue through the pipeline
    return EAT_NONE
  }

  ## A simple controller that interacts with our dispatcher.
  package MyController;

  use Moo;

  has 'dispatcher' => (
    is      => 'rw',
    default => sub {  MyDispatcher->new()  },
  );

  sub BUILD {
    my ($self) = @_;
    $self->dispatcher->plugin_add( 'MyPlugin',
      MyPlugin->new()
    );
  }

  sub do_stuff {
    my $self = shift;
    $self->dispatcher->process( 'my_event', @_ )
  }

=head1 DESCRIPTION

A L<Moo::Role> for turning instances of your class into pluggable objects.
Consumers of this role gain a plugin pipeline and methods to manipulate it,
as well as a flexible dispatch system (see L</_pluggable_process>).

The logic and behavior is based almost entirely on L<Object::Pluggable>. 
Some methods are the same; implementation & interface differ some and you 
will still want to read thoroughly if coming from L<Object::Pluggable>.

It may be worth noting that this is nothing at all like the Moose 
counterpart L<MooseX::Role::Pluggable>. If the names confuse ... well, I 
lacked for better ideas. ;-)

If you're using L<POE>, also see L<MooX::Role::POE::Emitter>.

=head2 Initialization

=head3 _pluggable_init

  $self->_pluggable_init(
    ## Prefix for registration events.
    ## Defaults to 'plugin_' ('plugin_register' / 'plugin_unregister')
    reg_prefix   => 'plugin_',

    ## Prefix for dispatched internal events
    ##  (add, del, error, register, unregister ...)
    ## Defaults to 'plugin_ev_'
    event_prefix => 'plugin_ev_',

    ## Map type names to prefixes.
    ## Event types are arbitrary.
    ## Prefix is prepended when dispathing events of a particular type.
    ## Defaults to: { NOTIFY => 'N', PROCESS => 'P' }
    types => {
      NOTIFY  => 'N',
      PROCESS => 'P',
    },
  );

A consumer can call B<_pluggable_init> to set up pipeline-related options 
appropriately; this should be done prior to loading plugins or dispatching 
to L</_pluggable_process>. If it is never called, the defaults 
(as shown above) are used.

B<< types => >> can be either an ARRAY of event types (which will be used 
as prefixes):

  types => [ qw/ IncomingEvent OutgoingEvent / ],

... or a HASH mapping an event type to a prefix:

  types => {
    Incoming => 'I',
    Outgoing => 'O',
  },

A '_' is automatically appended to event type prefixes when events are 
dispatched via L</_pluggable>, but not to C<reg_prefix>/C<event_prefix>. An 
empty string C<reg_prefix>/C<event_prefix> is valid.

=head3 _pluggable_destroy

  $self->_pluggable_destroy;

Shuts down the plugin pipeline, unregistering all known plugins.

=head3 _pluggable_event

  sub _pluggable_event {
    my ($self, $event, @args) = @_;
    ## Dispatch out, perhaps.
  }

C<_pluggable_event> is called for internal notifications.

It should be overriden in your consuming class to do something useful with 
the dispatched event (and any other arguments passed in).

The C<$event> passed will be prefixed with the configured B<event_prefix>.

Also see L</Internal events>

=head2 Registration

=head3 subscribe

  $self->subscribe( $plugin_obj, $type, @events );

Registers a plugin object to receive C<@events> of type C<$type>.

This is frequently called from within the plugin's registration handler:

  ## In a plugin:
  sub plugin_register {
    my ($self, $core) = @_;

    $core->subscribe( $self, 'PROCESS',
      qw/
        my_event
        another_event
      /
    );

    $core->subscribe( $self, 'NOTIFY', 'all' );
  }

Subscribe to 'all' to receive all events.

=head3 unsubscribe

The unregister counterpart to L</subscribe>; stops delivering
specified events to a plugin.

Carries the same arguments as L</subscribe>.

=head3 plugin_register

The C<plugin_register> method is called on a loaded plugin when it is 
added to the pipeline; it is passed the plugin object (C<$self>), the 
Pluggable object, and any arguments given to L</plugin_add> (or similar 
registration methods).

Normally one might call a L</subscribe> from here to start receiving 
events after load-time:

  sub plugin_register {
    my ($self, $core, @args) = @_;
    $core->subscribe( $self, 'NOTIFY', @events );
  }

=head3 plugin_unregister

The unregister counterpart to L</plugin_register>, called when the object 
is removed from the pipeline by normal means.

Carries the same arguments.

=head2 Dispatch

=head3 _pluggable_process

  my $eat = $self->_pluggable_process( $type, $event, \@args );
  return 1 if $eat == EAT_ALL;

The C<_pluggable_process> method handles dispatching.

If C<$event> is prefixed with our event prefix (see L</_pluggable_init>),
the prefix is stripped prior to dispatch (to be replaced with a type 
prefix matching the specified C<$type>).

Arguments should be passed in as an ARRAY. During dispatch, references to 
the arguments are passed to subs following automatically-prepended objects 
belonging to the plugin and the pluggable caller, respectively:

  my @args = qw/baz bar/;
  $self->_pluggable_process( 'NOTIFY', 'foo', \@args );

  ## In a plugin:
  sub N_foo {
    my ($self, $core) = splice @_, 0, 2;
    ## Dereferenced expected scalars:
    my $baz = ${ $_[0] };
    my $bar = ${ $_[1] };
  }

This allows for argument modification as an event is passed along the 
pipeline.

Dispatch process for C<$event> 'foo' of C<$type> 'NOTIFY':

  - Prepend the known prefix for the specified type, and '_'
    'foo' -> 'N_foo'
  - Attempt to dispatch to $self->N_foo()
  - If no such method, attempt to dispatch to $self->_default()
  - If the event was not eaten (see below), dispatch to plugins

"Eaten" means a handler returned a EAT_* constant from 
L<MooX::Role::Pluggable::Constants> indicating that the event's lifetime 
should terminate.

Specifically:

B<If our consuming class provides a method or '_default' that returns:>

    EAT_ALL:    skip plugin pipeline, return EAT_ALL
    EAT_CLIENT: continue to plugin pipeline
                return EAT_ALL if plugin returns EAT_PLUGIN later
    EAT_PLUGIN: skip plugin pipeline entirely
                return EAT_NONE unless EAT_CLIENT was seen previously
    EAT_NONE:   continue to plugin pipeline

B<If one of our plugins in the pipeline returns:>

    EAT_ALL:    skip further plugins, return EAT_ALL
    EAT_CLIENT: continue to next plugin, set pending EAT_ALL
                (EAT_ALL will be returned when plugin processing finishes)
    EAT_PLUGIN: return EAT_ALL if previous sub returned EAT_CLIENT
                else return EAT_NONE
    EAT_NONE:   continue to next plugin

This functionality (derived from L<Object::Pluggable>) provides 
fine-grained control over event lifetime.

Higher layers can check for an C<EAT_ALL> return value from 
_pluggable_process to determine whether to continue operating on a 
particular event 
(re-dispatch elsewhere, for example). Plugins can use 'EAT_CLIENT' to 
indicate that an event should be eaten after plugin processing 
is complete, 'EAT_PLUGIN' to stop plugin processing, and 'EAT_ALL' 
to indicate that the event should not be dispatched further.

=head2 Public Methods

Plugin pipeline manipulation methods will set C<$@>, C<carp()>, and return 
empty list on error (unless otherwise noted). See L</plugin_error> 
regarding errors raised during plugin registration and dispatch.

=head3 plugin_add

  $self->plugin_add( $alias, $plugin_obj, @args );

Add a plugin object to the pipeline. Returns the same values as 
L</plugin_pipe_push>.

=head3 plugin_del

  $self->plugin_del( $alias_or_plugin_obj, @args );

Remove a plugin from the pipeline.

Takes either a plugin alias or object. Returns the removed plugin object.

=head3 plugin_get

  my $plug_obj = $self->plugin_get( $alias );
	my ($plug_obj, $plug_alias) = $self->plugin_get( $alias_or_plugin_obj );

In scalar context, returns the plugin object belonging to the specified 
alias.

In list context, returns the object and alias, respectively.

=head3 plugin_alias_list

  my @loaded = $self->plugin_alias_list;

Returns a list of loaded plugin aliases.

=head3 plugin_replace

  $self->plugin_replace(
    old    => $alias_or_plugin_obj,
    alias  => $new_alias,
    plugin => $new_plugin_obj,
    ## Optional:
    register_args   => [ ],
    unregister_args => [ ],
  );

Replace an existing plugin object with a new one.

Returns the old (removed) plugin object.

=head2 Pipeline methods

=head3 plugin_pipe_push

  $self->plugin_pipe_push( $alias, $plugin_obj, @args );

Add a plugin to the end of the pipeline. (Typically one would use 
L</plugin_add> rather than calling this method directly.)

=head3 plugin_pipe_pop

  my $plug = $self->plugin_pipe_pop( @unregister_args );

Pop the last plugin off the pipeline, passing any specified arguments to 
L</plugin_unregister>.

In scalar context, returns the plugin object that was removed.

In list context, returns the plugin object and alias, respectively.

=head3 plugin_pipe_unshift

  $self->plugin_pipe_unshift( $alias, $plugin_obj, @args );

Add a plugin to the beginning of the pipeline.

Returns the total number of loaded plugins (or an empty list on failure).

=head3 plugin_pipe_shift

  $self->plugin_pipe_shift( @unregister_args );

Shift the first plugin off the pipeline, passing any specified args to 
L</plugin_unregister>.

In scalar context, returns the plugin object that was removed.

In list context, returns the plugin object and alias, respectively.

=head3 plugin_pipe_get_index

  my $idx = $self->plugin_pipe_get_index( $alias_or_plugin_obj );
  if ($idx < 0) {
    ## Plugin doesn't exist
  }

Returns the position of the specified plugin in the pipeline.

Returns -1 if the plugin does not exist.

=head3 plugin_pipe_insert_after

  $self->plugin_pipe_insert_after(
    after  => $alias_or_plugin_obj,
    alias  => $new_alias,
    plugin => $new_plugin_obj,
    ## Optional:
    register_args => [ ],
  );

Add a plugin to the pipeline after the specified previously-existing alias 
or plugin object. Returns boolean true on success.

=head3 plugin_pipe_insert_before

  $self->plugin_pipe_insert_before(
    before => $alias_or_plugin_obj,
    alias  => $new_alias,
    plugin => $new_plugin_obj,
    ## Optional:
    register_args => [ ],
  );

Similar to L</plugin_pipe_insert_after>, but insert before the specified 
previously-existing plugin, not after.

=head3 plugin_pipe_bump_up

  $self->plugin_pipe_bump_up( $alias_or_plugin_obj, $count );

Move the specified plugin 'up' C<$count> positions in the pipeline.

Returns -1 if the plugin cannot be bumped up any farther.

=head3 plugin_pipe_bump_down

  $self->plugin_pipe_bump_down( $alias_or_plugin_obj, $count );

Move the specified plugin 'down' C<$count> positions in the pipeline.

Returns -1 if the plugin cannot be bumped down any farther.

=head2 Internal events

=head3 plugin_error

Issued via L</_pluggable_event> when an error occurs.

The first argument is always the error string; if it wasn't our consumer 
class that threw the error, the source object is included as the second 
argument.

=head3 plugin_added

Issued via L</_pluggable_event> when a new plugin is registered.

Arguments are the new plugin alias and object, respectively.

=head3 plugin_removed

Issued via L</_pluggable_event> when a plugin is unregistered.

Arguments are the old plugin alias and object, respectively.

=head1 AUTHOR

Jon Portnoy <avenj@cobaltirc.org>

Based on L<Object::Pluggable> by BINGOS, HINRIK, APOCAL, japhy et al.

=cut
