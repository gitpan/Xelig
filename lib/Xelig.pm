package Xelig;

use strict;
use warnings;

use base qw(Exporter);
our @EXPORT_OK = qw(MVC);
our $VERSION = 0.04;

sub MVC {
    my ($model, $view, $controller_arg) = @_;
    my $controller = ref $controller_arg ?
	$controller_arg :
	Xelig::Controller->new($controller_arg);
    return $controller->control($model, $view);
}

package Xelig::Controller;

use strict;
use warnings;

use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;
use Acme::Util;
use Carp qw(confess);
use XML::Simple; $XML::Simple::PREFERRED_PARSER = 'XML::Parser'; # XML::SAX is evil

sub new {
    my $class = shift;
    my $path = shift || '<controller><template re="*" /></controller>';
    my %args = @_;
    my $self = { map { uc $_ => $args{$_} } keys %args };
    my $default = $self->{DEFAULT} ||= { };

    # Default namespace (prefix) for built-in packages
    $default->{NAMESPACE}   =  'Xelig'	    unless (exists $default->{NAMESPACE});
    # Default Template class if none is supplied
    $default->{CLASS}	    =  'Template'   unless (exists $default->{CLASS});
    # Tag to use to delimit ill-formed sections of XML: used by Xelig::Parser
    $default->{XELIG}	    =  'xelig'	    unless (exists $default->{XELIG});
    # unique tag or @attribute used to identify the template
    $default->{SELECTOR}    =  '@id'	    unless (exists $default->{SELECTOR});
    # bool passed to all templates indicating how restrictive they should be 
    $default->{STRICT}	    =  0	    unless (exists $default->{STRICT});

    # path to XML configuration file: may be needed later in case of error
    $self->{PATH}	= $path;
    # templates are either identified collectively by pattern...
    $self->{RE}	      ||= { };
    $self->{ID}	      ||= { };		# ... or specifically by name

    # make the controller an object as early as possible so its methods
    # are available to the handlers
    bless $self, ref $class || $class;

    my $default_class	    = $default->{CLASS};
    my $default_ns	    = $default->{NAMESPACE};
    my $default_selector    = $default->{SELECTOR};
    my $default_strict	    = $default->{STRICT};
    my $forcearray	    = $self->{FORCEARRAY} || [ qw (template ignore alias) ];

    my $xmlin;
    # FIXME: this (XMLin) should be inside some kind of try/catch block
    {
	local $XML::Simple::PREFERRED_PARSER = 'XML::Parser'; # XML::SAX is evil
	$xmlin = XMLin($path, 
	    forcearray  => $forcearray,
	    contentkey  => '#',
	    keyattr	    => [ ],
	) || '';	# returns an array of template HASH refs
    }

    unless (ref $xmlin eq 'HASH') {
	confess ("new: invalid configuration file: expected XML::Simple::XMLin" .
		 "to produce a HASH ref, got '$xmlin': " . Dumper($xmlin));
    }

    my $templates = $xmlin->{template};

    unless (ref $templates eq 'ARRAY') {
	confess ("new: invalid configuration file: expected ARRAY ref: " .
		 "{ template => [ <- here -> ] }, got '$templates': " . Dumper($xmlin));
    }

    $self->selector(defined $xmlin->{selector} ? $xmlin->{selector} : $default_selector);
    $self->strict(defined $xmlin->{strict} ? $xmlin->{strict} : $default_strict);
    
    for my $template (@$templates) {

	unless (ref $template eq 'HASH') {
	    $template ||= '';
	    confess ("new: invalid configuration file: expected HASH ref: " .
		     "{ template => [ { <- here -> } ] }, got '$template'");
	}
	    
	# 1) establish the name of the class that this template will be implemented by
	# 2) normalise that name if necessary: i.e. prefix the default namespace
	#    to it and/or initial-capitalize it
	# 3) cache the prototype (returned by the proto method of the identified class) 
	#    in the controller object: when lookup() is later performed and a match
	#    is successful, the relevant template will be deserialized from the cache,
	#    and the remaining view-related details (attributes, tag, id)
	#    will be set via init() to complete the template initialization
	
	my $name = $template->{class} || $default_class;
	unless ($name =~ /::/) {
	    $name = ucfirst ($name) unless ($name =~ /^[A-Z]/);
	    $name = sprintf ('%s::%s', $default_ns, $name) 
	}

	my $prototype;
	if (exists $template->{id}) {
	    my $key = $template->{id};
	    $prototype = $self->{ID}->{$key} = $name->proto ($self, $template);
	} elsif (exists $template->{re}) {
	    my $key = $template->{re};
	    $key = '.*' if ($key eq '*');
	    $prototype = $self->{RE}->{$key} = $name->proto ($self, $template);
	} else {
	    confess ("invalid template: template must contain an 'id' or 're' attribute");
	}

	$prototype->strict(1) if ($self->strict);
    }

    # <template id="/" class="Template" />
    $self->{ID}->{'/'} = Xelig::Template->new() unless ($self->{ID}->{'/'});

    return $self;
}

# read-only
sub path { my $self = shift; return $self->{PATH}; }

sub strict { 
    my ($self, $strict) = @_;
    return $self->{STRICT} unless (defined $strict);
    $self->{STRICT} = $strict;
}

sub control {
    local $_;
    my ($self, $model_arg, $view_arg) = @_;
    my ($model, $view);
    if (ref $model_arg) {
	$model = $model_arg;
    } else {
	die ("control: invalid model path: '$model_arg'") 
	    unless (-s $model_arg);
	die ("TODO: automatic model import");
    }

    if (ref $view_arg) {
	$view = $view_arg;
    } else {
	# die ("control: invalid view path: '$view_arg'") unless (-s $view_arg);
	my $parser = Xelig::Parser->new($self);
	$view = $parser->parse($view_arg);
    }

    $view->export($model);
    return $view;
}

sub selector {
    my ($self, $selector) = @_;
    if (defined $selector) {
	confess "selector: invalid selector: $selector"
	    unless ($selector =~ /^#|\w+$/);
	$self->{SELECTOR} = $selector;
    } else {
	return $self->{SELECTOR};
    }
}

sub lookup {
    my ($self, $tag, $attributes) = @_;
    my $selector = $self->selector();
    my $id = ($selector eq '#') ? $tag : $attributes->{substr ($selector, 1)};

    return unless (defined $id);

    my ($lookup_id, $lookup_re) = @{$self}{qw(ID RE)};
    my $prototype;
    
    if ($lookup_id->{$id}) {
	# squashed megabug: clone even if 'unique' id-based in case controller is reused
        $prototype = Acme::Util::clone($lookup_id->{$id});
    } else {
        for my $pattern (keys %$lookup_re) {
            next unless ($id =~ /$pattern/i);
            $prototype = Acme::Util::clone($lookup_re->{$pattern});
        }
    }

    $prototype->init($id, $tag, $attributes) if ($prototype);
    
    return $prototype;  
}

1;

############################################

package Xelig::Parser;

# FIXME This would be a lot simpler with an XML::Pull parser

use strict;
use warnings;
use Acme::Util qw(xmlparse);
use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;
use Carp qw(confess);

# Create a new Parser: blaze through xml
# template, consulting controller to see
# which elements should generate Xelig templates

sub new {
    my ($class, $controller) = @_;
    my $self = {};
    my $xelig_tag = $controller->{DEFAULT}->{XELIG};
    my $xelig = sprintf '(<%s\s+id=(["\'])([^\2]+)\2>)|(</%1$s>)', $xelig_tag;
    my $dreamweaver = '(#BeginEditable\s+((["\'])[^\7]+\7))|(#EndEditable)';
    # TODO Note this is hardwired - don't necessarily
    # change it; just note it
    $self->{ROOT} = '/';
    $self->{RE} = sprintf ('^\s*(?:%s|%s)\s*$', $xelig, $dreamweaver);
    $self->{STACK} = [];
    $self->{CONTROLLER} = $controller;
    $self->{BUFFER} = '';
    bless $self, ref $class || $class;
}

sub parse {
    my ($self, $xml) = @_;
    die ("parse: no xml supplied") unless (defined $xml);
    require XML::Parser::Expat;
    my $parser = XML::Parser::Expat->new('ErrorContext' => 1); 
    my $subparser = XML::Parser::ExpatNB->new('ErrorContext' => 1);
    # We need to be able to recover the main 
    # parser from the subparser in case a subparsed comment is 
    # not selected by the controller: the
    # parent's original_string can then be padded as usual
    $subparser->{SUPERPARSER} = $parser;
    $parser->setHandlers(
	    'Start' => \&start,
	    'End' => \&end,
	    'Default' => \&default,
	    'Comment' => \&comment);
    $parser->{SUBPARSER} = $subparser;
    # Set userdata for both parsers
    $parser->{XELIG} = $subparser->{XELIG} = $self; # store Xelig userdata in expat(s)
    $subparser->setHandlers('Start' => \&start, 'End' => \&end);
    # Fake-open the document node for the fragment (i.e. comment) subparser
    $subparser->parse_more('<root id="/">');
    my $root = $self->{STACK}->[0];
    xmlparse($parser, $xml);
    # Fake-close the document node for the fragment (i.e. comment) subparser
    $subparser->parse_more('</root>');
    # Unset the handlers
    # $subparser->setHandlers ('Start' => 0, 'End' => 0);
    # Break the circular reference - just to be on the safe side
    $subparser->{SUPERPARSER} = $parser->{SUBPARSER} 
	= $subparser->{XELIG} = $parser->{XELIG} = undef;
    $subparser->parse_done(); # Clean up ExpatNB
    # $subparser->release(); # FIXME this causes an error in ExpatNB
    $parser->release();
    my $last = $self->{LAST};
    if ($last) {
	my $buffer = $self->{BUFFER};
	my ($suffix) = ($buffer =~ m{^\s*?($/?[ \t\f]*)});
	# $last->suffix($suffix) if (defined $suffix);
	delete $self->{LAST};
    }
    die ("Xelig::Parser::parse: stack is not empty: " . Dumper($self))
	unless ($#{$self->{STACK}} == -1);
    return $root;
}

sub end {
    my ($expat, $tag) = @_;
    my $self = $expat->{XELIG};

    # if it's the superparser i.e. vanilla blocking Expat
    my $blocking = $expat->{SUPERPARSER};
    my $parser = $blocking ? $blocking : $expat;
    my $element_index = $expat->element_index;
    my $index = $blocking ? ($element_index * -1): $element_index;
    my $indexed = $self->parent->{ELEMENT_INDEX}; 
    my $original_string = $expat->original_string();

    if ((defined $indexed) && ($indexed == $index)) {
	my $this = pop (@{$self->{STACK}});
	$this->empty(1) unless (length $original_string);
	# print Dumper($this), $/;
	delete $this->{ELEMENT_INDEX};
	my $parent = $self->parent;
	my $last = $self->{LAST};
	my $buffer = $self->{BUFFER};

	if ($last) {
	    my ($suffix) = ($buffer =~ m{^\s*?($/?[ \t\f]*)});
	    $last->suffix($suffix);
	}
	# print STDERR $/;
	$self->{BUFFER} = '';
	$self->{LAST} = $this;
	$parent->add($this) if ($parent); 
    } else {
	$self->parent->pad($original_string);
    }
}

sub start {
    local $_;
    my ($expat, $tag, %attributes) = @_;
    # FIXME XML is case-sensitive? 
    my $attributes = { map { lc($_) => $attributes{$_} } keys %attributes };
    my $self = $expat->{XELIG};
    my $controller = $self->{CONTROLLER};
    # If it has the parent field specified, it's the subparser
    my $blocking = $expat->{SUPERPARSER};
    my $parser = $blocking ? $blocking : $expat;
    my $template = $controller->lookup($tag, $attributes);
    
    return $self->parent->pad($parser->original_string) unless ($template);

    my $last = $self->{LAST};
    my $buffer = $self->{BUFFER};
    my ($prefix) = $+ if ($self->{BUFFER} =~ s{($/?[ \t\f]*)$}{});

    # printf STDERR "start_id: '%s'%s", $template->id(), $/; 
    # printf STDERR "start_last: '%s'%s", $last || '', $/;

    $template->prefix($prefix) if (defined $prefix);

    if ($last) {
	my ($suffix) = ($buffer =~ m{^\s*?($/?[ \t\f]*)});
	$last->suffix($suffix) if (defined $suffix);
    }

    # print STDERR $/;

    $self->{BUFFER} = '';

    push (@{$self->{STACK}}, $template);
    # if this is the subparser, the vital statistics are to be found on the superparser
    my $element_index = $expat->element_index();
    my $index = $blocking ? ($element_index  * -1) : $element_index;
    
    $template->{COMMENT} = $tag if ($blocking);
    $template->{ELEMENT_INDEX} = $index || 0;
}

sub comment {
    my ($expat, $comment) = @_;
    my $self = $expat->{XELIG};
    my $re = $self->{RE};
    my $original_string = $expat->original_string;

    unless ($comment =~ /$re/io) {
	$self->{BUFFER} .= $original_string;
	return $self->parent->pad($original_string);
    }

    if ($1) { # Opening xelig tag
	$expat->{SUBPARSER}->parse_more($1);
    } elsif ($4) { # Closing xelig tag
	$expat->{SUBPARSER}->parse_more($4);
    } elsif ($5) { # Opening DreamWeaver comment
	$expat->{SUBPARSER}->parse_more("<dreamweaver id=$6>");
    } elsif ($8) { # Closing DreamWeaver comment
	$expat->{SUBPARSER}->parse_more("</dreamweaver>");
    }
}

sub default {
    my $expat = shift;
    my $self = $expat->{XELIG};
    my $parent = $self->parent;
    my $original_string = $expat->original_string;
    $self->{BUFFER} .= $original_string;
    $parent->pad($original_string);
}

sub parent { return $_[0]->{STACK}->[-1]; }

1;

package Xelig::Template; 

# v 0.0.1 - 2000-02-23 - 2000-03-12
# v 0.0.2 - 2001-12-01 - ????-??-??
# v 0.0.3 - 2003-02-24 - ????-??-?? restored XML::Simple for controller config parsing

# Base class for all Templates. 

# TEMPLATE provides arbitrary access to mixed-content children 
# e.g. <span id="change_me">dynamic text<span id="and_me">ditto</span></span>

# SEQUENCER is used for loops - only children that are templates
# are appended i.e cdata not contained in a template is ignored

use strict;
use warnings;

# Acme::Util::clone requires a Perl_magick_backref-using
# implementation of Perl in order to work with weak
# references.
# FIXME: check to see if this is supported by older perls

use 5.008_000;

use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;
use Carp qw(confess);
use Acme::Util qw(ltrim rtrim); # trim()s to work around <alias ... /> bug (see handle_alias)
use Scalar::Util qw(weaken isweak);

sub new {
    my $class = shift;
    my $self = { 
	TARGET	    => 0,
	MAP	    => {},
	ALIAS	    => {},
	IGNORE	    => {},
	CHILDREN    => [],
	INDEX	    => {}
    };

    bless ($self, ref $class || $class);
}

sub init {
    my ($self, $id, $tag, $attributes) = @_;
    confess ("init: id not defined") unless (defined $id);
    confess ("init: tag not supplied") unless (defined $tag);
    confess ("init: attributes not supplied") unless ($attributes);
    confess ("init: attributes not a HASH ref") unless (ref $attributes eq 'HASH');
    # FIXME: use mutators
    $self->{ID} = $id;
    $self->{TAG} = $tag;
    $self->{ATTRIBUTES} = $attributes;
    return $self;
}

# die, ya bastard!
sub DESTROY {
    my $self = shift;
    %$self = ();
    $self = undef;
}

sub proto {
    my ($class, $controller, $template) = @_;
    # squashed bug: this has to be $class->new rather than new $class
    # otherwise SuperClass::new(Subclass) is called instead of Subclass:new
    # thus bypassing any modifications in the subclass ctor 
    my $self = $class->new;
    delete @{$template}{qw(re id class)};
    for my $key (keys %$template) {
	my $handler = sprintf 'handle_%s', $key;
	confess ("${class}::proto: no handler defined for '$key': "
		 . Dumper($template->{$key}))
	    unless ($handler = $self->can($handler));
	    $handler->($self, $template->{$key});
    }
    return $self;
}

sub handle_ignore {
    my ($self, $ignore) = @_;
    confess ("handle_ignore: invalid ignore: expected ARRAY ref, got '$ignore'")
	unless (ref $ignore eq 'ARRAY');
	
    # print STDERR "handle_ignore: ignore: ", Dumper($ignore), $/;

    for my $hash (@$ignore) {
        confess ("handle_ignore: invalid ignore: expected HASH ref, got '$ignore'")
	    unless (ref $hash eq 'HASH');
	for my $key (keys %$ignore) {
	    # print STDERR "key   => '$key'", $/;
	    # print STDERR "value => '", $hash->{$key}, "'", $/;
	    die ("handle_ignore: invalid key: expected 'value', got '$key'")
		unless ($key eq 'value');
	    $self->{IGNORE}->{$hash->{$key}} = 1;
	}
    }
    return $self;
}

sub handle_alias { # expects ARRAY of HASHes
    my ($self, $alias) = @_;
    
    confess ("handle_alias: invalid alias: expected ARRAY ref, got '$alias'")
	unless (ref $alias eq 'ARRAY');
    # print STDERR "handle_alias: alias: ", Dumper($alias), $/;

    for my $hash (@$alias) {
	confess ("handle_alias: invalid alias entry: expected HASH ref, got $hash")
	    unless (ref $hash eq 'HASH');
	confess ("handle_alias: no 'from' defined")
	    unless (exists $hash->{from});
	confess ("handle_alias: no 'to' defined")
	    unless ((exists $hash->{to}) || (exists $hash->{'#'}));
	$self->{ALIAS}->{$hash->{from}} = ltrim(rtrim(exists $hash->{to} ? $hash->{to} : $hash->{'#'}));
    }
    return $self;
}

sub handle_target {
    my ($self, $target) = @_;
    confess ("handle_target: target not defined")
	unless (defined $target);
    confess ("handle_target: invalid target: expected SCALAR, got $target")
	if (ref $target);
    $self->{TARGET} = $target;
    return $self;
}

sub handle_text {
    my ($self, $text) = @_;
    confess ("handle_text: text not defined")
	unless (defined $text);
    $self->text($text);
    return $self;
}

sub handle_strict {
    my ($self, $strict) = @_;
    confess ("handle_strict: strict setting not defined")
	unless (defined $strict);
    $self->strict($strict);
    return $self;
}

sub handle_hide {
    my ($self, $hide) = @_;
    confess ("handle_hide: hide not defined")
	unless (defined $hide);
    $self->hide($hide);
    return $self;
}

sub handle_show {
    my ($self, $show) = @_;
    confess ("handle_show: show not defined")
	unless (defined $show);
    $self->show($show);
    return $self;
}

sub handle_anonymous {
    my ($self, $anonymous) = @_;
    confess ("handle_anonymous: anonymity not defined")
	unless (defined $anonymous);
    confess ("handle_anonymous: invalid anonymity: expected SCALAR, got $anonymous")
	if (ref $anonymous);
    $self->{DO_NOT_ANONYMIZE} = 1 if ($anonymous =~ /0|false|no/);
    return $self;
}

sub parent { # read-only
    my ($self, $parent) = @_;
    # weaken(): the second most important optimization in the whole script
    # after the delete $self->{PARENT} line in clone()
    return $parent ? weaken ($self->{PARENT} = $parent) : $self->{PARENT};
}

sub tag { # accessor/mutator for XML tag
    my ($self, $tag) = @_;
    return $self->{TAG} unless (defined $tag);
    $self->{TAG} = $tag;
}

sub comment { # accessor/mutator for XML tag
    my ($self, $comment) = @_;
    return $self->{COMMENT} unless (defined $comment);
    $self->{COMMENT} = $comment;
}

sub children { # read-only
    my $self = shift;
    my $children = $self->{CHILDREN};
    return wantarray ? @$children : $children;
}

sub prefix {
    my ($self, $prefix) = @_;
    # (defined $prefix) ? $self->{PREFIX} = $prefix : "p'" . ($self->{PREFIX} || '') . "'";
    return (defined $prefix) ? $self->{PREFIX} = $prefix : $self->{PREFIX} || '';
}

sub suffix {
    my ($self, $suffix) = @_;
    # (defined $suffix) ? $self->{SUFFIX} = $suffix : "s'" . ($self->{SUFFIX} || '') . "'";
    return (defined $suffix) ? $self->{SUFFIX} = $suffix : $self->{SUFFIX} || '';
}

sub empty {
    my ($self, $empty) = @_;
    return (defined $empty) ? $self->{EMPTY} = $empty || 0 : $self->{EMPTY};
}

sub degenerate { # read-only
    my $self = shift;
    return (($self->kids == 0) && $self->empty) ? 1 : 0;
}

sub invisible { # read-only
    my $self = shift;
    return $self->show() ? 0 : 1;
}

sub visible { # read-only
    my $self = shift;
    return $self->show() ? 1 : 0;
}

sub always {
    my $self = shift;
    return 1;
}

sub never {
    my $self = shift;
    return 0;
}

sub content_only { # read only
    my $self = shift;
    my $comment = $self->comment();
    return (($comment && $self->is_clone()) || $self->text()) ? 1 : 0;
}

sub strict {
    my ($self, $strict) = @_;
    return ($self->{STRICT} ? 1 : 0) unless (defined $strict);
    $self->{STRICT} = $strict ? 1 : 0;
}

sub selected {
    my ($self, $selected) = @_;
    return ($self->{SELECTED} ? 1 : 0) unless (defined $selected);
    $self->{SELECTED} = $selected ? 1 : 0;
}

sub hide {
    my ($self, $hide) = @_;
    return $self->show($hide, 1);
}

# FIXME diagnostics
sub show {
    my ($self, $show, $negate) = @_;
    $negate = 0 unless (defined $negate);
    my $name = $negate ? 'hide' : 'show';
    my $handler;
    if (defined $show) {
	die ("$name (set): invalid $name predicate")
	    unless ($handler = $self->can($show));
	if ($negate) {
	    my $original_handler = $handler;
	    $handler = sub { my $obj = shift; $original_handler->($obj) ? 0 : 1 } 
	}
	$self->{SHOW}->{$show} = $handler;
    } else {
	for $show (keys %{$self->{SHOW}}) {
	    die ("show (get): invalid $name predicate")
		unless ($handler = $self->can($show));
	    return 0 unless ($handler->($self)); # don't show if a handler fails
	}
	return 1; # don't show
    }
}

sub attributes { # accessor/mutator for attributes HASH ref
    my $self = shift;
    my $attributes = $self->{ATTRIBUTES};
    my $args = $#_ + 1;

    if ($args == 0) { # no args
	return wantarray ? %$attributes : $attributes;
    } elsif ($args == 1) { # hashref
	$self->{ATTRIBUTES} = shift;
    } else { # hash
	$self->{ATTRIBUTES} = { @_ };
    }
}

# accessor/mutator for attribute key/value
sub attribute {
    my ($self, $key, $value) = @_;
    return $self->{ATTRIBUTES}->{$key} unless (defined $value);
    $self->{ATTRIBUTES}->{$key} = $value;
}

sub id { # Read only - unique template name
    my $self = shift;
    return $self->{ID};
}

sub content { # Process children
    my ($self, $force) = @_;
    return '' if ((not $force) && $self->invisible);
    my $children = $self->children;
    my $content = '';
    for my $child (@$children) {
	if (ref $child) {
	    my $prefix = ($child->{APPENDED}) ? $child->prefix() : '';  # activate header
	    my $suffix = ($child->{PREPENDED}) ? $child->suffix() : ''; # activate header
	    # $prefix = $suffix = '' if ($self->content_only);
	    $content .= $prefix . $child->xml() . $suffix;
	} else {
	    $content .= $child;
	}
    }
    return $content;
}

# Called by export() - attaches new template to previous
# template (if previous template exists),
# and returns the next template to be attached to.
# Note that the attach() method is called on the new clone
# with the optional target as its argument i.e.
# new->attach(old) rather than old->attach(new)

sub attach {
    my ($self, $target) = @_;
    $target->append($self) if ($target);
    return $self; # returns the new target for subsequent attachments
}

# Pulled out of add() to speed (typically text-bound) template parsing
sub pad {
    my ($self, $child) = @_;
    my $children = $self->children;

    if ((ref $children->[-1]) || ($#$children == -1)) {
	push (@$children, $child);
    } else {
	$children->[-1] .= $child;
    }

    return $self;
}

sub add { # Add child template to this parent
    my ($self, $child) = @_;
    return $self->pad($child) unless (ref $child);
    push (@{$self->children}, $child); # Template
    $self->register($child); # Apprises child of its parentage - returns $self
}

# As well as linking a child to its parent, this sub hoists the child's
# INDEX up into the parent - the upshot of which is that a template has
# direct access (via INDEX) to *all* of its children by name i.e. the
# functionality of a global template registry without the need for a
# separate factory or lookup table mechanism.
#
# See anonymize() for more information on why clones can't add
# prototype (i.e. non-clone) children: If they could, then an
# exhaustive traversal of *all* descendants would need to be performed
# both by register() and anonymize(). This restriction ensures that a
# parent can index and/or anonymize its descendants with a minimum of fuss.
# This is another "what's practical" v "what's theoretically complete"
# (bloated) trade-off.

sub register {
    my ($self, $child) = @_;

    die ("register: child is not a template") unless (ref $child);

    # Squashed mega-stupidity: this used to call clone() (twice)
    # instead of is_clone() - D'oh!
    die (sprintf "register: attempt to add a prototype (%s) to a clone (%s)",
	$child->id(), $self->id())
	if ($self->is_clone && (not $child->is_clone));

    # Apprise the object of its parentage and merge its index
    $child->parent($self);

    unless ($child->is_clone()) {
	my $self_id = $self->id();
	my $child_id = $child->id();

	if ($self->{INDEX}->{$child_id}) {
	    $Data::Dumper::Maxdepth = 2;
	    printf "PARENT: %s$/%s$/CHILD: %s$/%s$/",
		Dumper($self), $self->xml(), Dumper($child), $child->xml();
	    die ("$self_id: duplicate child id ($child_id)");
	}

	$self->{INDEX}->{$child_id} = $child;

	for my $key (keys %{$child->{INDEX}}) {
	    if ($self->{INDEX}->{$key}) {
		$Data::Dumper::Maxdepth = 2;
		printf "PARENT: %s$/%s$/CHILD: %s$/%s$/",
		    Dumper($self), $self->xml(), Dumper($child), $child->xml();
		die ("$self_id: duplicate grandchild id ($key)");
	    }
	    $self->{INDEX}->{$key} = $child->{INDEX}->{$key};
	}
    }

    return $self;
}

sub path {
    my $self = shift;
    my $parent = $self->parent();
    my $path;

    if ($parent) {
	$path = $parent->path . '/' . $self->id;
    } else {
	$path = '';
    }

    return $path;
}

sub root {
    my $self = shift;
    my $parent = $self->parent();
    return $parent ? $parent->root() : $self;
}

sub target {
    my ($self, $target) = @_;
    return $self->{TARGET} unless (defined $target);
    $self->{TARGET} = $target;
}

sub text {
    my ($self, $text) = @_;
    return $self->{TEXT} unless (defined $text);
    $self->{TEXT} = $text ? 1 : 0;
}

sub kids { # number of children
    my $self = shift;
    return $#{$self->children} + 1;
}

sub siblings { # number of fellow children
    my $self = shift;
    my $parent = $self->parent();
    return 0 unless $parent;
    return $parent->kids() - 1;
}

# set a flag on this template indicating that it has been written to
sub dirty {
    my ($self, $dirty) = @_;
    return ($self->{DIRTY} ? 1 : 0) unless (defined $dirty);
    $self->{DIRTY} = $dirty ? 1 : 0;
}

# propagate an operation up the template tree
sub cascade {
    my ($self, $fnc) = @_;
    $fnc->($self);
    my $parent = $self->parent();
    $parent->cascade($fnc) if ($parent);
    return $self;
}

# apply $fnc to all children
sub ripple {
    my ($self, $fnc) = @_;
    for my $child ($self->children) {
	$fnc->($child);
    }
    return $self;
}

# FIXME target="./0" caused a deep recursion warning last time I used it
sub get {
    local $_;
    my $self = shift;
    my ($child, $regex, $id);

    return $self unless (defined ($id = shift));

    my $map = $self->{MAP};
    # FIXME Jettisoned old case-insensitivity habit
    # Need to test that everything still works
    $id = $_ if (defined ($_ = $map->{$id}));

    # Template ID's pattern (\w+) also matches Indexed Child,
    # so look for the more specific one (Indexed Child) first
    die ("get: invalid path: " . $self->id . "[$id]") unless 
	($id =~ m/^
	 (?:(\d+)(?:=~(.+))?)	    | # Ordinal
	 (\w+)			    | # Path
	 (?:\@([^=]+)(?:=~(.+))?)   | # Attribute
	 (?:\/(\w+))		    | # Absolute Path
	 (?:(\#)(?:=~(.+))?)	    | # Tag
	 (\.)			    | # Self
	 (\.\.)			      # Parent
	 $/x);

    if (defined $1) { # Ordinal (Template or Text)
	($id, $regex) = ($1, $2);
	my $children = $self->children;
	if ($self->strict) {
	    return unless (exists $children->[$id]);
	    # return unless (($id >= 0) && ($id <= $#$children));
	}
	# FIXME Watch out for autovivification - particularly
	# with Sequencer which is designed to forbid text children 
	$child = \($children->[$id]);
	return $$child if (ref $$child);
    } elsif (defined $3) { # Path
	# FIXME Need to test that everything still works now that
	# case-insensitivity has been jettisoned
	return unless ($child = $self->{INDEX}->{$id});
	# Squashed bug: scalar @_ not @_
	return (scalar @_) ? $child->get(@_) : $child;
    } elsif (defined $4) { # Attribute
	($id, $regex) = ($4, $5);
	if ($self->strict) {
	    return unless (exists $self->{ATTRIBUTES}->{$id});
	    # return unless (($id >= 0) && ($id <= $#$children));
	}
	$child = \($self->{ATTRIBUTES}->{$id});
    } elsif (defined $6) { # Absolute Path
	# FIXME check this works with '/'
	my $root = $self->root();
	return (($id eq '/') ? $root->get(@_) : $root->get($6, @_));
    } elsif (defined $7) { # Tag
	($id, $regex) = ($7, $8);
	$child = \($self->{TAG});
    } elsif (defined $9) { # Self
	return $self;
    } else { # Parent
	return $self->parent();
    }

    # We will have returned already if $child is a template, so this
    # regex match will always be sane
    my @match = ();
    if ($child && (not(defined $regex) || scalar(@match = $$child =~ /$regex/))) {
	return wantarray ? ($child, $regex, [ @match ]) : $$child;
    } # else return undef
}

# FIXME What happens if you try to overwrite a template with a string?
sub set {
    my $self = shift; # Don't amalgamate these 2 lines
    my ($target, $value, $regex) = @_;

    # set() can be called as a low-level mutator for the scalar reference
    # or as a high-level accessor/mutator like get()
    # thus:
    #	    set($target, $value, $regex)
    #   or
    #	    set('/', 'some', 'random', 'path', '@target=~$', 'value')
    #	    i.e. set(@path, $value)

    unless ((ref $target) eq 'SCALAR') { # Direct access to scalar reference
	($target, $regex) = $self->get($target);
	# return $self to facilitate chaining e.g. $foo->set(@whatever)->bar();
	return $self unless ($target);
	my $candidate;

	$target = $candidate while 
	    (
		((ref $target) ne 'SCALAR') && 
		(($candidate, $regex) = $target->get($target->target)) && 
		($candidate ne $target) # Nip infinity in the bud
	    );

	unless ((ref $target) eq 'SCALAR') {
	    die (sprintf("set: %s['%s'] does not target a scalar",
			$self->id(), join('/', @_)));
	}
    }

    $value = '' unless (defined $value);
    # propagate dirtiness up the tree to the root if $value is nonempty
    $self->cascade(sub { $_[0]->dirty(1); }) unless ($value eq '');
    my $ref = ref $value;
    die ("set: unexpected reference value assignment: $ref") if ($ref);

    if (defined $regex) {
	# FIXME /g ? 
	# print "REPLACING: $$target =~ s/$regex/$value/g", $/;
	$$target =~ s/$regex/$value/g;
    } else {
	$$target = $value; 
    }

    return $self;
}

# This feature (clones should not clone the id attribute, which the
# specs and common sense require to be unique) was one of the original
# motivations for rewriting Enhydra's XMLC in perl (along with the fact
# that altering templates required painful recompilation). Anonymization
# could be done on-demand by registering the ur-templates
# in a static helper hash (like the %split and %children hashes used
# by export()) - templates not registered there would thus be identified
# as clones. However, anonymize() is more in keeping with this module's
# philosphy of eschewing global state, and as a one-off 'compile-time' hack
# is probably faster / more scalable.
#
# A variety of optimizations are enabled if clones are prohibited
# (as they are in register()) from add()ing prototypes (i.e. non-clones).
# Conversely, a great deal of complexity is added if clones *are* allowed
# to add prototypes.
#
# In particular, this restriction means that:
#
# 1) cloning a clone doesn't require further anonymization. It's guaranteed
#    to be anonymous all the way down to the leaves.
#    Without this prohibition, in contrast, anonymize() is more or less
#    guaranteed to be *non-optimal* - how to tell whether a template,
#    clone or not, contains prototype descendants, and if so how to reach
#    them quickly?
#   
# 2) registration (i.e. maintaining a global index of templates without
#    using a separate factory/registry) is painless if we can skip it
#    for clones, but again requires some kind of difficult/impossible
#    to optimize search of descendants if clones can have prototype
#    children/grandchildren.
#
# The key point is that INDEX points to prototypes unless/until a template
# is cloned in which case we really don't know what it points to, and have
# to waste cycles finding out. That is not the case if all descendants of
# a clone are *guaranteed* to be clones themselves.

sub anonymize {
    my ($self, $inner) = @_;
    $inner = -1 unless ($inner);
    return $self if ($self->is_clone()); # See above
    delete $self->{ATTRIBUTES}->{id} unless ($self->{DO_NOT_ANONYMIZE});
    # took out the feeble attempt at unique clone identification
    # (by assigning sequential numbers to the clone field),
    # as export() uses the lightweight (i.e. fast) Acme::Util::clone() - which
    # doesn't re-uniquify the new clone or its descendants - for the bulk
    # of its clonage; in addition, if unique naming was ever seriously
    # needed (it was only ever used for debug purposes), it would be much
    # better handled on-demand (see above)

    for my $descendent (values %{$self->{INDEX}}) { # also see above
	$descendent->anonymize($inner - 1);
    }

    $self->is_clone($inner); # inner clone
    $self->dirty(0);

    return $self;
}

sub is_clone { # get / set CLONE flag of object
    my ($self, $value) = @_;
    return (defined $value) ? $self->{CLONE} = $value : $self->{CLONE} || 0;
}

# FIXME This *used to* benchmark faster if PARENT was passed
# as a "don't copy" param to clone() rather than deleted/restored
# each time. The same may still be true, and something similar
# may or not speed up copy()
sub clone {
    my $self = shift;

    # print STDERR "inside clone ctor for $self: ", $self->id(), $/;
    # The following harmless-looking line is probably the most important
    # optimization in the whole framework.
    # Take out the parent before cloning, otherwise the whole tree ends up being cloned!

    # my $clone = Acme::Util::clone($self, [ $self->{PARENT} ]);
    my $parent = delete $self->{PARENT};
    my $clone = Acme::Util::clone($self);
    weaken ($self->{PARENT} = $parent);
    confess ("post-clone parent link is no longer weak: " . Dumper($self))
	unless (isweak $self->{PARENT});
    # The following 2 lines may bench faster than the previous 2,
    # but need to be re-tested for correctness and speed: 

    # my $clone = Acme::Util::clone($self, [ $self->{PARENT} ]);
    # delete $clone->{PARENT};

    $clone->anonymize(); # returns $clone
}

# shallow version of clone() - reproduces the raw template without children
# piggybacks off clone() to avoid (squashed) bugs and redundancy
sub copy {
    my $self = shift;
    my ($children, $index) = delete @{$self}{qw(CHILDREN INDEX)};
    $self->{CHILDREN} = [];
    $self->{INDEX} = {};
    # deals with PARENT optimization, deletion of id attribute and setting CLONE to true
    my $copy = $self->clone();
    @{$self}{qw(CHILDREN INDEX)} = ($children, $index);
    return $copy;
}

sub append {
    my ($self, $sibling, $parent) = @_;
    unless ($parent ||= $self->parent()) {
	die sprintf("append: can't append to orphan template: %s",
	    $self->id); 
    }
    $sibling->{APPENDED} = 1;
    $parent->insert($sibling, $self->position($parent) + 1);
    return $self;
}

sub prepend { 
    my ($self, $sibling, $parent) = @_;
    unless ($parent ||= $self->parent()) {
	die sprintf("prepend: can't prepend to orphan template: %s",
	    $self->id);
    }
    $sibling->{PREPENDED} = 1;
    $parent->insert($sibling, $self->position($parent));
    return $self;
}

sub insert {
    my ($self, $child, $position) = @_;
    $child->parent($self) if (ref $child);
    # we're not replacing, so the number of elements to be removed (LENGTH) is 0
    splice(@{$self->children}, $position, 0, $child);
    $self->register($child);
    return $self;
}

sub position { # Find the location of self in siblings array
    my $self = shift;
    my $parent = shift || $self->parent(); # parent is optional
    my $candidate;
    
    die sprintf("position: can't find index for orphan template: %s", $self->id)
	unless ($parent);

    my $siblings = $parent->children();
    my $position = $#$siblings;

    --$position while
	(
	    ($position >= 0) &&
	    (defined ($candidate = $siblings->[$position])) &&
	    ((not(ref $candidate)) || ($candidate ne $self))
	);

    confess sprintf("position: can't find index of '%s' in '%s'", $self->id, $parent->id) 
	if ($position < 0);

    return $position;
}

sub xml {
    my ($self, $force) = @_;
    my $tag = $self->tag();
    my $comment = $self->comment();
    # confess ("invalid template: " . Dumper ($self)) unless ($self->{ATTRIBUTES});
    my %attributes = $self->attributes();
    my ($start, $end, $content);

    if ((not $force) && $self->invisible) {
	$start = $end = $content = '';
    } elsif ((not $force) && $self->content_only) {
	# cloned comment, or text="1": hide tags and prefix/suffix
	$start = $end = ''; # hide start tag and end tag
    } else {
	local $_;
	my $attributes = join '', map qq[ $_="$attributes{$_}"], keys %attributes;
	my $degenerate = $self->degenerate();

	if ($comment) { # prototype comment: show
	    if (lc($comment) eq 'dreamweaver') { # Dreamweaver-style
		$start = sprintf '<!-- #BeginEditable "%s" -->', $self->id;
		$end = "<!-- #EndEditable -->"; 
	    } else { # Xelig-style
		# FIXME What about embedded double-quotes?
		if ($degenerate) {
		    ($start, $end, $content) = ("<!-- <%s%s", " /> -->", '');
		} else {
		    ($start, $end) = ("<!-- <%s%s> -->", "<!-- </%s> -->");
		}
	    }
	} else {
	    # FIXME Need low-level (i.e. fast)
	    # escape for possible embedded double quotes
	    if ($degenerate) {
		($start, $end, $content) = ("<%s%s", " />", '');
	    } else {
		($start, $end) = ("<%s%s>", "</%s>");
	    }
	}

	# extra args are ignored unless the format requires them 
	$start = sprintf $start, $tag, $attributes;
	$end = sprintf $end, $tag;
    }

    $content = $self->content($force) unless (defined $content);
    return wantarray ? ($start, $content, $end) : $start . $content . $end;
}

{
    my (%split, %children);

    sub export {
	local $_;
	my ($self, $model) = @_;
	my ($last, $clone, $prototype);
	my $ref_model = ref $model;

	if ($ref_model eq 'HASH') { # HASH
	    $model = [ $model ];
	} elsif ($ref_model ne 'ARRAY') { # SCALAR or OBJECT - handle UNDEF in set()
	    $model = [ { $self->target() => $model } ];
	}

	# Squashed megabug: clone() the virgin template before it's filled with data
	# one clone *HAS* to be wasted: don't try to recycle it (as the last appendix)
	# or the original bug *WILL* come back and bite mucho ass!
	
	my $count = $#$model + 1;

	$prototype = $self->clone() if ($#$model > 0);

	for my $resource (@$model) {
	    # First update the original template, thereafter update the clones

	    if ($last) {
		$clone = Acme::Util::clone($prototype);
		# print STDERR "Acme::Util::clone ctor: $clone", $/;
	    } else {
		$clone = $self;
	    }

	    # $clone = $last ? Acme::Util::clone($prototype) : $self;

	    # $clone->target() could be pulled out of this loop if we assume
	    # that the prototype and all its clones share the same target
	    # but it's plausible that an implementation may wish to assign
	    # a new target during clone(), so we don't risk it
	    # FIXME True?

	    unless ((ref $resource) eq 'HASH') {
		# FIXME This mutates the original!
		$resource = { $clone->target() => $resource }; 
	    }

	    # while (($path, $value) = each %$resource)
	    # FIXME Why did while/each here cause redundant iterations?

	    for my $paths (keys %$resource) {
		my $value = $resource->{$paths};
		# FIXME should this be inside the $path loop below?
		my $seen = {};

		# regex rather than split() so that we can ignore
		# leading or trailing whitespace
		my (@paths) = @{($split{$paths} ||= [ $paths =~ /(\S+)/g ])};
		for my $path (@paths) {
		    my ($alias, $ignore) = @{$self}{qw(ALIAS IGNORE)};
		    if (defined (my $aliased = $alias->{$path})) {
			if (exists $seen->{$path}) {
			    if ($self->strict) {
				# FIXME cycles could be detected at
				# compile-time. But what about
				# hand-rolled views?
				die ("export$/" .
				     $clone->id() .
				     "['$path'] alias cycle detected");
			    } # else don't alias
			} else {
			    $seen->{$path} = $aliased;
			    push (@paths, split(/\s+/, $aliased));
			    next;
			}
		    }
		    # FIXME ignore could be implemented by aliasing to ""
		    # alias before ignore
		    next if ($ignore->{$path});
		    # $path=~s/\//$;/g if ($path =~ /=~/);
		    # Split along non-backslashed and 
		    # non-line-beginning forward slashes
		    my @children = @{($children{$path} ||= 
			[ split (m{(?<=[^\\])/}, $path) ])};

		    my ($target, $regex, $match) = $clone->get(@children);
		    # Bail out unless the target pointed to by
		    # the path is found. 
		    # No need to do defined($target)
		    # as get() always returns a ref or undef
		    unless ($target) {
			if ($self->strict) {
			    print STDERR "strict: ", $self->strict(), $/;
			    print STDERR "id: ", $self->id(), $/;
			    # $Data::Dumper::Maxdepth = 2;
			    print $clone->xml(), $/;
			    # print Dumper($clone), $/;
			    die "can't find " . $clone->id() .
				"['$path'] while exporting to '" .
				$self->id() . "': " .
				 $/ . Dumper($resource);
			} else {
			    next;
			}
		    }

		    if ((ref $target) eq 'SCALAR') {
			# handles SCALAR, OBJECT or UNDEF values
			$clone->set($target, $value, $regex);
			# get() should always return a reference
			# (to a template or string) or undef when
			# called in array context, so we'll
			# know about it pretty soon if a bug
			# returns a non-template
			# So there's no need for fastidious e.g.
			#   elsif (ref $target) { ...} else { die ... }
			# error-handling here
		    } else {
			# Target must be a template, so recurse
			$target->export($value);
		    }
		}
	    }

	    $last = $clone->attach($last);
	}

	return $self;
    }
}

1;

############################################################

package Xelig::Sequencer;

use strict;
use warnings;
use Carp qw(confess);
use base qw(Xelig::Template);
use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;

sub new {
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    # $self->strict(1);
    return $self;
}

sub pad { # Overrides the base implementation
    my ($self, $text) = @_;
    return $self; # don't pad
}

sub add {
    my ($self, $child) = @_;
    return $self unless (ref $child);
    $self->SUPER::add($child);
    $child->{APPENDED} = 1; # force appendix status
    # $child->{ATTRIBUTES}->{seq_appendix} = $child->{APPENDED};
}

# Sequencer flags every child as an appendix, which is fine for the prefix
# (that's handled by SUPER::content), but misses the suffix of the last item:
# that is supplied here

sub content {
    my ($self, $force) = @_;
    my $content = $self->SUPER::content($force);
    return $content unless $self->kids();
    my $children = $self->children;
    my $first = $children->[0];
    my $last = $children->[-1];
    return $content . ($self->content_only() ? '' : $last->suffix());
}

1;

############################################################

package Xelig::Default;

use strict;
use warnings;
use Carp qw(confess);
use base qw(Xelig::Template);
use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;

sub proto {
    my ($class, $controller, $template) = @_;
    my $self = new $class->SUPER::proto($controller, $template);

    die ("${class}::proto: compulsory attribute (default) not supplied for template in "
	. $controller->path()) unless (defined $self->default);
    return $self;
}

sub handle_default {
    my ($self, $default) = @_;
    if (defined $default) {
	$self->default($default);
    } else {
	confess "'default' parameter is not defined";
    }
}

sub default {
    my ($self, $default) = @_;
    return $self->{DEFAULT} unless (defined $default);
    $self->{DEFAULT} = $default;
}

sub xml {
    my ($self, $force) = @_;
    return $self->SUPER::xml($force) if ($force || $self->visible);
    my $id = $self->default();
    my $default = $self->get($id);
    unless (defined $default) {
	print STDERR Dumper($self), $/;
	die "default template ('$id') not found"; 
    }
    return $default->xml(1);
}

sub content {
    my ($self, $force) = @_;
    return $self->SUPER::content($force) if ($force || $self->visible);
    my $id = $self->default();
    my $default = $self->get($id);
    confess "default template ('$id') not found" unless ($default);
    return $default->content(1);
}

1;

#############################################################

# Coerce all children to one text child
# FIXME need to override insert()
package Xelig::Editor;

use strict;
use warnings;

use base qw(Xelig::Template);

sub pad { 
    my ($self, $cdata) = @_;
    $self->children->[0] .= $cdata;
    return $self;
}

sub register { return $_[0]; } # not implemented
sub insert { return $_[0]; } # not implemented

sub add {
    my ($self, $child) = @_;
    if (ref $child) {
	return $self->pad($child->xml);
    } else {
	return $self->pad($child);
    }
}

1;
