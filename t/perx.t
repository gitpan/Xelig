#!perl -w

# a slightly more 'real world' test
#
# Takes an RSS file and maps it into an HTML template to render
# Per Dahlberg's Fixed Font Headlines applet:
# http://www.dahlberg.se/java/FixFontHeadlines/

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

use strict;
use Test::More tests => 4;
use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;

BEGIN {
    chdir 't' if -d 't';
    unshift @INC, '../lib';
    use_ok ('Xelig', 'MVC');
    use_ok ('Acme::Util', 'readfile');
    use_ok ('XML::Simple');
}

my $model = XMLin('perx/model.rss');
my $view = readfile 'perx/view.html';
my $controller = 'perx/controller.xml';
my $want = readfile 'perx/out.xml';
my $index = 0;

$_->{'index'} = ++$index for (@{$model->{item}});
$model->{'count'} = $index;

my $template = MVC($model, $view, $controller);
my $got = $template->content();

is($got, $want, "perx: RSS to Java Ticker");
