#!perl -w

# simple test script for Xelig

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

use strict;
use Test::More tests => 3;

BEGIN {
    chdir 't' if -d 't';
    unshift @INC, '../lib';
    use_ok ('Xelig', 'MVC');
    use_ok ('Acme::Util', 'readfile');
}

my $model = {
    foo => [
	{
	    alpha   => 'beta',
	    beta    => 'band',
	    gamma   => 'vlissides',
	    delta   => 'blues'
	},
	{
	    alpha   => 'bravo',
	    beta    => 'blockers',
	    gamma   => 'delta',
	    delta   => 'echo'
	},
    ],
    bar => 'baz' 
};

my $view = 'simple/view.xml';
my $template = MVC($model, $view); # default controller
my $want = readfile 'simple/out.xml';
my $got = $template->content();

is ($got, $want, 'simple export with clone, default controller');
