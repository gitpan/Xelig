#!perl -w

use strict;
use warnings;
use Test::More tests => 3;
use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;

# 2003-06-23: This test is to designed to prevent a regression.
# spaces before or after <alias> content </alias> should be ignored
# Version 0.02 (and possibly earlier versions) were corrupting the
# output if there were initial or trailing spaces

BEGIN {
    chdir 't' if -d 't';
    unshift @INC, '../lib';
    use_ok ('Xelig', 'MVC');
    use_ok ('Acme::Util', qw(readfile));
}

my $model = { email => 'foo@bar.com' };
my $view = 'space_alias/view.xml';
my $controller = 'space_alias/controller.xml';
my $template = MVC($model, $view, $controller);
my $want = readfile 'space_alias/out.xml';
my $got = $template->content();

is ($got, $want, 'simple test to cover regression');
