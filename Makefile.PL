use 5.008_000;
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Xelig',
    'VERSION_FROM'	=> 'lib/Xelig.pm', # finds $VERSION
    'PREREQ_PM'		=> {
	'Util'		=> 0, # for clone (requires recent perl to clone weakrefs)
	'Scalar::Util'	=> 0, # for weakrefs
	'XML::Parser'	=> 0, # to parse View XML
	'XML::Simple'	=> 0  # to parse Controller XML
    }, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (
      # ABSTRACT_FROM => 'Object.pm', # retrieve abstract from module
       AUTHOR     => 'chocolateboy: <chocolate.boy@email.com>') : ()),
);
