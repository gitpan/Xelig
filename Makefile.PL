use 5.008_000;
use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'          => 'Xelig',
    'VERSION_FROM'	=> 'lib/Xelig.pm',
    'INSTALLDIRS'   => 'perl',
	'AUTHOR'		=> 'chocolateboy: <chocolate.boy@email.com>',
    'PREREQ_PM'		=> {
		# for clone() (requires recent perl to clone weakrefs)
		'Acme::Util'    => 0.09, 
		# for weakrefs
		'Scalar::Util'	=> 0,
		# to parse XML Views
		'XML::Parser'	=> 0,
		# this is the earliest (non-beta) version to support XML::Simple::PREFERRED_PARSER
		'XML::Simple'	=> 1.08
	}
);
