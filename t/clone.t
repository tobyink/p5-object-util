=pod

=encoding utf-8

=head1 PURPOSE

Tests for C<< $_clone >>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

use strict;
use warnings;
use Test::More;

use Object::Util;
use Scalar::Util qw(refaddr);

sub TestClass::new {
	my $class = shift;
	bless +{ @_==1 ? %{$_[0]} : @_ }, $class
}

my $test = bless({ foo => [1..3], bar => 666, baz => 999 }, 'TestClass');
my $clone1 = $test->$_clone();
my $clone2 = $test->$_clone(baz => 42);

is_deeply(
	$clone1,
	$test,
	'clone with no args',
);

is_deeply(
	$clone2,
	bless({ foo => [1..3], bar => 666, baz => 42 }, 'TestClass'),
	'clone with args',
);

is(
	refaddr($test->{foo}),
	refaddr($clone1->{foo}),
	'clone is shallow',
);

diag('not tested: Moose objects');
diag('not tested: objects that provide their own clone method');
diag('not tested: exceptions');

done_testing;
