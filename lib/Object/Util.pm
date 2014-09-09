use 5.006;
use strict;
use warnings;

BEGIN { if ($] < 5.010000) { require UNIVERSAL::DOES } };

package Object::Util;

our $AUTHORITY = 'cpan:TOBYINK';
our $VERSION   = '0.001';

use B                            qw( perlstring );
use Carp                         qw( croak );
use List::Util       1.29        qw( pairkeys pairvalues );
use Module::Runtime              qw( use_module use_package_optimistically );
use MooX::Traits::Util;
use Role::Tiny;
use Scalar::Util     1.23        qw( blessed reftype );

my $anon_class_id = 0;

{
	my %op = (
		HASH    => '%{}',
		ARRAY   => '@{}',
		SCALAR  => '${}',
		CODE    => '&{}',
		GLOB    => '*{}',
		Regexp  => 'qr',
	);
	
	sub _is_reftype
	{
		my $object = shift;
		my $type   = $_[0];
		
		return !!1 if (reftype($object)||'') eq $type;
		return !!0 if not $INC{'overload.pm'};
		
		my $op = $op{$type} or return !!0;
		!!overload::Method($object, $op);
	}
}

sub _new :method
{
	my $class = shift;
	my $ref   = reftype($class);
	
	if ($ref)
	{
		croak "Invocant is not a coderef"
			unless _is_reftype($class, 'CODE');
	}
	else
	{
		use_package_optimistically($class);
		croak "Class $class does not provide a constructor called 'new'"
			unless $class->can("new");
	}
	
	$ref ? $class->(@_) : $class->new(@_);
}

sub _call_if_object :method
{
	my $object = shift;
	my $method = shift;
	return unless blessed($object);
	$object->$method(@_);
}

for my $method (qw/ isa can does DOES /)
{
	eval qq{
		sub _${method} :method
		{
			return unless Scalar::Util::blessed(\$_[0]);
			shift->${method}(\@_);
		}
		1;
	} or die "Internal problem: $@";
}

sub _try :method
{
	my $object = shift;
	my $method = shift;
	eval { $object->$method(@_) };
}

sub _tap :method
{
	my $object = shift;
	my $method = shift;
	$object->$method(@_);
	$object;
}

my %toolage;

sub _detect_metaclass
{
	my $class = shift;
	
	if ($INC{'Moose.pm'})
	{
		require Moose::Util;
		return "Moose" if Moose::Util::find_meta($class);
	}
	
	if ($INC{'Mouse.pm'})
	{
		require Mouse::Util;
		return "Mouse" if Mouse::Util::find_meta($class);
	}
	
	if ($INC{"Moo.pm"})
	{
		return "Moo" if $Moo::MAKERS{$class}{is_class};
	}
	
	"Other";
}

sub _clone :method
{
	my $object = shift;
	my $class  = blessed($object);
	
	croak "Cannot call \$_clone on non-object"
		unless $class;
	
	return $object->clone(@_)
		if $object->can("clone");
	
	my $tool = ($toolage{$class} ||= _detect_metaclass($class));
	
	if ($tool eq "Moose")
	{
		require Moose::Util;
		my $meta = Moose::Util::find_meta($class);
		return $meta->clone_object($object, @_);
	}
	
	if ($tool eq "Mouse")
	{
		require Mouse::Util;
		my $meta = Mouse::Util::find_meta($class);
		return $meta->clone_object($object, @_);
	}
	
	croak "Object does not provide a 'clone' method, and is not a hashref"
		unless _is_reftype($object, 'HASH');
	
	ref($object)->Object::Util::_new({ %$object, @_ });
}

sub _with_traits :method
{
	my $class = shift;
	
	croak "Cannot call \$_with_roles on reference"
		if ref $class;
	
	return $class unless @_;
	
	my $tool = ($toolage{$class} ||= _detect_metaclass($class));
	
	if ($tool eq 'Moose')
	{
		require Moose::Util;
		require MooX::Traits::Util;
		
		my @traits = MooX::Traits::Util::resolve_traits($class, @_);
		return Moose::Util::with_traits($class, @traits);
	}
	
	if ($tool eq 'Mouse')
	{
		require Mouse::Util;
		require MooX::Traits::Util;
		
		my @traits = MooX::Traits::Util::resolve_traits($class, @_);
		
		my $meta = ref(Mouse::Util::find_meta($class))->create(
			sprintf('%s::__ANON__::%s', __PACKAGE__, ++$anon_class_id),
			superclasses => [ $class ],
			roles        => \@traits,
			cache        => 1,
		);
		return $meta->name;
	}
	
	require Role::Tiny;
	require MooX::Traits::Util;
	MooX::Traits::Util::new_class_with_traits($class, @_);
}

sub _dump :method
{
	require Data::Dumper;
	local $Data::Dumper::Terse = 1;
	local $Data::Dumper::Indent = 1;
	local $Data::Dumper::Useqq = 1;
	local $Data::Dumper::Deparse = 1;
	local $Data::Dumper::Quotekeys = 0;
	local $Data::Dumper::Sortkeys = 1;
	Data::Dumper::Dumper($_[0]);
}

{
	my %_cache;
	sub _eigenclass
	{
		my $class = shift;
		my ($roles, $methods) = @_;
		
		my @traits;
		if ($roles and @$roles)
		{
			require MooX::Traits::Util;
			@traits = MooX::Traits::Util::resolve_traits($class, @$roles);
		}
		
		my $key = do
		{
			no warnings qw(once);
			require Storable;
			local $Storable::Deparse   = 1;
			local $Storable::canonical = 1;
			Storable::freeze( [$class, \@traits, $methods] );
		};
		
		my $eigenclass = $_cache{$key};
		unless ($eigenclass)
		{
			no strict qw(refs);
			$_cache{$key}
				= $eigenclass
				= sprintf('%s::__ANON__::%s', __PACKAGE__, ++$anon_class_id);
			*{"$eigenclass\::ISA"} = [ _with_traits($class, @traits) ];
			*{"$eigenclass\::$_"}  = $methods->{$_} for keys %$methods;
		}
		
		$eigenclass;
	}
}

sub _extend :method
{
	my $object = shift;
	my $class  = blessed($object)
		or croak("Cannot call \$_extend on non-object");
	
	my $roles   = _is_reftype($_[0], "ARRAY") ? shift : [];
	my $methods = _is_reftype($_[0], "HASH")  ? shift : {@_};
	
	return $object unless @$roles || keys(%$methods);
	bless $object, _eigenclass($class, $roles, $methods);
}

sub subs :method
{
	'$_new'             => \&_new,
	'$_isa'             => \&_isa,
	'$_can'             => \&_can,
	'$_does'            => \&_does,
	'$_DOES'            => \&_DOES,
	'$_call_if_object'  => \&_call_if_object,
	'$_tap'             => \&_tap,
	'$_try'             => \&_try,
	'$_clone'           => \&_clone,
	'$_with_traits'     => \&_with_traits,
	'$_dump'            => \&_dump,
	'$_extend'          => \&_extend,
}

sub sub_names :method
{
	my $me = shift;
	pairkeys($me->subs);
}

sub setup_for :method
{
	my $me   = shift;
	my @refs = @_;
	my @subs = pairvalues($me->subs);
	
	while (@refs)
	{
		my $ref = shift(@refs);
		my $sub = shift(@subs);
		die "Internal problem" unless _is_reftype($sub, 'CODE');
		
		$$ref = $sub;
		&Internals::SvREADONLY($ref, 1) if exists(&Internals::SvREADONLY);
	}
	
	die "Internal problem" if @subs;
	return;
}

sub import :method
{
	my $me = shift;
	my ($caller, $file) = caller;
	
	if ($file ne '-e' and eval { require B::Hooks::Parser })
	{
		my $varlist = join ',', $me->sub_names;
		my $reflist = join ',', map "\\$_", $me->sub_names;
		B::Hooks::Parser::inject(";my($varlist);$me\->setup_for($reflist);");
		return;
	}
	
	my %subs = $me->subs;
	for my $sub_name (sort keys %subs)
	{
		my $code = $subs{$sub_name};
		$sub_name =~ s/^.//;
		no strict 'refs';
		*{"$caller\::$sub_name"} = \$code;
	}
}

1;

__END__

=pod

=encoding utf-8

=for stopwords metaobject

=head1 NAME

Object::Util - a selection of utility methods that can be called on blessed objects

=head1 SYNOPSIS

   use Object::Util;
   
   # $foo might be undef, but this should not die
   if ($foo->$_isa("Bar")) {
      ...;
   }

=head1 DESCRIPTION

This module is inspired by L<Safe::Isa>, L<Object::Tap>, and my own
OO experiences. It is a hopefully helpful set of methods for working
with objects, exposed as lexical coderef variables.

=head2 Rationale

Providing methods as coderefs so that you can do:

   $object->$_foo(@args)

... is unusual, so probably requires some explanation.

Firstly some of these methods are designed to be called on either a
blessed object or some kind of unblessed reference or value. Calling a
method on an unblessed reference like this will croak:

   $ref->foo(@args)

Ditto calling methods on undef. Coderefs don't suffer from that
problem.

More importantly though, the aim of this module is that these methods
should be available for you to call on I<any> object. You can only
call C<< $object->foo(@args) >> if C<< $object >>'s class implements a
method called C<foo>, or inherits from a superclass that does. Coderef
methods can be called on any object.

This module adopts the C<< $_foo >> naming convention pioneered by
modules such as Safe::Isa. However (unlike Safe::Isa) the coderefs it
provides are I<< true lexical variables >> (a.k.a. C<my> variables),
not package variables (a.k.a. C<our> variables).

=head2 Methods

=over

=item C<< $_new >>

Can be used as C<< $factory->$_new(@args) >> to create a new object.

C<< $factory >> may be a class name (the module will be auto-loaded),
or may be a coderef or object overloading C<< &{} >>. If it's a class
name, then C<< $class->new(@args) >> will be called; otherwise
C<< $coderef->(@args) >> will be called.

=item C<< $_isa >>

C<< $object->$_isa($class) >> works like C<isa> as defined in
L<UNIVERSAL>, but returns C<undef> if C<< $object >> is undefined.

Same as L<Safe::Isa>.

=item C<< $_can >>

C<< $object->$_can($class) >> works like C<can> as defined in
L<UNIVERSAL>, but returns C<undef> if C<< $object >> is undefined.

Same as L<Safe::Isa>.

=item C<< $_does >>

C<< $object->$_does($class) >> works like C<does> as defined in
L<Moose::Object>, but returns C<undef> if C<< $object >> is undefined.

Same as L<Safe::Isa>.

=item C<< $_DOES >>

C<< $object->$_DOES($class) >> works like C<DOES> as defined in
L<UNIVERSAL>, but returns C<undef> if C<< $object >> is undefined.

Same as L<Safe::Isa>.

=item C<< $_call_if_object >>

C<< $object->$_call_if_object($method => @args) >> works like
C<< $object->$method(@args) >>, but returns C<undef> if C<< $object >>
is undefined.

Same as L<Safe::Isa>.

=item C<< $_try >>

C<< $object->$_try($method => @args) >> works like
C<< $object->$method(@args) >>, but returns C<undef> if any exception
is thrown.

=item C<< $_tap >>

C<< $object->$_tap($method => @args) >> works like
C<< $object->$method(@args) >>, but returns the object itself, making
it useful for chaining.

Same as L<Object::Tap>, or the C<tap> method in Ruby.

=item C<< $_clone >>

If the object provides a C<clone> method, calls that. Or if the object
appears to be Moose- or Mouse-based, clones it using the metaobject
protocol.

Otherwise takes the naive approach of treating the object as a hashref
or attribute values, and creates a new object of the same class.

   # clone overrides some attributes from the original object
   my $glenda = $glen->$_clone(name => "Glenda", gender => "f");

That final fallback obviously massively breaks your class'
encapsulation, so it should be used sparingly.

=item C<< $_with_traits >>

Calling C<< $class->$_with_traits(@traits) >> will return a new class
name that does some extra traits. Should roughly support L<Moose>,
L<Moo>, and C<Role::Tiny>.

=item C<< $_extend >>

Calling C<< $object->$_extend(\@traits, \%methods) >> will add some
extra roles and/or methods to an existing object.

Like L<Object::Extend>, but with added support for roles.

=item C<< $_dump >>

Calling C<< $object->$_dump >> returns a L<Data::Dumper> dump of the
object, with some useful changes to the default Data::Dumper output.
(Same as L<Data::Dumper::Concise>.)

=back

=head2 Implementation Details

L<B::Hooks::Parser> is used to inject these methods into your
lexical scope, and C<Internals::SvREADONLY> (an internal function
built into the Perl core) is used to make them read-only, so you
can't do:

   use Object::Util;
   $_isa = sub { "something else" };

If this module detects that B::Hooks::Parser cannot be used on your
version of Perl, or your Perl is too old to have Internals::SvREADONLY,
then it has various fallback routes, but the variables it provides may
end up as package (C<our>) variables, or not be read-only.

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Object-Util>.

=head1 SEE ALSO

L<Safe::Isa>, L<UNIVERSAL>, L<Object::Tap>, L<MooseX::Clone>,
L<Data::Dumper::Concise>, L<Object::Extend>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2014 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

