#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say postderef/;
use List::Util qw/reduce/;
use Data::Dumper;

package Unit;
use Moose;
use List::Util qw/any/;
use integer;
use feature qw/state/;

has group => (is => 'ro', isa => 'Int', required => 1,
              default => sub { state $id = 0; return ++$id; });
has 'side' => (is => 'ro', isa => 'Str');
has 'units' => (is => 'rw', isa => 'Int');
has 'hp' => (is => 'rw', isa => 'Int');
has 'damage' => (is => 'ro', isa => 'Int');
has 'type' => (is => 'ro', isa => 'Str');
has 'initiative' => (is => 'ro', isa => 'Int');
has 'weakness' => (is => 'ro', isa => 'ArrayRef[Str]');
has 'immune' => (is => 'ro', isa => 'ArrayRef[Str]');
has 'targeted' => (is => 'rw', isa => 'Bool', default => 0);
has 'target' => (is => 'rw', isa => 'Unit', clearer => 'clear_target',
                 predicate => 'has_target');
has 'boost' => (is => 'rw', isa => 'Int', default => 0);

sub is_weak {
  my ($self, $to) = @_;
  return any { $_ eq $to } $self->weakness->@*;
}

sub is_immune {
  my ($self, $to) = @_;
  return any { $_ eq $to } $self->immune->@*;
}

sub effective_power {
  my $self = shift;
  return $self->units * ($self->damage + $self->boost);
}

sub compute_damage {
  my ($self, $other) = @_;
  my $ep = $self->effective_power;
  if ($other->is_immune($self->type)) {
    return 0;
  } elsif ($other->is_weak($self->type)) {
    return $ep * 2;
  } else {
    return $ep;
  }
}

sub target_cmp {
  my ($f, $s) = @_;
  my $cmp = $s->effective_power <=> $f->effective_power;
  if ($cmp == 0) {
    return $s->initiative <=> $f->initiative;
  } else {
    return $cmp;
  }
}

sub clone {
  my $old = shift;
  bless { %$old }, ref $old;
}

package main;

my @immune;
my @infection;

my $which;
my $side;
while (<>) {
  if (/Immune System:/) {
    $side = "immune";
    $which = \@immune;
  } elsif (/Infection:/) {
    $side = "infection";
    $which = \@infection;
  } elsif (/(\d+) units each with (\d+) hit points\s*(\([^)]+\))?\s+with an attack that does (\d+) (\w+) damage at initiative (\d+)/) {
    my ($count, $hp, $damage, $type, $initiative) = ($1, $2, $4, $5, $6);
    my $weak = [];
    my $immune = [];
    if (defined $3) {
      my $bits = $3;
      $bits =~ s/[()]//g;
      my @parts = split /\s*;\s*/, $bits;
      for my $part (@parts) {
        if ($part =~ m/weak to/) {
          $part =~ s/weak to\s*//;
          @$weak = split /\s*,\s*/, $part;
        } elsif ($part =~ m/immune to/) {
          $part =~ s/immune to\s*//;
          @$immune = split /\s*,\s*/, $part;
        }
      }
    }
    push @$which, Unit->new(side => $side,
                            units => $count, hp => $hp, damage => $damage,
                            type => $type, initiative => $initiative,
                            weakness => $weak, immune => $immune);
  }
}

sub fight {
  my $turns = 0;
  my ($im, $in, $boost) = @_;
  my @immune = map { $_->clone } @$im;
  my @infection = map { $_->clone } @$in;
  $_->boost($boost) for @immune;

  while (scalar @immune && scalar @infection) {
    $turns += 1;
    my @all = sort { $a->target_cmp($b) } (@immune, @infection);
    for my $unit (@all) {
      $unit->targeted(0);
      $unit->clear_target;
    }
    for my $unit (@all) {
      my @targets = grep { not $_->targeted }
        ($unit->side eq "immune" ? @infection : @immune);
      next unless @targets;
      @targets = map { $_->[0] }
        sort { my $c = $a->[1] <=> $b->[1];
               if ($c == 0) {
                 $c = $a->[0]->effective_power <=> $b->[0]->effective_power;
                 if ($c == 0) {
                   return $a->[0]->initiative <=> $b->[0]->initiative;
                 } else {
                   return $c;
                 }
               } else {
                 return $c;
               }
             }
        map { [ $_, $unit->compute_damage($_) ] } @targets;
      while (@targets && $unit->compute_damage($targets[-1]) == 0) {
        pop @targets;
      }
      next unless @targets;
      my $target = $targets[-1];
      next unless $unit->compute_damage($target) > 0;
      $target->targeted(1);
      $unit->target($target);
    }
    @all = sort { $b->initiative <=> $a->initiative } @all;
    my $battles = 0;
    for my $unit (@all) {
      use integer;
      next if $unit->units <= 0;
      next unless $unit->has_target;
      my $target = $unit->target;
      next if $target->units <= 0;
      my $damage = $unit->compute_damage($target);
      my $tunits = $damage / $target->hp;
      $battles += 1 if $tunits > 0;
      $target->units($target->units - $tunits);
    }    
    @immune = grep { $_->units > 0 } @immune;
    @infection = grep { $_->units > 0 } @infection;
    if ($battles == 0) {
      say "No fights after $turns turns";
      return "tie", -1;
    }
  }

  if (@immune) {
    return "immune", reduce { $a + $b->units } (0, @immune);
  } else {
    return "infection", reduce { $a + $b->units } (0, @infection) ;
  }
}

my ($winner, $units) = fight(\@immune, \@infection, 0);

say "Part 1: $units";

for my $boost (30 .. 40) {
  say "Boost is $boost";
  my ($winner, $units) = fight(\@immune, \@infection, $boost);
  if ($winner eq "immune") {
    say "Part 2: $units";
    last;
  }
}
exit 0;

my $boost = 200;
my $immune_found = 0;
while (1) {
  say "Boost is $boost";
  my ($winner, $units) = fight(\@immune, \@infection, $boost);
  if ($winner eq "infection" || $winner eq "tie") {
    say "Infection won.";
    $boost += $immune_found ? 1 : 50;
  } else {
    say "Immune won.";
    $immune_found = 1;
    my ($w2, $u2) = fight(\@immune, \@infection, $boost - 1);
    if ($w2 eq "infection") {
      say "Part 2: $units";
      last;
    } else {
      $boost -= 25;
      $boost += 24 if $boost <= 0;
    }
  }
}
