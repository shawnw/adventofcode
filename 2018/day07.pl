#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say/;

my %steps;

while (<>) {
  if (/Step (.) must be finished before step (.)/) {
    $steps{$1}->{$2} = 1;
    $steps{$2} = {} unless exists $steps{$2};
  }
}

# Part 1
#my $base = 0;
#my $nworkers = 1;

# Part 2
my $base = 0;
my $nworkers = 5;

my @workers;
push @workers, [0, '-'] for 1 .. $nworkers;

sub order_steps {
  my $elapsed;
  for ($elapsed = 0; %steps; $elapsed += 1) {
    my $open = 0;
    for my $w (0 .. $nworkers - 1) {
      $workers[$w]->[0] -= 1 if $workers[$w]->[0] > 0;
      if ($workers[$w]->[0] == 0) {
        if ($workers[$w]->[1] ne '-') {
          delete $steps{$workers[$w]->[1]};
          print $workers[$w]->[1];
          $workers[$w]->[1] = '-';
        }
        $open = 1;
      }
    }
    next unless $open;
    my @heads = sort grep { my $s = $_;
                             not grep { $_->[1] eq $s } @workers
                             and
                             not grep { exists $steps{$_}->{$s} } keys %steps
                           } keys %steps;
    for my $w (0 .. $nworkers - 1) {
      next unless $workers[$w]->[1] eq '-';
      last unless @heads;
      my $step = shift @heads;
      $workers[$w]->[0] = $base + ord($step) - ord('A') + 1;
      $workers[$w]->[1] = $step;
    }
  }
  return $elapsed - 1;
}

print "Order: ";
my $time = order_steps;
say "\nTime: $time";
