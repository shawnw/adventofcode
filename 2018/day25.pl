#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say/;
use List::Util qw/any reduce/;
use List::MoreUtils qw/zip6/;

sub distance {
  my ($p1, $p2) = @_;
  return abs($p1->[0] - $p2->[0])
    + abs($p1->[1] - $p2->[1])
    + abs($p1->[2] - $p2->[2])
    + abs($p1->[3] - $p2->[3]);
  # return reduce { $a + $b } map { abs($_->[0] - $_->[1]) } zip6(@$p1, @$p2);
}

my @points;
while (<>) {
  chomp;
  my @point = split /,/;
  push @points, \@point;
}

my @constellations;

while (@points) {
  my $point = shift @points;
  my @cons = ($point);
  my $added = 1;
  while ($added == 1) {
    $added = 0;
    for (my $n = 0; $n < @points; ) {
      if (any { distance($points[$n], $_) <= 3 } @cons) {
        push @cons, $points[$n];
        splice @points, $n, 1, ();
        $added = 1;
      } else {
        $n += 1;
      }
    }
  }
  push @constellations, \@cons;
}

say "Part 1: ", scalar @constellations;
