#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say state/;
use integer;

#my $serial = 18;
my $serial = 1723;

my @grid;
for my $x (1 .. 300) {
  for my $y (1 .. 300) {
    my $rack_id = $x + 10;
    my $power = $rack_id * $y;
    $power += $serial;
    $power *= $rack_id;
    $power = ($power / 100) % 10;
    $power -= 5;
    $grid[$x]->[$y] = $power;
  }
}

sub sum_square {
  state %cache;
  my ($x1, $y1, $size) = @_;

  if (exists $cache{$x1} && exists $cache{$x1}->{$y1}) {
    $size -= 1;
    my $sum = $cache{$x1}->{$y1};
    $sum += $grid[$x1 + $size]->[$y1 + $_] for 0 .. $size;
    $sum += $grid[$x1 + $_]->[$y1 + $size] for 0 .. $size - 1;
    #$sum -= $grid[$x1 + $size]->[$y1 + $size];
    $cache{$x1}->{$y1} = $sum;
    return $sum;
  } else {
    my $sum = $grid[$x1]->[$y1];
    $cache{$x1}->{$y1} = $sum;
    return $sum;
  }
}

my $max2;
for my $size (1 .. 300) {
  my $thismax;
  for my $x (1 .. 300) {
    last if 300 - $x < $size;
    for my $y (1 .. 300) {
      last if 300 - $y < $size;
      my $sum = sum_square $x, $y, $size;
      if (defined $max2) {
        $max2 = [ $x, $y, $size, $sum ] if $sum > $max2->[3];
      } else {
        $max2 = [ $x, $y, $size, $sum ];
      }
      if ($size == 3) {
        if (defined $thismax) {
          $thismax = [ $x, $y, $size, $sum ] if $sum > $thismax->[3];
        } else {
          $thismax = [ $x, $y, $size, $sum ];
        }
      }
    }
  }
  say "Part 1: $thismax->[0],$thismax->[1]" if $size == 3;
}

say "Part 2: $max2->[0],$max2->[1],$max2->[2]";
