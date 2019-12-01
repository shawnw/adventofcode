#!/usr/bin/perl
use warnings;
no warnings qw/recursion experimental/;
use strict;
use feature qw/say postderef/;
use integer;
use Hash::PriorityQueue;

my $verbose = 0;

my $depth = 3198;
my @target = (12, 757);

#my $depth = 510;
#my @target = (10, 10);

my %cave;
$cave{0}->{0}->{'index'} = 0;
$cave{$target[0]}->{$target[1]}->{'index'} = 0;

sub geologic_index {
  my ($x, $y) = @_;
  if (exists $cave{$x}->{$y}->{'index'}) {
    return $cave{$x}->{$y}->{'index'};
  } elsif ($y == 0) {
    my $gi = $x * 16807;
    $cave{$x}->{$y}->{'index'} = $gi;
    return $gi;
  } elsif ($x == 0) {
    my $gi = $y * 48271;
    $cave{$x}->{$y}->{'index'} = $gi;
    return $gi;
  } else {
    my $gi = erosion_level($x - 1, $y) * erosion_level($x, $y - 1);
    $cave{$x}->{$y}->{'index'} = $gi;
    return $gi;
  }
}

sub erosion_level {
  my ($x, $y) = @_;
  if (exists $cave{$x}->{$y}->{'level'}) {
    return $cave{$x}->{$y}->{'level'};
  } else {
    my $el = (geologic_index($x, $y) + $depth) % 20183;
    $cave{$x}->{$y}->{'level'} = $el;
    return $el;
  }
}

sub erosion_type {
  my ($x, $y) = @_;
  if (exists $cave{$x}->{$y}->{'type'}) {
    return $cave{$x}->{$y}->{'type'};
  } else {
    my $et = erosion_level($x, $y) % 3;
    $cave{$x}->{$y}->{'type'} = $et;
    return $et;
  }
}

my $risk = 0;
for my $y (0 .. $target[1]) {
  for my $x (0 .. $target[0]) {
    $risk += erosion_type($x, $y);
    print (('.', '=', '|')[$cave{$x}->{$y}->{'type'}]) if $verbose;
  }
  print "\n" if $verbose;
}

say "Part 1: $risk";

package Node;
use warnings;
use strict;
use overload 'eq' => \&node_eq, 'ne' => \&node_ne, '""' => \&node_str;

sub new {
  my ($class, $x, $y, $equip, $score) = @_;
  return bless [ $x, $y, $equip, $score ], $class;
}

sub node_str {
  my $n = shift;
  return "[@$n[0,1,2]]";
}

sub node_eq {
  my ($lhs, $rhs) = @_;
  return $lhs->[0] == $rhs->[0] &&
    $lhs->[1] == $rhs->[1] &&
    $lhs->[2] eq $rhs->[2];
}

sub node_ne {
  my ($lhs, $rhs) = @_;
  return !($lhs->[0] == $rhs->[0] &&
           $lhs->[1] == $rhs->[1] &&
           $lhs->[2] eq $rhs->[2]);
}

package main;

my %graph;

sub add_graph_node {
  my ($x, $y) = @_;
  return if exists $graph{$x}->{$y};
  my $ctype = erosion_type($x, $y);

  if ($ctype == 0) {
    push $graph{$x}->{$y}->{'torch'}->@*, Node->new($x, $y, 'climbing', 7);
    push $graph{$x}->{$y}->{'climbing'}->@*, Node->new($x, $y, 'torch', 7);
  } elsif ($ctype == 1) {
    push $graph{$x}->{$y}->{'neither'}->@*, Node->new($x, $y, 'climbing', 7);
    push $graph{$x}->{$y}->{'climbing'}->@*, Node->new($x, $y, 'neither', 7);
  } elsif ($ctype == 2) {
    push $graph{$x}->{$y}->{'torch'}->@*, Node->new($x, $y, 'neither', 7);
    push $graph{$x}->{$y}->{'neither'}->@*, Node->new($x, $y, 'torch', 7);
  }

  for my $n ([-1, 0], [0, -1], [1, 0], [0, 1]) {
    my ($newx, $newy) = ($x + $n->[0], $y + $n->[1]);
    next if $newx < 0 || $newy < 0 || $newx > 2000 || $newy > 1000;
    my $ntype = erosion_type($newx, $newy);
    if ($ctype == 0) {
      if ($ntype == 0) {
        push $graph{$x}->{$y}->{'torch'}->@*,
          Node->new($newx, $newy, 'torch', 1);
        push $graph{$x}->{$y}->{'climbing'}->@*,
          Node->new($newx, $newy, 'climbing', 1);
      } elsif ($ntype == 1) {
        push $graph{$x}->{$y}->{'climbing'}->@*,
          Node->new($newx, $newy, 'climbing', 1);
        } elsif ($ntype == 2) {
          push $graph{$x}->{$y}->{'torch'}->@*,
            Node->new($newx, $newy, 'torch', 1);
        }
    } elsif ($ctype == 1) {
      if ($ntype == 0) {
        push $graph{$x}->{$y}->{'climbing'}->@*,
          Node->new($newx, $newy, 'climbing', 1);
      } elsif ($ntype == 1) {
        push $graph{$x}->{$y}->{'neither'}->@*,
          Node->new($newx, $newy, 'neither', 1);
        push $graph{$x}->{$y}->{'climbing'}->@*,
          Node->new($newx, $newy, 'climbing', 1);
      } elsif ($ntype == 2) {
        push $graph{$x}->{$y}->{'neither'}->@*,
          Node->new($newx, $newy, 'neither', 1);
      }
    } elsif ($ctype == 2) {
      if ($ntype == 0) {
        push $graph{$x}->{$y}->{'torch'}->@*,
          Node->new($newx, $newy, 'torch', 1);
      } elsif ($ntype == 1) {
        push $graph{$x}->{$y}->{'neither'}->@*,
          Node->new($newx, $newy, 'neither', 1);
      } elsif ($ntype == 2) {
        push $graph{$x}->{$y}->{'torch'}->@*,
          Node->new($newx, $newy, 'torch', 1);
        push $graph{$x}->{$y}->{'neither'}->@*,
          Node->new($newx, $newy, 'neither', 1);
      }
    }
  }
}

sub astar {
  my %closed;
  my $open = Hash::PriorityQueue->new();
  $open->insert(Node->new(0, 0, 'torch', 1), $target[0] + $target[1]);
  my %hopen;
  $hopen{0}->{0}->{'torch'} = 1;
  my %gscore;
  $gscore{0}->{0}->{'torch'} = 0;

  while (my $current = $open->pop()) {
    delete $hopen{$current->[0]}->{$current->[1]}->{$current->[2]};

    add_graph_node($current->[0], $current->[1]);

    if ($current->[0] == $target[0] && $current->[1] == $target[1]) {
      say "Done with @$current";
      my $time = $gscore{$current->[0]}->{$current->[1]}->{$current->[2]};
      $time += 7 if $current->[2] ne 'torch';
      return $time;
    }

    $closed{$current->[0]}->{$current->[1]}->{$current->[2]} = 1 ;

  NEIGHBOR:
    for my $n
      ($graph{$current->[0]}->{$current->[1]}->{$current->[2]}->@*) {
      next NEIGHBOR if exists $closed{$n->[0]}->{$n->[1]}->{$n->[2]};
      my $score =
        $gscore{$current->[0]}->{$current->[1]}->{$current->[2]} + $n->[3];
      my $new_fscore = $score +
        abs($n->[0] - $target[0]) +
        abs($n->[1] - $target[1]);
      $new_fscore += 7 if $n->[2] ne 'torch';

      if (!exists $hopen{$n->[0]}->{$n->[1]}->{$n->[2]}) {
        $open->insert($n, $new_fscore);
        $hopen{$n->[0]}->{$n->[1]}->{$n->[2]} = 1;
      } elsif ($score >= ($gscore{$n->[0]}->{$n->[1]}->{$n->[2]} // 100000)) {
        next NEIGHBOR;
      } else {
        $open->update($n, $new_fscore);
      }
      $gscore{$n->[0]}->{$n->[1]}->{$n->[2]} = $score;
    }
  }
  die "Path not found!";
}

say "Part 2: ", astar();
