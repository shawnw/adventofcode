#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say/;
use integer;

my $reps = shift;
my @board = (3, 7);
my $boardstr = "37";

my $elf1 = 0;
my $elf2 = 1;

sub advance {
  my $elf = shift;
  my $step = $elf + $board[$elf] + 1;
  if ($step >= @board) {
    $step = $step % @board;
  }
  return $step;
}

my $part1 = 0;
my $part2 = 0;
my $skipped = 0;

while ($part1 == 0 || $part2 == 0) {
  my $recipe = $board[$elf1] + $board[$elf2];
  my @new_recipes = split //, $recipe;
  push @board, $new_recipes[0];
  $boardstr .= $new_recipes[0];
  say "Part 1: ", @board[-10 .. -1] and $part1 = 1 if @board == $reps + 10;
  if (@new_recipes == 2) {
    push @board, $new_recipes[1];
    $boardstr .= $new_recipes[1];
  }
  if ($part1 == 0 && @board == $reps + 10) {
    say "Part 1: ", @board[-10 .. -1];
    $part1 = 1;
  }
  if ($part2 == 0 && $boardstr =~ m/${reps}.?$/) {
    say "Part 2: ", $skipped + $-[0];
    $part2 = 1;
  }
  if (length $boardstr > 10) {
    $skipped += length($boardstr) - 10;
    $boardstr = substr $boardstr, -10;
  }

  $elf1 = advance $elf1;
  $elf2 = advance $elf2;
}
