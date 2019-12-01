#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say/;

my $freq = 0;
my @shifts;

while (<>) {
  chomp;
  $freq += $_;
  push @shifts, $_;
}
say "Part 1: $freq";

my %freqs = ( 0 => 1 );
$freq  = 0;
LOOP:
while (1) {
  for (@shifts) {
    $freq += $_;
    if (++$freqs{$freq} == 2) {
      say "Part 2: $freq";
      last LOOP;
    }
  }
}
