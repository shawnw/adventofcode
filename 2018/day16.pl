#!/usr/bin/perl -s
use warnings;
use strict;
use feature qw/say/;
use integer;
use English;

our $part2;

$RS = "\n\n";

our @registers = (0, 0, 0, 0);

my %opcodes = (
    addr => sub { $registers[$_[2]] = $registers[$_[0]] + $registers[$_[1]] },
    addi => sub { $registers[$_[2]] = $registers[$_[0]] + $_[1] },
    mulr => sub { $registers[$_[2]] = $registers[$_[0]] * $registers[$_[1]] },
    muli => sub { $registers[$_[2]] = $registers[$_[0]] * $_[1] },
    banr => sub { $registers[$_[2]] = $registers[$_[0]] & $registers[$_[1]] },
    bani => sub { $registers[$_[2]] = $registers[$_[0]] & $_[1] },
    borr => sub { $registers[$_[2]] = $registers[$_[0]] | $registers[$_[1]] },
    bori => sub { $registers[$_[2]] = $registers[$_[0]] | $_[1] },
    setr => sub { $registers[$_[2]] = $registers[$_[0]] },
    seti => sub { $registers[$_[2]] = $_[0] },
    gtir => sub { $registers[$_[2]] = $_[0] > $registers[$_[1]] },
    gtri => sub { $registers[$_[2]] = $registers[$_[0]] > $_[1] },
    gtrr => sub { $registers[$_[2]] = $registers[$_[0]] > $registers[$_[1]] },
    eqir => sub { $registers[$_[2]] = $_[0] == $registers[$_[1]] },
    eqri => sub { $registers[$_[2]] = $registers[$_[0]] == $_[1] },
    eqrr => sub { $registers[$_[2]] = $registers[$_[0]] == $registers[$_[1]] }
    );

my %opnums;
my $total_matches = 0;
while (<>) {
    last if m/\A\s+\z/;
    if (/Before:\s+\[(\d+),\ (\d+),\ (\d+),\ (\d+)\]\s+
         (\d+)\ (\d+)\ (\d+)\ (\d+)\s+
         After:\s+\[(\d+),\ (\d+),\ (\d+),\ (\d+)\]/x) {
        my $matches = 0;  
        my $opname = "";
        while (my ($name, $op) = each %opcodes) {
            local @registers = ($1, $2, $3, $4);
            $op->($6, $7, $8);
            if ($registers[0] == $9 && $registers[1] == $10
                && $registers[2] == $11 && $registers[3] == $12) {
                $matches += 1;
                $opname = $name;
            }
        }
        if ($part2 && $matches == 1) {
            $opnums{$5} = $opcodes{$opname};
            delete $opcodes{$opname};
        }
        $total_matches += 1 if $matches >= 3;
    }
}

if (not $part2) {
    say "Part 1: $total_matches";
    exit 0;
}

$RS = "\n";
while (<>) {
    if (/(\d+) (\d+) (\d+) (\d+)/) {
        $opnums{$1}->($2, $3, $4);
    } else {
        die "Invalid input $_";
    }
}
say "Part 2: $registers[0]";
