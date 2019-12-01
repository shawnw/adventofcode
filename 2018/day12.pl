#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say/;
use integer;
use List::MoreUtils qw/minmax/;

my $line = <>;
if ($line =~ m/initial state: ([#.]+)/) {
    $line = $1;
} else {
    die "Invalid input!\n";
}

my %states;
while (<>) {
    if (/([#.]{5}) => ([.#])/) {
        $states{$1} = $2;
    }
}

my $count = 0;
my $pots = {};
for (split //, $line) {
    $pots->{$count++} = $_;
}

my $start = 0;
my $end = $count - 1;

sub run {
    my ($step_start, $step_end) = @_;
    my $prevsum = 0;
    for ($step_start .. $step_end) {
        my $prev = $pots;
        $pots = {};
        for my $p ($start - 2 .. $end + 2) {
            my $fragment;
            $fragment .= $prev->{$p - 2} // '.';
            $fragment .= $prev->{$p - 1} // '.';
            $fragment .= $prev->{$p} // '.';
            $fragment .= $prev->{$p + 1} // '.';
            $fragment .= $prev->{$p + 2} // '.';
            $pots->{$p} = $states{$fragment} // '.';
        }
        ($start, $end) = minmax keys %$pots;
        delete $pots->{$start++} while $pots->{$start} eq '.';
        delete $pots->{$end--} while $pots->{$end} eq '.';
        if ($_ == 20 || $_ >= 1000) {
            my $sum = sum_pots($pots);
            say "Part 1: $sum" if $_ == 20;
            if ($_ == 1001) {
                say "Part 2: ",
                    $prevsum + ((50000000000 - 1000) * ($sum - $prevsum));
                return;
            }
            $prevsum = $sum;
        }
    }
}

run(1,1005);

sub sum_pots {
    my $p = shift;
    my $sum = 0;
    while (my ($pos, $type) = each %$p) {
        $sum += $pos if $type eq "#";
    }
    return $sum;
}
