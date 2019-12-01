#!/usr/bin/perl
use warnings;
use strict;
use List::MoreUtils qw/minmax/;

my %ground;

$ground{500}->{0} = '+';

while (<>) {
    if (/x=(\d+), y=(\d+)\.\.(\d+)/) {
        my $x = $1;
        my $y1 = $2;
        my $y2 = $3;
        for ($y1 .. $y2) {
            $ground{$x}->{$_} = '#';
        }
    } elsif (/y=(\d+), x=(\d+)\.\.(\d+)/) {
        my $y = $1;
        my $x1 = $2;
        my $x2 = $3;
        for ($x1 .. $x2) {
            $ground{$_}->{$y} = '#';
        }
    }
}

my ($minx, $maxx) = minmax keys %ground;
my ($miny, $maxy) = minmax map { keys %$_ } values %ground;

sub print_map {
    for my $y ($miny .. $maxy) {
        for my $x ($minx - 1 .. $maxx + 1) {
            print $ground{$x}->{$y} // '.';
        }
        print "\n";
    }
}

sub count_water {
    my $sum = 0;
    for my $ys (values %ground) {
        for my $square (values %$ys) {
            $sum += 1 if $square eq "~" || $square eq "|";
        }
    }
    return $sum;
}

sub fill {
    my ($x, $y, $dir) = @_;
    return if $y < $miny || $y > $maxy;
    my $type = $ground{$x}->{$y} // '.';
    if ($type eq '.') {
        if ($dir eq 'down') {
            $ground{$x}->{$y} = '|';
            fill($x, $y + 1);
        } elsif ($dir eq 'left') {
            $ground{$x}->{$y} = '~';
            fill($x - 1, $y, 'left');
        } elsif ($dir eq 'right') {
            $ground{$x}->{$y} = '~';
            fill($x + 1, $y, 'right');
        }
        return;
    } elsif ($type eq '~') {
        return;
    } elsif ($type eq '#') {
        if ($dir eq 'down') {
            $ground{$x}->{$y - 1} = '~';
            fill($x - 1, $y - 1, 'left');
            fill($x + 1, $y - 1, 'right');
        }
        return;
    }    
}

print_map;

