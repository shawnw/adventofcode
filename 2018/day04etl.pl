#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say/;

# Usage:
# sort -k1,2 day04.txt | perl day04etl.pl | sqlite3

my %guards;
my $guard;

print <<EOQ;
CREATE TABLE observations(guard INTEGER, minute INTEGER, state INTEGER);
BEGIN TRANSACTION;
EOQ

while (<>) {
    if (/\[(\d\d\d\d-\d\d-\d\d) \d\d:(\d\d)\] Guard #(\d+) begins shift/) {
        $guard = $3;
        $guards{$guard}->{$1}->{$2} = 0;
    } elsif (/\[(\d\d\d\d-\d\d-\d\d) \d\d:(\d\d)\] falls asleep/) {
        $guards{$guard}->{$1}->{$2} = 1;
    } elsif (/\[(\d\d\d\d-\d\d-\d\d) \d\d:(\d\d)\] wakes up/) {
        $guards{$guard}->{$1}->{$2} = 0;
    }
}

while (my ($g, $days) = each %guards) {
    for my $minutes (values %$days) {
        my $state = undef;
        for my $min ("00" .. "59") {
            $state = $minutes->{$min} if exists $minutes->{$min};
            say "INSERT INTO observations VALUES($g, $min, $state);" if defined $state;
        }
    }
}

print <<EOQ;
CREATE INDEX obs_idx ON observations(guard, minute, state);

CREATE VIEW byminute(guard, minute, sleep_time) AS
 SELECT guard, minute, sum(state) FROM observations GROUP BY guard, minute;

COMMIT;

.header on
.mode column
.timer on

--EXPLAIN QUERY PLAN
SELECT guard, minute, guard * minute AS "Part 1"
FROM byminute
WHERE guard = (SELECT guard FROM observations
               WHERE state = 1 GROUP BY guard ORDER BY count(*) DESC LIMIT 1)
ORDER BY sleep_time DESC
LIMIT 1;

--EXPLAIN QUERY PLAN
SELECT guard, minute, guard * minute AS "Part 2"
FROM byminute
ORDER BY sleep_time DESC
LIMIT 1;

EOQ


