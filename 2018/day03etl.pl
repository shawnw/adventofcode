#!/usr/bin/perl -s
use warnings;
use strict;
use feature qw/say/;

# Usage: perl day03etl.pl [-svg] day03.txt | sqlite3

our $svg;

say "CREATE VIRTUAL TABLE claims USING geopoly();";
say "BEGIN TRANSACTION;";

while (<>) {
  if (/^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$/) {
    my $square = "[[$2,$3],";
    $square .= "[" . ($2 + $4) . ",$3],";
    $square .= "[" . ($2 + $4) . "," . ($3 + $5) . "],";
    $square .= "[$2," . ($3 + $5) . "],";
    $square .= "[$2,$3]]";
    say "INSERT INTO claims(rowid,_shape) VALUES ($1,'$square');";
  }
}

say "COMMIT;";

sub part1 {
  print <<EOQ;
WITH RECURSIVE
     rows(y) AS (SELECT 0 UNION ALL SELECT y + 1 FROM rows WHERE y < 999)
   , cols(x) AS (SELECT 0 UNION ALL SELECT x + 1 FROM cols WHERE x < 999)
SELECT count(*) AS "Part 1" FROM rows JOIN cols
WHERE (SELECT count(*) FROM claims
       WHERE geopoly_overlap(_shape,
                             json_array(json_array(x, y)
                                      , json_array(x + 1, y)
                                      , json_array(x + 1, y + 1)
                                      , json_array(x, y + 1)
                                      , json_array(x, y)))) > 1;
EOQ
}

sub part2 {
  print <<EOQ;
SELECT rowid AS "Part 2" FROM claims AS f1
WHERE NOT EXISTS (SELECT 1 FROM claims AS f2
                  WHERE f1.rowid <> f2.rowid
                    AND geopoly_overlap(f1._shape, f2._shape))
LIMIT 1;
EOQ
}

sub print_svg {
  print <<EOQ;
.headers off
.mode list
SELECT '<svg width="640" height="640">';
SELECT geopoly_svg(_shape) FROM fabric;
SELECT '</svg>';
EOQ
}

if ($svg) {
  print_svg();
} else {
  say ".headers on";
  say ".timer on";
  part1();
  part2();
}
