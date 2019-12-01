#!/usr/bin/perl
use warnings;
use strict;
use feature qw/say/;
use DBI;

my $dbh = DBI->connect("dbi:SQLite:dbname=day23.db", "", "",
                       { RaiseError => 1 });

sub taxicab {
    my ($x1, $y1, $z1, $x2, $y2, $z2) = @_;
    return abs($x1 - $x2) + abs($y1 - $y2) + abs($z1 - $z2);
}
$dbh->sqlite_create_function("distance", 6, \&taxicab, 0);

$dbh->do("DROP TABLE IF EXISTS grid");

$dbh->do(<<EOQ);
CREATE TABLE grid(x INTEGER
                , y INTEGER
                , z INTEGER
                , range INTEGER
                , PRIMARY KEY(x, y, z)) WITHOUT ROWID;
EOQ

my $ins = $dbh->prepare("INSERT INTO grid(x,y,z,range) VALUES (?,?,?,?)");

$dbh->begin_work;
while (<>) {
  if (/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/) {
    $ins->execute($1, $2, $3, $4);
  }
}
$dbh->commit;

my @b = $dbh->selectrow_array(<<EOQ);
SELECT x, y, z, range FROM grid ORDER BY range DESC LIMIT 1
EOQ

my $part1 = $dbh->selectrow_array(<<EOQ, {}, @b);
SELECT count(*) AS "Part 1"
FROM grid
WHERE distance(x, y, z, ?, ?, ?) <= (? + 0)
EOQ

say "Part 1: $part1";


my $t = $dbh->selectall_arrayref(<<EOQ);
WITH x_vals(x) AS
  (SELECT min(x) FROM grid
   UNION ALL
   SELECT x + 1 FROM x_vals WHERE x < (SELECT max(x) FROM grid))
, y_vals(y) AS
  (SELECT min(y) FROM grid
   UNION ALL
   SELECT y + 1 FROM y_vals WHERE x < (SELECT max(y) FROM grid))
, z_vals(z) AS
              


SELECT g1.x, g1.y, g1.z, g1.range, count(*)
FROM grid AS g1
JOIN grid AS g2
WHERE distance(g1.x, g1.y, g1.z, g2.x, g2.y, g2.z) <= g1.range
GROUP BY g1.x, g1.y, g1.z
ORDER BY count(*) DESC
-- LIMIT 1
EOQ

$" = ",";
say "@$_" for @$t;

my $part2 = $dbh->selectrow_array(<<EOQ);
WITH most(x, y, z, r) AS
(SELECT g1.x, g1.y, g1.z, g1.range
 FROM grid AS g1
 JOIN grid AS g2
 WHERE distance(g1.x, g1.y, g1.z, g2.x, g2.y, g2.z) <= g1.range
 GROUP BY g1.x, g1.y, g1.z
 ORDER BY count(*) DESC
 LIMIT 1
)
SELECT min(distance(0, 0, 0, g.x, g.y, g.z)) AS "Part 2"
FROM grid AS g
JOIN most AS m ON distance(m.x, m.y, m.z, g.x, g.y, g.z) <= m.r
EOQ

say "Part 2: $part2";
$dbh->disconnect;




