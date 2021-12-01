#!/bin/sh

# Takes one argument; file with input numbers

sqlite3 --batch <<EOF
CREATE TABLE day01(depth INTEGER);
.mode csv
.import '$1' day01
.headers on
.mode column
-- Part 1
WITH lags AS (SELECT depth, lag(depth) OVER (ORDER BY rowid) AS prev FROM day01)
SELECT count(*) AS Part1
FROM lags
WHERE depth > prev;
-- Part 2
WITH windows AS
 (SELECT min(rowid) OVER win AS rowid, sum(depth) OVER win AS depth
  FROM day01
  WINDOW win AS (ORDER BY rowid ROWS BETWEEN CURRENT ROW AND 2 FOLLOWING)),
 lags AS (SELECT depth, lag(depth) OVER (ORDER BY rowid) AS prev FROM windows)
SELECT count(*) AS Part2
FROM lags
WHERE depth > prev;
EOF
