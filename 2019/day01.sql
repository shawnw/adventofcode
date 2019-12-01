-- CREATE TABLE modules(mass INTEGER);
-- .import input.txt modules
-- Part 1
SELECT sum(mass/3 - 2) FROM modules;
-- Part 2
WITH fuels AS
 (SELECT  mass / 3 - 2 as fuel FROM modules
  UNION ALL
  SELECT fuel / 3 - 2 FROM fuels WHERE fuel / 3 - 2 > 0)
SELECT sum(fuel) FROM fuels;
