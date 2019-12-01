open Batteries

let run_part1 c =
  let count = ref c
  and elves = ref (BatDllist.of_enum (1--c)) in
  while !count > 1 do
    elves := BatDllist.next !elves |> BatDllist.drop;
    decr count
  done;
  BatDllist.get !elves

let run_part2 c =
  let count = ref c
  and elves = ref (BatDllist.of_enum (1--c)) in
  let victim = ref (BatDllist.skip !elves (!count / 2)) in
  while !count > 1 do
(*    BatPrintf.printf "Next elf: %d, victim: %d\n"
                     (BatDllist.get !elves) (BatDllist.get !victim); *)
    victim := BatDllist.drop !victim;
    if !count mod 2 = 1 then
      victim := BatDllist.next !victim;
    elves := BatDllist.next !elves;
    decr count;
  done;
  BatDllist.get !elves
                                             
let _ =
  let elves = 3_018_458 in
  let test1 = run_part1 5 in
  BatPrintf.printf "Test 1: Elf %d\n" test1;
  let part1 = run_part1 elves in
  BatPrintf.printf "Part 1: Elf %d\n" part1;
  let test2 = run_part2 5 in
  BatPrintf.printf "Test 2: Elf %d\n" test2;
  let part2 = run_part2 elves in
  BatPrintf.printf "Part 2: Elf %d\n" part2
                   
