open Batteries

let is_trap row n =
  let len = BatString.length row in
  let left = if n = 0 then '.' else row.[n - 1]
  and right = if n = (len - 1) then '.' else row.[n + 1] in
  left <> right

let count_safe row =
  BatString.fold_left (fun sum ch -> if ch = '.' then sum + 1 else sum) 0 row

let calc_both prev =
  let len = BatString.length prev
  and safe = ref 0 in
  let row =
    BatString.init len (fun n ->
                     if is_trap prev n then
                       '^'
                     else
                       begin
                         incr safe;
                         '.'
                       end) in
  row, !safe

let count_rows first rows =
  let row = ref first
  and safe = ref (count_safe first) in
  for n = 1 to rows - 1 do
    let nextrow, count = calc_both !row in
    safe := !safe + count;
    row := nextrow
  done;
  !safe

let _ =
  let test1 = count_rows "..^^." 3 in
  BatPrintf.printf "Test 1: %d\n" test1; (* 6 *)
  let test2 = count_rows ".^^.^.^^^^" 10 in
  BatPrintf.printf "Test 2: %d\n" test2; (* 38 *)
  let start_row = read_line () in
  let part1 = count_rows start_row 40 in
  BatPrintf.printf "Part 1: %d\n" part1; (* 1951 *)
  flush stdout;
  let part2 = count_rows start_row 400_000 in
  BatPrintf.printf "Part 2: %d\n" part2 (* 20002936 *)
                   
                 
