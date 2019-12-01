open Batteries

type disc = { positions : int; starting : int }

let input_discs () =
  BatIO.lines_of stdin
  |> BatList.of_enum 
  |> BatList.map (fun line ->
         BatScanf.sscanf line "Disc #%d has %d positions; at time=0, it is at position %d." (fun _ npos startpos -> { positions = npos; starting = startpos }))

let rec valid_state t = function
  | [] -> true
  | hd :: tl ->
     if (hd.starting + t + 1) mod hd.positions = 0 then
       valid_state (t + 1) tl
     else
       false
                                
let find_start_time discs =
  let t = ref 0 in
  while not @@ valid_state !t discs do
    incr t
  done;
  !t

let _ =
  let discs = input_discs () in
  let t = find_start_time discs in
  BatPrintf.printf "Part 1: Drop at time %d\n" t;
  let t2 = find_start_time (discs @ [ { positions = 11; starting = 0 } ]) in
  BatPrintf.printf "Part 2: Drop at time %d\n" t2
  

                             
            
