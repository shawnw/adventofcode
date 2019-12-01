(* % ./day06.byte < day06.txt *)

open Batteries

let _ =
  let cols = BatArray.init 8 (function _ -> BatHashtbl.create 26) in
  BatIO.lines_of stdin |>
    BatEnum.iter
      (BatString.iteri (fun i ch ->
           BatHashtbl.modify_opt
             ch
             (function Some i -> Some (i + 1) | None -> Some 1)
             cols.(i)));
  print_string "Part 1 (Most common): ";
  BatArray.iter (fun tbl ->
      let ch, v = 
        BatHashtbl.fold (fun ch v mx ->
            if v > snd mx then (ch, v) else mx) tbl ('_', -1) in
        print_char ch) cols;
  print_newline ();
  print_string "Part 2 (Least common): ";
  BatArray.iter (fun tbl ->
      let ch, v = 
        BatHashtbl.fold (fun ch v mn ->
            if v < snd mn then (ch, v) else mn) tbl ('_', max_int) in
        print_char ch) cols;
  print_newline ()
                                 
