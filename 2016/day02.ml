(* % ./day02.byte < day02.txt *)

open Batteries

type direction = L | R | U | D

let move_grid button dir =
  match button, dir with
  | 1, R -> 2
  | 1, D -> 4
  | 1, _ -> 1

  | 2, U -> 2
  | 2, R -> 3
  | 2, D -> 5
  | 2, L -> 1

  | 3, D -> 6
  | 3, L -> 2
  | 3, _ -> 3

  | 4, U -> 1
  | 4, R -> 5
  | 4, D -> 7
  | 4, L -> 4

  | 5, U -> 2
  | 5, R -> 6
  | 5, D -> 8
  | 5, L -> 4

  | 6, U -> 3
  | 6, R -> 6
  | 6, D -> 9
  | 6, L -> 5

  | 7, U -> 4
  | 7, R -> 8
  | 7, _ -> 7

  | 8, U -> 5
  | 8, R -> 9
  | 8, D -> 8
  | 8, L -> 7

  | 9, U -> 6
  | 9, L -> 8
  | 9, _ -> 9

  | _, _ -> raise (Invalid_argument "Number out of range")

let move_cross button dir =
  match button, dir with
  | 1, D -> 3
  | 1, _ -> 1
              
  | 2, R -> 3
  | 2, D -> 6
  | 2, _ -> 2

  | 3, U -> 1
  | 3, R -> 4
  | 3, D -> 7
  | 3, L -> 2

  | 4, D -> 8
  | 4, L -> 3
  | 4, _ -> 4

  | 5, R -> 6
  | 5, _ -> 5

  | 6, U -> 2
  | 6, R -> 7
  | 6, D -> 0xA
  | 6, L -> 5

  | 7, U -> 3
  | 7, R -> 8
  | 7, D -> 0xB
  | 7, L -> 6

  | 8, U -> 4
  | 8, R -> 9
  | 8, D -> 0xC
  | 8, L -> 7

  | 9, L -> 8
  | 9, _ -> 9

  | 0xA, U -> 6
  | 0xA, R -> 0xB
  | 0xA, _ -> 0xA

  | 0xB, U -> 7
  | 0xB, R -> 0xC
  | 0xB, D -> 0xD
  | 0xB, L -> 0xA

  | 0xC, U -> 8
  | 0xC, L -> 0xB
  | 0xC, _ -> 0xC

  | 0xD, U -> 0xB
  | 0xD, _ -> 0xD

  | _, _ -> raise (Invalid_argument "Number out of range 2")
            

                  
let c2d = function
  | 'L' -> L
  | 'R' -> R
  | 'U' -> U
  | 'D' -> D
  | c -> Printf.printf "Unknown character '%c'\n" c; raise (Invalid_argument "No such direction")

let print_hex n =
  Printf.printf "%x" n
                                                           
let _ =
  let buttons = ref (5, 5)
  and grid_password = ref []
  and cross_password = ref [] in
  BatIO.lines_of stdin |>
    BatEnum.iter (fun line ->
        let dirs = BatString.to_list line |> BatList.map c2d in
        buttons := BatList.fold_left (fun (grid,cross) dir ->
                       (move_grid grid dir,
                        move_cross cross dir)) !buttons dirs;
        grid_password := fst !buttons :: !grid_password;
        cross_password := snd !buttons :: !cross_password);
  print_string "Part 1: ";
  List.iter print_int (List.rev !grid_password);
  print_newline ();
  print_string "Part 2: ";
  List.iter print_hex (List.rev !cross_password);
  print_newline ();
    
                                    
            
