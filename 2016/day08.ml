(* % ./day08.byte < day08.txt *)

open Batteries

let width = 50
let height = 6
       
let make_screen () =
  BatArray.make_matrix width height false

let rect rows len screen =
  for x = 0 to rows - 1 do
    BatArray.fill screen.(x) 0 len true
  done;
  screen

let rotate_row row shift screen =
  let newrow = BatArray.make width false in
  for x = 0 to width - 1 do
    newrow.((x + shift) mod width) <- screen.(x).(row)
  done;
  for x = 0 to width - 1 do
    screen.(x).(row) <- newrow.(x)
  done;
  screen

let rotate_col col shift screen =
  let newcol = BatArray.make height false in
  for y = 0 to height - 1 do
    newcol.((y + shift) mod height) <- screen.(col).(y)
  done;
  BatArray.blit newcol 0 screen.(col) 0 height;
  screen

let count_pixels screen =
  BatArray.fold_left (fun acc col ->
      BatArray.fold_left (fun acc pixel ->
          if pixel then acc + 1 else acc) acc col)
                     0 screen

let try_parse fmt f screen line =
  try
    BatScanf.sscanf line fmt (fun a b -> Some (f a b screen))
  with
  | BatScanf.Scan_failure _ -> None
                     
let parse_rect = try_parse "rect %dx%d" rect
let parse_col = try_parse "rotate column x=%d by %d" rotate_col
let parse_row = try_parse "rotate row y=%d by %d" rotate_row

let parse screen line =
  let ways = [ parse_rect; parse_col; parse_row ] in
  match BatList.fold_right (fun way -> function
                                  | Some _ as p -> p
                                  | None -> way screen line) ways None with
  | Some s -> s
  | None -> raise (Invalid_argument line)

let print_screen screen =
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if screen.(x).(y) then
        print_char '#'
      else
        print_char ' '
    done;
    print_newline ()
  done
      
let _ =
  let screen =
    BatIO.lines_of stdin |> BatEnum.fold parse (make_screen ()) in
  Printf.printf "Part 1: %d\nPart 2:\n" (count_pixels screen);
  print_screen screen
    
  
  
                        
