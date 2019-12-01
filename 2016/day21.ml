open Batteries

type dir = Right | Left

let dir_of_string s =
  if s = "right" then
    Right
  else if s = "left" then
    Left
  else
    raise (Invalid_argument s)

let opposite = function Right -> Left | Left -> Right
          
type op = SwapPos of int * int
        | SwapChar of char * char
        | Rotate of dir * int
        | RotateChar of char
        | Reverse of int * int
        | Move of int * int

let swap str x y =
  let tmp = Bytes.get str x in
  Bytes.set str x (Bytes.get str y);
  Bytes.set str y tmp
                          
let rotate1_in_place str d =
  let len = (Bytes.length str) - 1 in
  match d with
  | Right ->
     let tmp = Bytes.get str len in
     Bytes.blit str 0 str 1 len;
     Bytes.set str 0 tmp
  | Left ->
     let tmp = Bytes.get str 0 in
     Bytes.blit str 1 str 0 len;
     Bytes.set str len tmp
               
let rev_in_place str =
  let len = Bytes.length str in
  for n = 0 to len/2 - 1 do
    swap str n (len - n - 1)
  done
    
let scram str = function
  | SwapPos (x, y) ->
     swap str x y
  | SwapChar (x, y) ->
     let xpos = Bytes.index str x
     and ypos = Bytes.index str y in
     swap str xpos ypos
  | Rotate (d, steps) ->
     for n = 1 to steps do
       rotate1_in_place str d
     done
  | RotateChar c ->
     let cpos = BatBytes.index str c in
(*     BatPrintf.printf "RotateChar: '%s' by %c at %d -> "
                      (BatBytes.to_string str) c cpos; *)
     rotate1_in_place str Right;
     for n = 1 to cpos do
       rotate1_in_place str Right
     done;
     if cpos >= 4 then
       rotate1_in_place str Right;
  (*     BatPrintf.printf "%s at %d\n" (BatBytes.to_string str) (BatBytes.index str c) *)
  | Reverse (x, y) ->
     let len = y - x + 1 in
     let tmp = BatBytes.sub str x len in
     rev_in_place tmp;
     BatBytes.blit tmp 0 str x len
  | Move (x, y) ->
     let tmp = BatBytes.get str x in
     let len = BatInt.abs (x - y) in
     if x < y then
       BatBytes.blit str (x+1) str x len
     else if x > y then
       BatBytes.blit str y str (y+1) len;
     BatBytes.set str y tmp

let scramble str ops =
  let str = BatBytes.copy str in
  BatList.iter (scram str) ops;
  str
                  
let unscram str = function
  | SwapPos (x, y) ->
     swap str x y
  | SwapChar (x, y) ->
     let xpos = Bytes.index str x
     and ypos = Bytes.index str y in
     swap str xpos ypos
  | Rotate (d, steps) ->
     let dir = opposite d in
     for n = 1 to steps do
       rotate1_in_place str dir
     done
  | RotateChar c ->     
     let cpos = BatBytes.index str c in
     (* Table to undo the rotation *)
     (* 0->1 1->3 2->5 3->7 4->2 5->4 6->6 7->0 *)     
     let steps =
       match cpos with
       | 0 -> 1 
       | 1 -> 1
       | 2 -> 6 
       | 3 -> 2
       | 4 -> 7
       | 5 -> 3
       | 6 -> 8
       | 7 -> 4
       | _ -> raise Not_found in
     for n = 1 to steps do
       rotate1_in_place str Left
     done     
  | Reverse (x, y) ->
     let len = y - x + 1 in
     let tmp = BatBytes.sub str x len in
     rev_in_place tmp;
     BatBytes.blit tmp 0 str x len
  | Move (y, x) ->
     let tmp = BatBytes.get str x in
     let len = BatInt.abs (x - y) in
     if x < y then
       BatBytes.blit str (x+1) str x len
     else if x > y then
       BatBytes.blit str y str (y+1) len;
     BatBytes.set str y tmp

let unscramble str ops =
  let str = BatBytes.copy str in
  BatList.rev ops |> BatList.iter (unscram str);
  str
      
let parse_swap line =
  BatScanf.sscanf line "swap position %d with position %d"
                  (fun x y -> SwapPos (x, y))
let parse_swapchar line =
  BatScanf.sscanf line "swap letter %c with letter %c"
                  (fun x y -> SwapChar (x, y))
let parse_reverse line =
  BatScanf.sscanf line "reverse positions %d through %d"
                  (fun x y -> Reverse (x, y))
let parse_rotate line =
  BatScanf.sscanf line "rotate %s %d step"
                  (fun dir steps ->
                    Rotate (dir_of_string dir, steps))
let parse_rotate_char line =
  BatScanf.sscanf line "rotate based on position of letter %c"
                  (fun c -> RotateChar c)
let parse_move line =
  BatScanf.sscanf line "move position %d to position %d"
                  (fun x y -> Move (x,y))
                  
let try_parse f line =
  try
    Some (f line)
  with
    BatScanf.Scan_failure _ -> None
  
let parse_op line =
  let parsers = [ try_parse parse_swap;
                  try_parse parse_swapchar;
                  try_parse parse_reverse;
                  try_parse parse_rotate;
                  try_parse parse_rotate_char;
                  try_parse parse_move ] in
  let op = BatList.fold_left (fun instr op ->
               match instr with
               | Some x -> instr
               | None -> op line) None parsers in
  match op with
  | Some x -> x
  | None -> raise (Invalid_argument "Unknown instruction.")

                 
let teststr = "abcde"
                
let testbytes = BatBytes.of_string teststr
and test_ops = [
    SwapPos (4,0);
    SwapChar ('d', 'b');
    Reverse (0,4);
    Rotate (Left, 1);
    Move (1,4);
    Move (3,0);
    RotateChar 'b';
    RotateChar 'd'
  ]

let test () =
  let res = scramble testbytes test_ops in
  let resstr = BatBytes.to_string res in
  BatPrintf.printf "Test 1: %s scrambled to %s\n" teststr resstr
                   
let main ()  =
  let ops = BatIO.lines_of stdin |> BatEnum.map parse_op |> BatList.of_enum in
  let inputstr = "abcdefgh" in
  let inputbytes = BatBytes.of_string inputstr in
  let inputstr2 = "fbgdceah" in
  let inputbytes2 = BatBytes.of_string inputstr2 in
  let res = scramble inputbytes ops in
  let resstr = BatBytes.to_string res in
  BatPrintf.printf "Part 1: %s scrambled to %s\n" inputstr resstr;
  let test2 = unscramble res ops in
  let test2str = BatBytes.to_string test2 in
  BatPrintf.printf "Test 2: %s unscrambled to %s\n" resstr test2str;
  let res2 = unscramble inputbytes2 ops |> BatBytes.to_string in
  BatPrintf.printf "Part 2: %s unscrambled to %s\n" inputstr2 res2

let _ =
  test ();
  main ()
