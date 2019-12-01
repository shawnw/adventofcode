open Batteries

type register = Reg of int

type op = CopyR of (register * register) | CopyC of (int * register)
          | Add of (register * int) | JnzR of (register * int)
          | JnzC of (int * int)                            

let char_to_reg = function
  | 'a' | 'A' -> Reg 0
  | 'b' | 'B' -> Reg 1
  | 'c' | 'C' -> Reg 2
  | 'd' | 'D' -> Reg 3
  | _ -> raise (Invalid_argument "No such register")

let parse_cpy line =
  try
    BatScanf.sscanf line "cpy %d %c"
                    (fun c dr -> Some (CopyC (c, char_to_reg dr)))
  with
  | BatScanf.Scan_failure _ ->
     try
       BatScanf.sscanf line "cpy %c %c"
                       (fun sr dr ->
                         Some (CopyR (char_to_reg sr, char_to_reg dr)))
     with
     | BatScanf.Scan_failure _ -> None

let parse_inc line =
  try
    BatScanf.sscanf line "inc %c"
                    (fun r -> Some (Add (char_to_reg r, 1)))
  with
  | BatScanf.Scan_failure _ ->
     try
       BatScanf.sscanf line "dec %c"
                       (fun r -> Some (Add (char_to_reg r, -1)))
     with
     | BatScanf.Scan_failure _ -> None

let parse_jnz line =
  try
    BatScanf.sscanf line "jnz %d %d"
                    (fun x y -> Some (JnzC (x, y)))
  with
  | BatScanf.Scan_failure _ ->
     try
       BatScanf.sscanf line "jnz %c %d"
                       ( fun x y -> Some (JnzR (char_to_reg x, y)))
     with
     | BatScanf.Scan_failure _ -> None

let parse line =
  let ops = [ parse_cpy; parse_inc; parse_jnz ] in
  let instr = BatList.fold_left (fun oper f ->
                  match oper with
                  | Some _ -> oper
                  | None -> f line) None ops in
  BatOption.get instr

                
let eval sp instrs registers =
  match instrs.(!sp) with
  | CopyC (x, Reg y) -> registers.(y) <- x
  | CopyR (Reg r, Reg y) ->
     registers.(y) <- registers.(r)
  | Add (Reg x, y) ->
     registers.(x) <- registers.(x) + y
  | JnzR (Reg x, y) ->
     if registers.(x) <> 0 then
       sp := !sp + y - 1
  | JnzC (x, y) ->
     if x <> 0 then
       sp := !sp + y - 1
                
let _ =
  let sp = ref 0
  and registers = BatArray.make 4 0
  and instrs = BatIO.lines_of stdin |> BatEnum.map parse |> BatArray.of_enum in
  while !sp < BatArray.length instrs do
    eval sp instrs registers;    
    incr sp
  done;
  BatPrintf.printf "Part 1: a = %d\n" registers.(0);
  sp := 0;
  BatArray.fill registers 0 4 0;
  registers.(2) <- 1;
  while !sp < BatArray.length instrs do
    eval sp instrs registers;
    incr sp
  done;
  BatPrintf.printf "Part 2: a = %d\n" registers.(0);
  
                   
                   
  
                                
                                  
                      
     
  
           
                              

                                

                 
