open Batteries

(* Heavily based on Day 12 code, as suggested by the problem *)


type register = A | B | C | D
       
type param = Reg : register -> param
           | Const : int -> param

type op = Copy of (param * param)  | Inc of param | Dec of param
          | Jnz of (param * param) | Toggle of param | Mul of (param * param)

let r2i = function
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
                                                                
let char_to_reg = function
  | 'a' | 'A' -> Reg A
  | 'b' | 'B' -> Reg B
  | 'c' | 'C' -> Reg C
  | 'd' | 'D' -> Reg D
  | x -> raise (Invalid_argument (BatPrintf.sprintf "No such register: %c" x))

let param_to_string = function
  | Reg A -> "a"
  | Reg B -> "b"
  | Reg C -> "c"
  | Reg D -> "d"
  (* | Reg _ -> raise (Invalid_argument "Unknown register value") *)
  | Const n -> string_of_int n
               
let op_to_string = function
  | Copy (x, y) -> BatPrintf.sprintf "cpy %s %s"
                                     (param_to_string x)
                                     (param_to_string y)
  | Inc x -> BatPrintf.sprintf "inc %s" (param_to_string x)
  | Dec x -> BatPrintf.sprintf "dec %s" (param_to_string x)
  | Jnz (x, y) -> BatPrintf.sprintf "jnz %s %s"
                                    (param_to_string x)
                                    (param_to_string y)
  | Toggle x -> BatPrintf.sprintf "tgl %s" (param_to_string x)
  | Mul (x, y) -> BatPrintf.sprintf "mul %s %s"
                                    (param_to_string x)
                                    (param_to_string y)
                               
let parse_cpy line =
  try
    BatScanf.sscanf line "cpy %d %c"
                    (fun c dr -> Some (Copy (Const c, char_to_reg dr)))
  with
  | BatScanf.Scan_failure _ ->
     try
       BatScanf.sscanf line "cpy %c %c"
                       (fun sr dr ->
                         Some (Copy (char_to_reg sr, char_to_reg dr)))
     with
     | BatScanf.Scan_failure _ -> None

let parse_inc line =
  try
    BatScanf.sscanf line "inc %c"
                    (fun r -> Some (Inc (char_to_reg r)))
  with
  | BatScanf.Scan_failure _ ->
     try
       BatScanf.sscanf line "dec %c"
                       (fun r -> Some (Dec (char_to_reg r)))
     with
     | BatScanf.Scan_failure _ -> None

let parse_jnz line =
  try
    BatScanf.sscanf line "jnz %d %d"
                    (fun x y -> Some (Jnz (Const x, Const y)))
  with
  | BatScanf.Scan_failure _ ->
     try
       BatScanf.sscanf line "jnz %d %c"
                       ( fun x y ->
                         Some (Jnz (Const x, char_to_reg y)))
     with
     | BatScanf.Scan_failure _ ->
        try
          BatScanf.sscanf line "jnz %c %d"
                          ( fun x y -> Some (Jnz (char_to_reg x, Const y)))
        with
        | BatScanf.Scan_failure _ -> None
                                       

let parse_tgl line =
  try
    BatScanf.sscanf line "tgl %c" (fun r -> Some (Toggle (char_to_reg r)))
  with
  | BatScanf.Scan_failure _ -> None
                                    
let parse line =
  let ops = [ parse_cpy; parse_inc; parse_jnz; parse_tgl ] in
  match BatList.fold_left (fun oper f ->
                  match oper with
                  | Some _ -> oper
                  | None -> f line) None ops with
  | None -> BatPrintf.printf "Unable to parse line: %s\n" line;
            exit 1;
  | Some op -> op

let toggle instrs ip =
  if ip >= 0 && ip < BatArray.length instrs then
    begin
      BatPrintf.printf "Toggling instruction %d from '%s' to "
                       ip (op_to_string instrs.(ip));
      let newinstr = 
        match instrs.(ip) with
        (* Single argument ops: inc becomes dec, all others become inc *)
        | Inc x ->  Dec x
        | Dec x | Toggle x -> Inc x
        (* Two argument ops: jnz becomes cpy, all others become jnz *)
        | Jnz x -> Copy x
        | Copy x | Mul x -> Jnz x in
      instrs.(ip) <- newinstr;
      BatPrintf.printf "'%s'\n" (op_to_string newinstr)
    end
                
let eval sp instrs registers =
  match instrs.(!sp) with
  | Copy (Const x, Reg y) -> registers.(r2i y) <- x
  | Copy (Reg r, Reg y) -> registers.(r2i y) <- registers.(r2i r)
  | Inc (Reg x) -> registers.(r2i x) <- registers.(r2i x) + 1
  | Dec (Reg x) -> registers.(r2i x) <- registers.(r2i x) - 1
  | Jnz (Reg x, Const y) ->
     if registers.(r2i x) <> 0 then
       sp := !sp + y - 1
  | Jnz (Const x, Const y) ->
     if x <> 0 then
       sp := !sp + y - 1
  | Jnz (Const x, Reg y) ->
     if x <> 0 then
       sp := !sp + registers.(r2i y) - 1
  | Toggle (Reg x) ->
     let ip = !sp + registers.(r2i x) in
     toggle instrs ip
  | Toggle (Const x) ->
     let ip = !sp + x in
     toggle instrs ip
  | Mul (Reg x, Reg y) ->
     registers.(r2i y) <- registers.(r2i y) * registers.(r2i x)
  | Mul (Const x, Reg y) ->
     registers.(r2i y) <- registers.(r2i y) * x
  | op ->  (* Ignore invalid ops *)
     let opstr = op_to_string op in
     BatPrintf.printf "Invalid instruction at %d: %s\n" !sp opstr
                      
let _ =
  let sp = ref 0
  and registers = BatArray.make 4 0
  and instrs = BatIO.lines_of stdin |> BatEnum.map parse |> BatArray.of_enum in
  let orig_instrs = BatArray.copy instrs in
  registers.(0) <- 7;    
  while !sp < BatArray.length instrs do
    eval sp instrs registers;    
    incr sp
  done;
  BatPrintf.printf "Part 1: a = %d\n%!" registers.(0);
  sp := 0;
  let instrs = orig_instrs in 
  BatArray.fill registers 0 4 0;
  registers.(0) <- 12;
  while !sp < BatArray.length instrs do
    eval sp instrs registers;
    incr sp
  done;
  BatPrintf.printf "Part 2: a = %d\n" registers.(0);
                   
