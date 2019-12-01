(* Let's compile assembunny to C! *)

(* % ./day25.byte < day25.txt > output.c
   % gcc -O -std=c99 output.c
   % ./a.out
 *)

open Batteries

let preamble () =
  print_endline "#include <stdio.h>";
  print_endline "#include <string.h>";
  print_endline "int a=0,b=0,c=0,d=0;";
  print_endline "#define BUFLEN 101";
  print_endline "char output[BUFLEN];";
  print_endline "int o=0;";
  print_endline "_Bool is_repeat(void) {";
  print_endline "for (int i = 0; i < BUFLEN - 1; i += 2) {";
  print_endline "if (!(output[i] == '0' && output[i+1] == '1')) return 0;";
  print_endline "}\nreturn 1;\n}";
  print_endline "_Bool invoke(void) {"
  
let finalize () =
  print_endline "return 0;\n}";
  print_endline "int main(void) {";
  print_endline "int i = 0;";
  print_endline "while (1) {";
  print_endline "o=0; memset(output, 0, BUFLEN);";
  print_endline "a=i; b=0; c=0; d=0;";
  print_endline "if (invoke()) break;";
  print_endline "i += 1;";
  print_endline "}\nprintf(\"%d, output=\\\"%s\\\"\\n\", i, output);\nreturn 0;\n}"


let parse_cpy i line =
  try
    BatScanf.sscanf line "cpy %d %c"
                    (fun x r -> BatPrintf.printf "%c=%d;\n" r x);
    Some true
  with
    BatScanf.Scan_failure _ ->
    try
      BatScanf.sscanf line "cpy %c %c"
                      (fun x r -> BatPrintf.printf "%c=%c;\n" r x);
      Some true
    with BatScanf.Scan_failure _ -> None
                   
let parse_inc i line =
  try
    BatScanf.sscanf line "inc %c"
                    (fun r -> BatPrintf.printf "%c += 1;\n" r);
    Some true
  with BatScanf.Scan_failure _ -> None

let parse_dec i line =
  try
    BatScanf.sscanf line "dec %c"
                    (fun r -> BatPrintf.printf "%c -= 1;\n" r);
    Some true
  with BatScanf.Scan_failure _ -> None

let parse_jnz_cc i line =
  try
    BatScanf.sscanf line "jnz %d %d"
                    (fun x y -> BatPrintf.printf "if (%d != 0) goto instr%d;\n"
                                                 x (i + y));
    Some true
  with BatScanf.Scan_failure _ -> None

let parse_jnz_rc i line =
  try
    BatScanf.sscanf line "jnz %c %d"
                    (fun x y -> BatPrintf.printf "if (%c != 0) goto instr%d;\n"
                                                 x (i + y));
    Some true
  with BatScanf.Scan_failure _ -> None

let parse_jnz i line =
  BatList.fold_left (fun res parser ->
      match res with
      | None -> parser i line
      | _ -> res) None [ parse_jnz_cc; parse_jnz_rc ]

let parse_out i line =
  try
    BatScanf.sscanf line "out %c"
                    (fun r -> BatPrintf.printf "output[o++] = %c + '0';\n" r);
    print_endline "if (o == BUFLEN) return is_repeat();";
    Some true
  with BatScanf.Scan_failure _ -> None
                
let compile_instr i line =
  let parsers = [ parse_cpy; parse_inc; parse_dec; parse_jnz; parse_out ] in  
  BatPrintf.printf "instr%d:\n" i;
  match BatList.fold_left (fun res parser ->
                match res with
                | None -> parser i line
                | x -> x) None parsers with
  | None -> raise (Invalid_argument line)
  | _ -> ()

                
let _ =
  preamble ();
  BatIO.lines_of stdin |> BatEnum.iteri compile_instr;
  finalize ();
  
  
