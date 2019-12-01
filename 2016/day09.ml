(* % ./day09.byte < day09.txt *)

open Batteries

(* Returns characters up to ch from an enum, discards ch *)
let chars_up_to input ch =
  let pref = BatEnum.take_while (fun c -> c <> ch) input in
  let s = BatString.of_enum pref in
  BatEnum.junk input;
  s
                    
let decodev1 input =
  let output = BatBuffer.create 500 in
  let rec helper () =
    match BatEnum.get input with
    | None -> BatBuffer.contents output
    | Some ch ->
       if ch = '(' then begin
           let len = chars_up_to input 'x' |> int_of_string
           and rep = chars_up_to input ')' |> int_of_string in
           let s = BatEnum.take len input |> BatString.of_enum in
           for n = 1 to rep do
             BatBuffer.add_string output s
           done;
         end
       else if not @@ BatChar.is_whitespace ch then
         BatBuffer.add_char output ch;
       helper ()
  in
  helper ()

let testsv1 = [ "ADVENT", 6; "A(1x5)BC", 7; "(3x3)XYZ", 9;
              "A(2x2)BCD(2x2)EFG", 11; "(6x1)(1x3)A", 6;
              "X(8x2)(3x3)ABCY", 18 ]

let rec decodev2 input =
  let rec helper len =
    match BatEnum.get input with
    | None -> len
    | Some ch ->
       if ch = '(' then begin
           let rlen = chars_up_to input 'x' |> int_of_string
           and rep = chars_up_to input ')' |> int_of_string in
           let s = BatEnum.take rlen input in
           let slen = decodev2 s in
           helper (len + (slen * rep))
         end
       else if BatChar.is_whitespace ch then begin
         helper len
       end else
         helper (len + 1)
  in
  helper 0

let testsv2 = [ "(3x3)XYZ", 9; "X(8x2)(3x3)ABCY", 20;
                "(27x12)(20x12)(13x14)(7x10)(1x12)A", 241920;
                "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN", 445
              ]
                                
let run_testsv1 () =
  print_endline "Part 1 tests:";
  BatList.iteri (fun i (s, l) ->
      let d = BatString.enum s |> decodev1 in
      BatPrintf.printf "Test %d: '%s', expected length %d, actual length %d\n"
                       (i + 1) s l (BatString.length d)) testsv1

let run_testsv2 () =
  print_endline "Part 2 tests:";
  BatList.iteri (fun i (s, l) ->
      let dl = BatString.enum s |> decodev2 in
      BatPrintf.printf "Test %d: '%s', expected length %d, actual length %d\n"
                       (i + 1) s l dl) testsv2
                      
let _ =
  run_testsv1 ();
  run_testsv2 ();
  let input = BatIO.read_all stdin in
  let inputv1 = BatString.enum input in
  let inputv2 = BatEnum.clone inputv1 in
  let resultv1 = decodev1 inputv1 |> BatString.length
  and resultv2 = decodev2 inputv2 in
  BatPrintf.printf "Part 1: %d\n" resultv1;
  BatPrintf.printf "Part 2: %d\n" resultv2
