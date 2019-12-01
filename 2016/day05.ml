(* % ./day05.native INPUT *)

open Batteries

let is_magic md5 =
  BatChar.code md5.[0] lor BatChar.code md5.[1] lor
    (BatChar.code md5.[2] land 0xF0) = 0

let hexdigits = "0123456789abcdef"
(* 0 <= n <= 15 *)
let of_hexdigit n = hexdigits.[n]
                                         
let find_password seed =
  let password1 = BatBytes.create 8
  and password2 = BatBytes.make 8 '_'
  and idx1 = ref 0
  and idx2 = ref 0
  and i = ref 0 in
  let make n = seed ^ (string_of_int n) in
  while !idx1 < 8 || !idx2 < 8 do
    let md5 = ref (BatDigest.string (make !i)) in
    while not (is_magic !md5) do
      incr i;
      md5 := BatDigest.string (make !i)
    done;
    incr i;
    let char6 = of_hexdigit (BatChar.code !md5.[2] land 0x0F) in
    if !idx1 < 8 then begin
        BatBytes.set password1 !idx1 char6;
        incr idx1
      end;
    if BatChar.is_digit char6 then begin
        let n = BatChar.code char6 - BatChar.code '0' in
        if n < 8 && (BatBytes.get password2 n) = '_' then begin
            let char7 = of_hexdigit (BatChar.code !md5.[3] asr 4) in
            BatBytes.set password2 n char7;
            incr idx2
          end
      end              
  done;
  password1, password2

let finder seed =
  let pass1, pass2 = find_password seed in
  BatPrintf.printf "For %s:\n" seed;
  BatPrintf.printf "1: %s\n" (BatBytes.to_string pass1);
  BatPrintf.printf "2: %s\n" (BatBytes.to_string pass2)

let _ =
  finder Sys.argv.(1)

                

 

              

              
