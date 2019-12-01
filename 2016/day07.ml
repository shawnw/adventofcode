(* % ./day07.byte < day07.txt *)

open Batteries

       (* Let's be different and not use regular expressions! *)

exception Hypernet
exception Found
            
let has_tls addr =
  let in_hypernet = ref false
  and abba = ref false in
  try
    for n = 0 to BatString.length addr - 4 do
      if addr.[n] = '[' then
        in_hypernet := true
      else if addr.[n] = ']' then
        in_hypernet := false
      else if addr.[n] = addr.[n+3] && addr.[n+1] = addr.[n+2] &&
                addr.[n] <> addr.[n+1] then begin
          if !in_hypernet then
            raise Hypernet
          else
            abba := true
        end
    done;
    !abba
  with
  | Hypernet -> false

let rec bracket_helper str pos what =
  let obpos = BatString.index_from str pos '[' in
  let cbpos = BatString.index_from str obpos ']'
  and whatpos = BatString.find_from str obpos what in
  if whatpos < cbpos then
    true
  else
    bracket_helper str (cbpos + 1) what

let in_brackets str what =
  try
    bracket_helper str 0 what
  with
  | Not_found -> false
                                
let has_ssl addr =
  let in_hypernet = ref false in
  try
    for n = 0 to BatString.length addr - 3 do
      if addr.[n] = '[' then
        in_hypernet := true
      else if addr.[n] =']' then
        in_hypernet := false
      else if addr.[n] = addr.[n+2] && addr.[n] <> addr.[n+1] &&
                !in_hypernet = false && BatChar.is_letter addr.[n+1] then
          let bab = Bytes.create 3 in
          Bytes.set bab 0 addr.[n+1];
          Bytes.set bab 1 addr.[n];
          Bytes.set bab 2 addr.[n+1];
          if in_brackets addr (Bytes.to_string bab) then
            raise Found
    done;
    false
  with
  | Found -> true
                  
                                       
let tlstests = [ "abba[mnop]qrst";
                 "abcd[bddb]xyyx";
                 "aaaa[qwer]tyui";
                 "ioxxoj[asdfgh]zcxvbn"
               ]

let ssltests = [ "aba[bab]xyz";
                 "xyx[xyx]xyx";
                 "aaa[kek]eke";
                 "zazbz[bzb]cdb";
                 "aba[xyz]bab[cat]dog"
               ]
                 
let run_tests () =
  print_endline "has_tls:";
  BatList.iter (fun addr ->
      Printf.printf "%s: %B\n" addr (has_tls addr)) tlstests;
  print_endline "has_ssl:";
  BatList.iter (fun addr ->
      Printf.printf "%s: %B\n" addr (has_ssl addr)) ssltests

let _ =
  run_tests ();
  let tls_enum = BatIO.lines_of stdin in
  let ssl_enum = BatEnum.clone tls_enum in
  let tls_count = BatEnum.filter has_tls tls_enum |> BatEnum.count
  and ssl_count = BatEnum.filter has_ssl ssl_enum |> BatEnum.count in
  Printf.printf "Supports TLS: %d\nSupports SSL: %d\n" tls_count ssl_count
  
                
                  
      
                     
