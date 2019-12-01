open Batteries

let flip = function
  | '0' -> '1'
  | '1' -> '0'
  | _ -> raise Not_found

let rev_map f s =
  let len = BatString.length s in
  let res = BatBytes.create len in
  let r = ref 0 in
  for n = len - 1 downto 0 do
    f s.[n] |> BatBytes.set res (len - n - 1)
  done;
  res
    
let dragon a =
  let buf = BatBuffer.create (BatString.length a * 2 + 1) in
  BatBuffer.add_string buf a;
  BatBuffer.add_char buf '0';
  rev_map flip a |> BatBuffer.add_bytes buf;
  BatBuffer.contents buf

let rec checksum s  =
  let len = BatString.length s / 2 in
  let buf = BatBuffer.create len in
  for n = 0 to len - 1 do
    let a = s.[n * 2]
    and b = s.[(n * 2) + 1] in
    if a = b then
      BatBuffer.add_char buf '1'
    else
      BatBuffer.add_char buf '0'
  done;
  let chk = BatBuffer.contents buf in
  if String.length chk mod 2 = 1 then
    chk
  else
    checksum chk

let find_checksum seed len =
  let s = ref seed in
  while BatString.length !s < len do
    s := dragon !s
  done;
  let raw = BatString.sub !s 0 len in
  checksum raw


let _ =
  let input = "00101000101111010" in
  BatPrintf.printf "Test checksum: %s\n"
                   (find_checksum "10000" 20);
  BatPrintf.printf "Part 1 checksum: %s\n"
                   (find_checksum input 272);
  BatPrintf.printf "Part 2 checksum: %s\n"
                   (find_checksum input 35651584)
                   
