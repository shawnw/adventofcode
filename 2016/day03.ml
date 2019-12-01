(* % ./day03.byte < day03.txt *)

open Batteries

type triangle = { a: int; b : int; c : int }

let make_triangle a' b' c' = { a = a'; b = b'; c = c' }

let triangle_of_string str =
  BatScanf.sscanf str " %d %d %d" make_triangle
                                              
let read_triangles_by_col source =
  let make_triangles a1 a2 a3 b1 b2 b3 c1 c2 c3 =
    BatList.enum [ make_triangle a1 b1 c1; make_triangle a2 b2 c2;
      make_triangle a3 b3 c3 ] in
  let str = BatEnum.take 3 source |>
              BatEnum.fold (fun b s -> BatBuffer.add_string b s;
                                       BatBuffer.add_char b ' ';
                                       b)
                           (BatBuffer.create 100) |> BatBuffer.contents in  
  BatScanf.sscanf str " %d %d %d %d %d %d %d %d %d" make_triangles
  
let is_triangle tri =
  (tri.a + tri.b > tri.c) && (tri.a + tri.c > tri.b)
  && (tri.b + tri.c > tri.a)

let part1 source =
  let tris =
    BatEnum.fold (fun sum line ->
        if triangle_of_string line |> is_triangle then
          sum + 1
        else
          sum) 0 source in
  BatPrintf.printf "Part 1: %d\n" tris

let part2 source =
  let tris = ref 0 in
  while not @@ BatEnum.is_empty source do
    tris := !tris + BatEnum.count (read_triangles_by_col source // is_triangle)
  done;
  print_string "Part 2: ";
  print_int !tris;
  print_newline ()
                                 
let _ =
  let part1_enum = BatIO.lines_of stdin in
  let part2_enum = BatEnum.clone part1_enum in
  part1 part1_enum;
  part2 part2_enum
          
                            
