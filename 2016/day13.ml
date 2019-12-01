open Batteries

type location = Open | Wall

let magic = ref 10 (* 1358 *)
              
let location_type x y =
  let v = x*x + 3*x + 2*x*y + y + y*y + !magic in
  let bits = BatInt.popcount v in
  if bits mod 2 == 0 then
    Open
  else
    Wall

type point = { x : int; y : int }

let get_neighbors p =
  let neighbors = ref [] in
  if p.x > 0 then
    neighbors := { p with x = p.x -  1 } :: !neighbors;
  neighbors := { p with x = p.x + 1 } :: !neighbors;
  if p.y > 0 then
    neighbors := { p with y = p.y - 1 } :: !neighbors;
  neighbors := { p with y = p.y + 1 } :: !neighbors;
  BatList.filter (fun np -> match location_type np.x np.y with
                            | Open -> true
                            | Wall -> false) !neighbors
  |> BatList.map (fun np -> np, 1)
               
module PointAstar =
  Astar.Make(struct
        type t = point
        let guess_distance p1 p2 =
          Astar.taxi_distance p1.x p1.y p2.x p2.y
        let neighbors = get_neighbors
        let compare (a:t) (b:t) = compare a b
        let equal (a:t) (b:t) = a = b
        let hash = BatHashtbl.hash
      end)
                                       
let rec expand n tbl (prev: point list) =
  if n = 0 then
    BatHashtbl.length tbl
  else
    begin
      let neighbors =
        BatList.map get_neighbors prev
        |> BatList.concat
        |> BatList.filter_map (fun (p,_) ->
               if BatHashtbl.mem tbl p then
                 None
               else
                 begin
                   BatHashtbl.add tbl p true;
                   Some p
                 end) in
      expand (n - 1) tbl neighbors
    end

let rec print_path = function
  | [] -> print_newline ();
  | hd :: [] -> BatPrintf.printf "(%d,%d)\n" hd.x hd.y
  | hd :: tl ->
     BatPrintf.printf "(%d,%d) -> " hd.x hd.y;
     print_path tl

let _ =
  let starting_point = { x = 1; y = 1 } in
  let (test_path, test_len) = PointAstar.find starting_point { x = 7; y = 4 } in
  BatPrintf.printf "Test: Shortest path length is %d\n" test_len;
  print_path test_path;  
  magic := 1358;
  let part1_len = PointAstar.distance starting_point { x = 31; y = 39 } in
  BatPrintf.printf "Part 1: Shortest path length is %d\n" part1_len;
  let part2_table = BatHashtbl.create 1000 in
  BatHashtbl.add part2_table starting_point true;
  let part2_count = expand 50 part2_table [ starting_point ] in
  BatPrintf.printf "Part 2: Rooms reachable in 50 steps: %d\n" part2_count
                  

                        
