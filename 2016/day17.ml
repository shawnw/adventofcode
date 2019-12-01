open Batteries

type point = { x : int; y : int }

let start_room = { x = 0; y = 0 }
let end_room = { x = 3; y = 3 }

let is_open = function
  | 'b' | 'c' | 'd' | 'e' | 'f' -> true
  | _ -> false
                 
exception Found of string

let find_neighbors room hash =
  let neighbors = ref [] in
  if is_open hash.[0] && room.x > 0 then
    neighbors := ({ room with x = room.x - 1 }, 'U') :: !neighbors;
  if is_open hash.[1] && room.x < 3 then
    neighbors := ({ room with x = room.x + 1 }, 'D') :: !neighbors;
  if is_open hash.[2] && room.y > 0 then
    neighbors := ({ room with y = room.y - 1 }, 'L') :: !neighbors;
  if is_open hash.[3] && room.y < 3 then
    neighbors := ({room with y = room.y + 1 }, 'R') :: !neighbors;
  !neighbors                                                        

module Shortest =
  struct
    type t = point * string * string
                                
    let guess_distance (a, _, _) (b, _, _) =
      Astar.taxi_distance a.x a.y b.x b.y
                                          
    let neighbors (room, pathsofar, seed) =
      let hash =
        BatDigest.string (seed ^ pathsofar) |> BatDigest.to_hex in
      find_neighbors room hash
      |> BatList.map (fun (r,d) ->
             ((r, (BatPrintf.sprintf "%s%c" pathsofar d), seed), 1))
                     
    let compare (a, _, _) (b, _, _) = compare a b
    let equal = (=)
    let hash = BatHashtbl.hash 
  end
    
module ShortestPath = Astar.Make(Shortest)
                                
(* Shortest path *)
let pathfind seed =
  let p = ShortestPath.path (start_room, "", seed) (end_room, "", seed) in
  let (_, path, _) = BatList.last p in
  path

(* Longest path *)   
let pathfind2 seed =
  let lengths = ref 0 in
  let rec helper room pathsofar =
    if room = end_room then
      lengths := BatInt.max (BatString.length pathsofar) !lengths
    else
      begin
        let hash = BatDigest.string (seed ^ pathsofar) |> BatDigest.to_hex in
        let neighbors = find_neighbors room hash in
        let newpathlen = BatString.length pathsofar + 1 in
        let buf = BatBuffer.create newpathlen in
        BatList.iter (fun (r,d) ->
            BatBuffer.add_string buf pathsofar;
            BatBuffer.add_char buf d;
            helper r (BatBuffer.contents buf);
            BatBuffer.reset buf) neighbors
      end    
  in
  helper start_room "";
  !lengths

      
let _ = 
  let test1 = pathfind "ihgpwlah" in
  BatPrintf.printf "Test 1.1: %s\n" test1;
  let test2 = pathfind "kglvqrro" in
  BatPrintf.printf "Test 1.2: %s\n" test2;
  let test3 = pathfind "ulqzkmiv" in
  BatPrintf.printf "Test 1.3: %s\n" test3;
  let part1 = pathfind "pslxynzg" in
  BatPrintf.printf "Part 1: %s\n" part1;
  let test1 = pathfind2 "ihgpwlah" in
  BatPrintf.printf "Test 2.1: %d\n" test1;
  let test2 = pathfind2 "kglvqrro" in
  BatPrintf.printf "Test 2.2: %d\n" test2;
  let test3 = pathfind2 "ulqzkmiv" in
  BatPrintf.printf "Test 2.3: %d\n" test3;
  let part2 = pathfind2 "pslxynzg" in
  BatPrintf.printf "Part 2: %d\n" part2
                   
  
                   
                   
