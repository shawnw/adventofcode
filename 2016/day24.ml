open Batteries
          
type point = Floor | Wall

let is_open = function Floor -> true | Wall -> false

let locations = ref []

let comploc (a,_,_) (b,_,_) = compare a b

let rec genperms = function
  | [] -> []
  | hd :: [] -> [[hd]]
  | lst ->
     BatList.fold_left (fun acc x ->
         acc @ BatList.map
                 (fun p -> x :: p) (genperms (BatList.remove lst x)))
                       [] lst
                    
let to_number ch =
  (BatChar.code ch) - (BatChar.code '0')   
                    
let to_point ch x y=
  match ch with
  | '#' -> Wall
  | '.' -> Floor
  | '0' .. '9' ->
     locations := (to_number ch, x, y) :: !locations;
     Floor
  | _ -> raise (Invalid_argument (BatPrintf.sprintf "Unknown tile '%c'" ch))
             
let parse lines =  
  BatEnum.mapi (fun y line ->
      let row = BatArray.make (BatString.length line) Wall in
      BatString.iteri (fun x ch -> row.(x) <- to_point ch x y) line;
      row) lines |> BatArray.of_enum

let grid = BatIO.lines_of stdin |> parse

module ShortestPath =
  Astar.Make(struct
              type t = int * int
              let guess_distance (x1,y1) (x2,y2) =
                Astar.taxi_distance x1 y1 x2 y2
              let neighbors (x,y) =
                let spots = ref [] in
                if x > 0 && is_open grid.(y).(x - 1) then
                  spots := ((x - 1, y), 1) :: !spots;
                if x < BatArray.length grid.(y) - 1
                   && is_open grid.(y).(x + 1) then
                  spots := ((x + 1, y), 1) :: !spots;
                if y > 0 && is_open grid.(y-1).(x) then
                  spots := ((x, y - 1), 1) :: !spots;
                if y < BatArray.length grid - 1
                   && is_open grid.(y + 1).(x) then
                  spots := ((x, y + 1), 1) :: !spots;
                !spots
              let compare = compare
              let equal = (=)
              let hash = BatHashtbl.hash
            end)

let cache = BatHashtbl.create 100
            
let shortest_path start allspots =
  let mindist = ref BatInt.max_num in
  BatList.iter (fun spots ->
      let dist = ref 0 in
      let _ = 
        BatList.fold_left (fun (s1,x1,y1) (s2,x2,y2) ->
            if !dist < !mindist then
              begin
                let pair = min s1 s2, max s1 s2 in
                match BatHashtbl.find_option cache pair with
                | Some d -> dist := !dist + d
                | None ->
                   let d = ShortestPath.distance (x1,y1) (x2,y2) in
                   BatHashtbl.add cache pair d;
                   dist := !dist + d
              end;
            (s2,x2,y2)) start spots in
            mindist := min !mindist !dist;
    ) allspots;
  !mindist
            
let _ =
  let start = BatList.find (fun (a,_,_) -> a = 0) !locations in
  let spots = BatList.remove !locations start in
  let allspots = genperms spots in
  BatPrintf.printf "Looking at routes through %d locations. There are %d permutations.\n%!" (BatList.length spots) (BatList.length allspots);
  let d = shortest_path start allspots in
  BatPrintf.printf "Part 1: Shortest total distance %d\n%!" d;
  let allspots2 = BatList.map (fun lst -> lst @ [start]) allspots in
  let d = shortest_path start allspots2 in
  BatPrintf.printf "Part 2: Shortest distance ending back at 0: %d\n" d
