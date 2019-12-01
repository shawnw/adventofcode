(* % ./day01.byte < day01.txt *)

open Batteries

type pair = { x : int; y : int }
type direction = North | East | West | South
type rotate = Left | Right


let new_direction d r = 
  match d with
  | North when r = Left -> West
  | North -> East
  | East when r = Left -> North
  | East -> South
  | South when r = Left -> East
  | South -> West
  | West when r = Left -> South
  | West -> North

let to_direction = function
  | 'L' -> Left
  | 'R' -> Right
  | _ -> raise (Invalid_argument "Direction must be L or R")

let adjust dir dist pos =
  match dir with
  | North -> { pos with y = pos.y + dist }
  | East -> { pos with x = pos.x + dist }
  | South -> { pos with y = pos.y - dist }
  | West -> { pos with x = pos.x - dist }
              
let move dir (d, pos) =
  BatScanf.sscanf dir " %c%d "
                  (fun way dist ->
                    let r = to_direction way in
                    let newdir = new_direction d r in
                    (newdir, adjust newdir dist pos))
                  
let taxi orig dest = Astar.taxi_distance orig.x orig.y dest.x dest.y

let rec add_points cache orig dest =
  if orig = dest then
    (false, orig, cache)
  else begin
    let newpoint = 
    if orig.x < dest.x then
      { orig with x = orig.x + 1 }
    else if orig.x > dest.x then
      { orig with x = orig.x - 1 }
    else if orig.y < dest.y then
      { orig with y = orig.y + 1 }
    else if orig.y > dest.y then
      { orig with y = orig.y - 1 } 
    else
      orig in
    if BatSet.mem newpoint cache then
      (true, newpoint, cache)
    else
      add_points (BatSet.add newpoint cache) newpoint dest
  end

let first_repeat dirs loc =
  let points = BatSet.add (snd loc) BatSet.empty in
  let rec helper loc cache = function
    | hd :: tl ->
      let (newdir, newloc) = move hd loc in
      let (found, repeat, cache) = add_points cache (snd loc) newloc in
      if found then
	repeat
      else
	helper (newdir, newloc) cache tl
    | [] -> raise (Invalid_argument "No repeated coordinates!") in
  helper loc points dirs

let main () = 
  let startc = {x = 0; y = 0 }
  and currc = ref (North, { x = 0; y = 0 })
  and dirs = BatString.nsplit (input_all Pervasives.stdin) "," in
  BatList.iter (fun d -> currc := move d !currc) dirs;
  BatPrintf.printf "Ending coordinates: x = %d, y = %d\n" (snd !currc).x (snd !currc).y;
  BatPrintf.printf "Distance: %d\n" (taxi startc (snd !currc));
  let repeat = first_repeat dirs (North, { x = 0; y = 0}) in
  BatPrintf.printf "First repeated coordinate: x = %d, y = %d\n" repeat.x repeat.y;
  BatPrintf.printf "Distance: %d\n" (taxi startc repeat)

let _ = main ()
      
