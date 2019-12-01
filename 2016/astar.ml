let taxi_distance x1 y1 x2 y2 =
  BatInt.abs (x1 - x2) + BatInt.abs (y1 - y2)

module type LocationType = sig
  type t
  val guess_distance: t -> t -> int
  val neighbors: t -> (t * int) list
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
end

module Make(Location:LocationType) : sig
  val distance: Location.t -> Location.t -> int
  val path: Location.t -> Location.t -> Location.t list
  val find: Location.t -> Location.t -> Location.t list * int
end = struct

  exception Found of Location.t  

  module Hash = BatHashtbl.Make(Location)

  let find_score tbl p =
    Hash.find_default tbl p BatInt.max_num

  let cmp_dist (a, _) (b, _) = BatInt.compare a b

  let rec rev_and_append lst trail =
    match lst with
    | [] -> trail
    | hd :: tl -> rev_and_append tl (hd :: trail)
                                              
  let insert_sorted v lst =
    let rec helper acc = function
      | [] -> List.rev (v :: acc)
      | hd :: tl as rest when cmp_dist v hd < 0 ->
         rev_and_append acc (v :: rest)
      | hd :: tl -> helper (hd :: acc) tl in
    helper [] lst
     (*
    BatList.merge cmp_dist lst [v]
      *)

  let remove_all lst item =
    lst := BatList.filter (fun (_, loc) -> not @@ Location.equal item loc) !lst
           
  let min_score candidates distances =
    let (_, loc) =
      BatList.find (fun (_, loc) -> Hash.mem candidates loc) !distances in
    loc
                                       
  let build_path came_from current =
    let total_path = ref [ current ] in
    let point = ref current in
    while Hash.mem came_from !point do
      point := Hash.find came_from !point;
      total_path := !point :: !total_path;
    done;
    !total_path
                       
  let astar start goal =
    let initial_guess = Location.guess_distance start goal in
    let visited = Hash.create initial_guess
    and candidates = Hash.create initial_guess
    and came_from = Hash.create initial_guess
    and guess_distances = ref [ initial_guess, start ]
    and calculated_distances = Hash.create initial_guess in
    Hash.add candidates start true;
    Hash.add calculated_distances start 0;
  try
    while not @@ Hash.is_empty candidates do
      let current = min_score candidates guess_distances in
      if (Location.compare current goal) = 0 then
        raise (Found current);
      Hash.remove candidates current;
      remove_all guess_distances current;
      Hash.add visited current true;
      let neighbors = Location.neighbors current    
      and tentative_score = find_score calculated_distances current in
      BatList.iter (fun (p, dist) ->
          let tentative_score = tentative_score + dist in
          if not @@ Hash.mem visited p then
            begin
              if not @@ Hash.mem candidates p then
                Hash.add candidates p true;
              if tentative_score < find_score calculated_distances p then
                begin
                  Hash.add came_from p current;
                  Hash.add calculated_distances p tentative_score;
                  guess_distances :=
                    insert_sorted
                      (tentative_score + (Location.guess_distance p goal), p)
                      !guess_distances
                end              
            end) neighbors;
    done;
    None
  with Found p -> Some (build_path came_from p, Hash.find calculated_distances p)

  let distance start goal =
    match astar start goal with
    | None -> raise Not_found
    | Some (_, dist) -> dist

  let path start goal =
    match astar start goal with
    | None -> raise Not_found
    | Some (path, _) -> path

  let find start goal =
    match astar start goal with
    | None -> raise Not_found
    | Some x -> x
                          
end
