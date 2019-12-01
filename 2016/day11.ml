open Batteries

type building = { elv: int;
                  floors: BatInt64.t;
                }

let build_floor g m =
  Int64.logxor (Int64.shift_left (Int64.of_int g) 8) (Int64.of_int m)
                   
let build_floors g3 m3 g2 m2 g1 m1 g0 m0  =
  Int64.logxor (Int64.shift_left (build_floor g3 m3) 48)
            (Int64.logxor (Int64.shift_left (build_floor g2 m2) 32)
                       (Int64.logxor (Int64.shift_left (build_floor g1 m1) 16)
                                  (build_floor g0 m0)))
            
let part1_start_state = {
    elv = 0;
    floors = build_floors 0 0
                          0 0
                          0 0b0000_1100
                          0b0001_1111 0b0001_0011
  }
                  
let part1_end_state = {
    elv = 3;
    floors = build_floors 0b0001_1111 0b0001_1111
                          0 0
                          0 0
                          0 0
  }

let part2_start_state = {
    elv = 0;
    floors = build_floors 0 0
                          0 0
                          0 0b0000_1100
                          0b0111_1111 0b0111_0011
  }
                  
let part2_end_state = {
    elv = 3;
    floors = build_floors 0b0111_1111 0b0111_1111
                          0 0
                          0 0
                          0 0
  }
                        
let test_start_state =
  {
    elv = 0;
    floors = build_floors 0 0
                          0b0000_1000 0
                          0b0001_0000 0
                          0 0b0001_1000
  }

let test_end_state = {
    elv = 3;
    floors = build_floors 0b0001_1000 0b0001_1000
                          0 0
                          0 0
                          0 0
  }

let get_floor bld n =
  BatInt64.logand (BatInt64.shift_right_logical bld.floors (n * 16)) 0xFFFFL

let get_floori bld n = get_floor bld n |> BatInt64.to_int 
                                            
let get_chipsi bld n =
  (get_floori bld n) land 0x00FF

let get_gensi bld n =
  ((get_floori bld n) land 0xFF00) lsr 8

let get_chips bld n =
  Int64.logand (get_floor bld n) 0x00FFL

let get_gens bld n =
  Int64.shift_right_logical (Int64.logand (get_floor bld n) 0xFF00L) 8

let floor_of_chip bld ch =
  let bit = 1 lsl ch in
  if get_floori bld 0 land bit > 0 then
    0
  else if get_floori bld 1 land bit > 0 then
    1
  else if get_floori bld 2 land bit > 0 then
    2
  else
    3

let floor_of_gen bld g =
  let bit = 1 lsl (g + 8) in
  if get_floori bld 0 land bit > 0 then
    0
  else if get_floori bld 1 land bit > 0 then
    1
  else if get_floori bld 2 land bit > 0 then
    2
  else
    3

let print_building bld =
  BatPrintf.printf "{ elv = %d" bld.elv;
  for n = 0 to 3 do
    BatPrintf.printf "; floor%d.chips = 0x%X; floor%d.gens = 0x%X"
                     n (get_chipsi bld n) n (get_gensi bld n)
  done;
  print_char '}'

let move_to_floor bld fl mask =
  let mask = BatInt64.of_int mask in
  let stripped =
    BatInt64.logand bld.floors (BatInt64.lognot
                                  (BatInt64.shift_left mask (bld.elv * 16))) in
  {
    elv = fl;
    floors = BatInt64.logor stripped (BatInt64.shift_left mask (fl * 16))
  }

let invalid_state bld flr =
  let chips = get_chipsi bld flr
  and gens = get_gensi bld flr in
  if chips == 0 || gens == 0 then
    false
  else
    (chips land (lnot gens)) <> 0
  
let empty_below bld =
  match bld.elv with
  | 0 -> false
  | 1 -> BatInt64.logand bld.floors 0xFFFF_FFFF_FFFF_0000L = bld.floors
  | 2 -> BatInt64.logand bld.floors 0xFFFF_FFFF_0000_0000L = bld.floors
  | 3 -> BatInt64.logand bld.floors 0xFFFF_0000_0000_0000L = bld.floors
  | _ -> raise (Invalid_argument "Floor out of range")
    
let states = BatHashtbl.create 256

let _ =
  BatHashtbl.add states 0 ([], []);
  for n = 1 to 255 do
    let ones = ref []
    and twos = ref [] in
    for m = 1 to n do
      if n land m = m then
        match BatInt.popcount m with
        | 1 -> ones := m :: !ones
        | 2 -> twos := m :: !twos
        | _ -> ()
    done;
    BatHashtbl.add states n (!ones, !twos)
  done

type direction = Up | Down

let normalize bld =
  let pairs = ref [] in
  for n = 0 to 7 do
    pairs := (floor_of_chip bld n, floor_of_gen bld n) :: !pairs
  done;
  BatList.sort compare !pairs |>
    BatList.fold_left (fun n (chip,gen) ->
        BatInt64.logor
          (BatInt64.logor
             (BatInt64.shift_left (BatInt64.of_int chip) 3)
             (BatInt64.of_int gen))
          (BatInt64.shift_left n 6)
      ) (BatInt64.of_int bld.elv)

  
module Hash64 = BatHashtbl.Make(
                    struct
                      type t = int64
                      let equal (a:int64) (b:int64) = a = b
                      let hash = BatHashtbl.hash
                    end)
  
(*module Hash64 = BatHashtbl*)
let transitions bld dir =
  let flr = match dir with | Up -> bld.elv + 1 | Down -> bld.elv - 1 in
  if flr < 0 || flr > 3 then
    []
  else if dir = Down && empty_below bld then
    []    
  else
    begin
      let chips = get_chipsi bld bld.elv
      and gens = get_gensi bld bld.elv in
      let (cones, ctwos) = BatHashtbl.find states chips
      and (gones, gtwos) = BatHashtbl.find states gens in
      let shift = fun mask -> mask lsl 8 in
      let gones = BatList.map shift gones
      and gtwos = BatList.map shift gtwos in
      let pairs = BatList.filter_map (fun mask ->
                      if gens land mask == mask then
                        Some (mask lor (mask lsl 8))
                      else
                        None) cones in
      let moves = ctwos @ gtwos @ cones @ gones @ pairs in
      let normalized = Hash64.create (BatList.length moves) in
      BatList.filter_map (fun mask ->
          let newbld = move_to_floor bld flr mask in
          let normbld = normalize newbld in
          if Hash64.mem normalized normbld then
            None
          else if invalid_state newbld bld.elv || invalid_state newbld flr then
            begin
              Hash64.add normalized normbld true;
              None
            end
          else
            begin
              Hash64.add normalized normbld true;
              Some newbld
            end) moves
    end

let print_hist choices =
  print_char '(';              
  BatList.iter (fun choice -> print_building choice; print_char ' ') choices;
  print_endline ")"

let choices dir prev visited =
  BatList.map (fun (bld, _) -> transitions bld dir) prev
  |> BatList.concat 
  |> BatList.filter_map
       (fun bld ->
         let normbld = normalize bld in
         if Hash64.mem visited normbld then
           None
         else
           Some (bld, normbld))

let rec search goal prev depth visited =
  let upchoices = choices Up prev visited in
  if BatList.mem_assoc goal upchoices then
      depth
  else
    begin           
      let downchoices = choices Down prev visited in
      let choices =
        upchoices @ downchoices
        |> BatList.unique_cmp ~cmp:(fun (_, norma) (_, normb)
                                    -> BatInt64.compare norma normb)
      in
      if BatList.is_empty choices then
          -1
      else
        begin
          let visited = Hash64.create (BatList.length prev) in 
          BatList.iter (fun (_,normbld)  ->
              Hash64.add visited normbld true) prev;
          search goal choices (depth + 1) visited
        end
    end

let run start goal  =
  let visited = Hash64.create 10 in
  Hash64.add visited (normalize start) true;
  search goal [start, normalize start] 1 visited
                       
let test () = run test_start_state test_end_state
let part1 () = run part1_start_state part1_end_state
let part2 () = run part2_start_state part2_end_state

let _ =
  BatPrintf.printf "Test: %d\n" (test ());
  flush stdout;
  BatPrintf.printf "Part 1: %d\n" (part1 ());
  flush stdout;
  BatPrintf.printf "Part 2: %d\n" (part2 ());
  ()
                    
  
    


  
