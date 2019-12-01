(* % ./day10.byte < day10.txt *)

open Batteries

type destination = Bot of int | Output of int
                                            
type bot = {
    id: int;
    mutable low: destination;
    mutable high: destination;
    mutable chips: int list;
  }

let make_bot id =
  { id = id; low = Output (-1); high = Output (-1); chips = [] }
             
module Bots = BatMap.Make(Int)

let try_parse fmt f line =
  try
    BatScanf.sscanf line fmt (fun a b c -> Some (f a b c))
  with
  | BatScanf.Scan_failure _ -> None

type instruction = Initialize of int * int | Distribute of int * destination * destination
                                 
let parse_init line = try_parse "value %d goes to bot %d %d" (fun v b _ -> Initialize (b,v)) (line ^ " 0")
let parse_distbb = try_parse "bot %d gives low to bot %d and high to bot %d"
                             (fun b bl bh -> Distribute (b, Bot bl, Bot bh))
let parse_distoo = try_parse "bot %d gives low to output %d and high to output %d"
                             (fun b ol oh -> Distribute (b, Output ol, Output oh))
let parse_distbo = try_parse "bot %d gives low to bot %d and high to output %d"
                             (fun b bl oh -> Distribute (b, Bot bl, Output oh))
let parse_distob = try_parse "bot %d gives low to output %d and high to bot %d"
                             (fun b ol bh -> Distribute (b, Output ol, Bot bh))
                         
let build bots line =
  let insts = [ parse_init; parse_distbb; parse_distoo; parse_distbo; parse_distob ] in
  match BatList.fold_right (fun p -> function
                                    | Some _ as i -> i
                                    | None -> p line) insts None with
  | Some (Initialize (bot, chip)) ->
     Bots.modify_def (make_bot bot) bot
                     (fun b -> b.chips <- chip :: b.chips; b) bots
  | Some (Distribute (bot, low, high)) ->
     let lowv = match low with Bot b -> b | _ -> -1
     and highv = match high with Bot b -> b | _ -> -1 in
     let addlow = Bots.modify_def (make_bot lowv) lowv identity bots in
     let addhigh = Bots.modify_def (make_bot highv) highv identity addlow in
     Bots.modify_def (make_bot bot) bot
                     (fun b ->
                       b.low <- low;
                       b.high <- high;
                       b) addhigh
  | None -> raise (Invalid_argument line)

let find_bot bots lowval highval =
  let test id bot =
    BatList.mem lowval bot.chips && BatList.mem highval bot.chips in
  Bots.filter test bots |> Bots.enum |> BatEnum.get

let outputs = BatHashtbl.create 50
                                          
let cycle bots =
  let twochips = Bots.filterv (fun bot -> BatList.length bot.chips = 2) bots |> Bots.enum in
  BatEnum.fold (fun c (id, bot) ->
      let a, b = (BatList.at bot.chips 0), (BatList.at bot.chips 1) in
      let low, high = if a < b then a,b else b,a in
      bot.chips <- [];
      begin match bot.low with
      | Output o -> BatHashtbl.modify_def low o (fun _ -> low) outputs
      | Bot b ->
         let newbot = Bots.find b bots in
         newbot.chips <- low :: newbot.chips
      end;
      begin match bot.high with
      | Output o -> BatHashtbl.modify_def high o (fun _ -> high) outputs
      | Bot b ->
         let newbot = Bots.find b bots in
         newbot.chips <- high :: newbot.chips
      end) () twochips;
  bots
    
let rec cycle_until bots cmpfound outsfound =
  if not (cmpfound && outsfound) then begin
      let cf = ref cmpfound
      and ouf = ref outsfound in
      if not cmpfound then begin
        match find_bot bots 17 61 with
        | Some (id, bot) -> BatPrintf.printf "Part 1: %d\n" id;
                            cf := true
        | None -> ()
        end;
      if not outsfound then begin
          match BatHashtbl.find_option outputs 0,
                BatHashtbl.find_option outputs 1,
                BatHashtbl.find_option outputs 2 with
          | Some a, Some b, Some c ->
             BatPrintf.printf "Part 2: %d\n" (a * b * c);
             ouf := true
          | _ -> ()
        end;
      let b = cycle bots in
      cycle_until b !cf !ouf
    end

let dest_to_string = function
  | Bot b -> "Bot " ^ (string_of_int b)
  | Output o -> "Output " ^ (string_of_int o)

let chips_to_string chips =
  let buf = BatBuffer.create 10 in
  BatBuffer.add_char buf '[';
  BatList.iter (fun chip -> BatBuffer.add_string buf (string_of_int chip);
                            BatBuffer.add_string buf " ") chips;
  BatBuffer.add_char buf ']';
  BatBuffer.contents buf
                              
let print_bots =
  Bots.iter (fun id bot ->
      BatPrintf.printf "Bot %d: Low to %s, high to %s, starts with %s\n"
                       id (dest_to_string bot.low) (dest_to_string bot.high)
                       (chips_to_string bot.chips))
                 
let _ =
  let bots = BatIO.lines_of stdin |> BatEnum.fold build Bots.empty in
  (*  print_bots bots; *)
  cycle_until bots false false
  
