open Batteries

type drive = {
    x: int;
    y: int;
    size: int;
    used: int;
    avail: int;
    percent: int
  }

type grid = drive array array

let make_grid x y = BatArray.make_matrix x y { x = -1; y = -1; size = -1;
                                               used = -1; avail = -1;
                                               percent = -1 }
                         
let parse_line line =
  BatScanf.sscanf line "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%"
                  (fun x y sz usd aval perc ->
                    { x = x; y = y; size = sz; used = usd;
                      avail = aval; percent = perc })                          
       
let _ =
  let lines = BatIO.lines_of stdin in
  BatEnum.junk lines;
  BatEnum.junk lines;
  let input = BatEnum.map parse_line lines |> BatList.of_enum in
  let maxx = BatList.fold_left (fun curr drive ->
                 max curr drive.x) 0 input
  and maxy = BatList.fold_left (fun curr drive ->
                 max curr drive.y) 0 input in
  let grid = make_grid (maxx+1) (maxy+1) in
  BatList.iter (fun drive -> grid.(drive.x).(drive.y) <- drive) input;
  let viable = ref 0 in
  for ay = 0 to maxy do
    for ax = 0 to maxx do
      let a = grid.(ax).(ay) in
      for by = 0 to maxy do
        for bx = 0 to maxx do
          let b = grid.(bx).(by) in
          if a.used > 0 && (a <> b) && a.used <= b.avail then
            incr viable;         
          done
        done
    done
  done;
  BatPrintf.printf "Part 1: Viable drive pairs: %d\n" !viable
      
