open Claudius


let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"

let palette = Palette.load_tic80_palette vapour_palette

(* ----- *)

let distance (x1 : float) (y1 : float) (x2 : float) (y2 : float) : float =
    Float.sqrt((Float.pow (x2 -. x1) 2.) +. (Float.pow (y2 -. y1) 2.))

(* ----- *)

let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0)

let tick t s _p _i =
  let w, h = Screen.dimensions s in
  let palsize = Palette.size (Screen.palette s) in
  Random.init 42;
  let ft = (float_of_int t) /. 50. in
  let fw = (float_of_int w) /. 2.
  and fh = (float_of_int h) /. 2. in
  let x1 = fw +. ((sin ft) *. 130.)
  and y1 = fh -. 100.
  and x2 = fw +. ((cos ft) *. 130.)
  and y2 = fh +. 100. in
  Framebuffer.init (Screen.dimensions s) (fun x y ->
    let fx = float_of_int x and fy = float_of_int y in
    let d1 = 470. /. (distance x1 y1 fx fy)
    and d2 = 470. /. (distance x2 y2 fx fy) in
    let dist = int_of_float(d1 +. d2) in
    let r = Float.rem (d1 +. d2) 1. in
    let m = if r > (Random.float 1.) then 1  else 0 in
    let d = min (dist + m) (palsize - 1) in
    if d < 0 then d + palsize else d
  )

let slide = (palette, boot, tick)
