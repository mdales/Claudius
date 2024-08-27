open Claudius

let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3ffffff"

let palette = Palette.load_tic80_palette vapour_palette

let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _x _y -> 1)

let tick t s _p i =
  let notshow1 = Base.KeyCodeSet.exists (fun x -> x == 0x00000031) i in
  let notshow2 = Base.KeyCodeSet.exists (fun x -> x == 0x00000032) i in
  let nottime1 = Base.KeyCodeSet.exists (fun x -> x == 0x00000033) i in
  let nottime2 = Base.KeyCodeSet.exists (fun x -> x == 0x00000034) i in

  let ft = (float_of_int t)
  and w, h = (Screen.dimensions s)
  and colors = (Palette.size (palette)) - 1 in
  let fcolors = Float.of_int colors in
  let buffer = Framebuffer.init (w, h) (fun i j ->
    let x = Float.of_int (i - (w / 2))
    and y = Float.of_int (j - (h / 2)) in

    let d1 = (float_of_int w) /. sqrt ((x *. x) +. (y *. y) +. 1.0)
    and c1 = ((atan2 y x) +. Float.pi) *. (fcolors /. (2.0 *. Float.pi)) in

    let c2 = if nottime1 then c1 else c1 +. (sin (ft /. 70.0) *. Float.pi *. 2.0)
    and d2 = if nottime2 then d1 else d1 +. (Float.rem (ft /. 10.0) fcolors) in

    let c2 = if notshow1 then 0.0 else c2 in
    let d2 = if notshow2 then 0.0 else d2 in

    let p = (int_of_float (Float.floor c2)) lxor (int_of_float (Float.floor d2)) in
    let pindex = (p mod colors) in
    if pindex < 0 then (colors + pindex) else  pindex
  ) in

  (match Screen.font s with
    | None -> ()
    | Some font ->(
      if notshow1 then ignore (Framebuffer.draw_string 10 (h - 50) font "d1 = (float_of_int w) /. sqrt ((x *. x) +. (y *. y) +. 1.0)" colors buffer);
      if notshow2 then ignore (Framebuffer.draw_string 10 (h - 50) font "c1 = ((atan2 y x) +. Float.pi) *. (fcolors /. (2.0 *. Float.pi))" colors buffer);
      if (nottime2 == false) && notshow1 then
      ignore (Framebuffer.draw_string 10 (h - 30) font "     +. (Float.rem (ft /. 10.0) fcolors)" colors buffer);
      if (nottime1 == false) && notshow2 then
      ignore (Framebuffer.draw_string 10 (h - 30) font "     +. (sin (ft /. 70.0) *. Float.pi *. 2.0)" colors buffer);
  ));

  Framebuffer.filled_circle (w / 2) (h / 2) 15. 1 buffer;
  buffer

  let slide = (palette, boot, tick)
