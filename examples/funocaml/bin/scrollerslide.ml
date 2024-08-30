open Claudius

let palette = Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let tick prose t s _fb _i =
  let ft = float_of_int t in
  let w, h = Screen.dimensions s in
  let font = Option.get (Screen.font s) in
  let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in
  let sl = List.init (String.length prose) (String.get prose) in
  List.iteri (fun i c ->
    let xpos = (w - ((t * 3) mod (w + 200)) + (i * 12)) in
    let fxpos = float_of_int xpos in
    let ypos = (h / 2) + (int_of_float ((sin (fxpos /. 30.)) *.  (10. +. (40. *. (cos (ft /. 100.)))))) in
    ignore (Framebuffer.draw_char xpos ypos font c 12 fb)
  ) sl;
  fb

let generate_slide prose = (palette, None, tick prose)
