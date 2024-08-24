open Claudius

let prism_radius = 100.
let rainbow = [0xE03A3C ; 0xF6821F ; 0xFCB827 ; 0x62BB47 ; 0x009DDC ; 0x963D97]

let draw_light s fb = 
  let w, h = Screen.dimensions s in
  for i = 0 to 2 do
    Framebuffer.draw_line 0 ((h / 2) - 30 + i) (w/2) ((h / 2) - 100 + i) 7 fb
  done

let draw_rainbow s fb = 
  let w, h = Screen.dimensions s in
  for i = 0 to 5 do
    Framebuffer.filled_triangle 
      (w / 2) (((h / 2) - 100 + i))
      w (((h / 2) - 80 + (i * 10)))
      w (((h / 2) - 79 + ((i + 1) * 10)))
      (8 + i)
      fb
  done

let draw_flair s fb = 
  let w, h = Screen.dimensions s in
  let mx, my = ((w / 2) - 42), (((h / 2) - 86)) in
  for i = ((8 * 5) - 1) downto 0 do
    Framebuffer.filled_triangle
      mx my
      (mx + i) (my - ((Int.of_float ((Float.of_int (i + 1)) *. sin 0.25)) + 2))
      (mx + i) (my + ((Int.of_float ((Float.of_int (i + 1)) *. sin 0.25)) + 2))
      (7 - ((i + 1) / 5))
      fb
  done

let draw_prism s fb =
  let w, h = Screen.dimensions s in
  let mx, my = (w / 2, ((2 * h) / 5)) in

  for i = 7 downto 0 do
    Framebuffer.filled_triangle
      (mx + Int.of_float((prism_radius +. (Float.of_int (i * 2))) *. sin 0.))
      (my - Int.of_float((prism_radius +. (Float.of_int (i * 2))) *. cos 0.))
      (mx + Int.of_float((prism_radius +. (Float.of_int (i * 2))) *. sin (2. *. Float.pi /. 3.)))
      (my - Int.of_float((prism_radius +. (Float.of_int (i * 2))) *. cos (2. *. Float.pi /. 3.)))
      (mx - Int.of_float((prism_radius +. (Float.of_int (i * 2))) *. sin (2. *. Float.pi /. 3.)))
      (my - Int.of_float((prism_radius +. (Float.of_int (i * 2))) *. cos (2. *. Float.pi /. 3.)))
      i
      fb
  done

let boot s =
  let fb = Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0) in
  draw_light s fb;
  draw_rainbow s fb;
  draw_prism s fb;
  draw_flair s fb;
  fb

let tick _t _s fb _i = fb

let () =
  Palette.of_list ((Palette.to_list (Palette.generate_mono_palette 8)) @ (rainbow)) |>
  Screen.create 480 480 1 |>
  Base.run "Dark Side Of The Moon" (Some boot) tick
