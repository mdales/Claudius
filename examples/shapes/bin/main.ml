open Claudius

let tick t _s prev _i =
  let buffer = Framebuffer.shader (fun pixel -> 
    match pixel with 
    | 0 -> 0
    | x -> x - 1
  ) prev in

  let ft = (Float.of_int t) /. 50. in

  Framebuffer.draw_line 
    (Int.of_float(50. *. sin ft) + 100)
    (Int.of_float(50. *. cos ft) + 100)
    (Int.of_float(-50. *. sin ft) + 100)
    (Int.of_float(-50. *. cos ft) + 100)
    15 buffer;

  for i = 49 downto 0 do
    Framebuffer.filled_circle 210 100 (Float.of_int i) ((i + (t / 10)) mod 16) buffer;
    Framebuffer.filled_rect (430 - i) (100 - i) ((i * 2)) ((i * 2)) ((i + (t / 10)) mod 16) buffer
  done;

  let v = (t/5 ) mod 50 in
  Framebuffer.draw_rect (270 + v) (50 + v) (100 - (v * 2)) (100 - (v * 2)) 15 buffer; 

  buffer

let () = 
  Palette.generate_mono_palette 16 |>
  Screen.create 640 480 1 |>
  Base.run "Shapes test" None tick
