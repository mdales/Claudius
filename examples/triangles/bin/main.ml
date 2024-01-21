open Claudius

let draw_triangle x0 y0 x1 y1 x2 y2 col fb =
  Framebuffer.draw_line x0 y0 x1 y1 col fb;
  Framebuffer.draw_line x1 y1 x2 y2 col fb;
  Framebuffer.draw_line x2 y2 x0 y0 col fb

let tick t s fb =
  Framebuffer.shader_inplace (fun _p -> 0) fb;
  let width, height = Screen.dimensions s
  and ft = (Float.of_int t) /. 100.
  and col = (Palette.size (Screen.palette s)) -1 in
  Framebuffer.draw_circle (width / 2) (height / 2) 150. (col / 4) fb;
  Framebuffer.draw_circle (width / 2) (height / 2) 100. (col / 3) fb;
  Framebuffer.draw_circle (width / 2) (height / 2) 50. (col / 2) fb;

  draw_triangle 
    ((Int.of_float (50. *. (sin (ft)))) + (width / 2))
    ((Int.of_float (50. *. (cos (ft)))) + (height / 2))
    ((Int.of_float (150. *. (sin (ft +. (Float.pi *. 1.5))))) + (width / 2))
    ((Int.of_float (150. *. (cos (ft +. (Float.pi *. 1.5))))) + (height / 2))
    ((Int.of_float (100. *. (sin (ft +. 2.)))) + (width / 2))
    ((Int.of_float (100. *. (cos (ft +. 2.)))) + (height / 2))
    col fb;
  fb

let () = 
  Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 16))) |>
  Screen.create 640 480 1 |>
  Base.run "Triangle testing" None tick
