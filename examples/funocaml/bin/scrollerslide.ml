open Claudius

let palette = Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let boot s = 
  Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0)

let tick prose t s _fb _i =
  let w, h = Screen.dimensions s in
  let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in (
  match (Screen.font s) with
  | None -> ()
  | Some font -> (
    ignore (Framebuffer.draw_string (w - (t mod (w + 200))) (h / 2) font prose 12 fb)
  ));
  fb

let generate_slide prose = (palette, boot, tick prose)
