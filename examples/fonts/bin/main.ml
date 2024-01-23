open Claudius
let tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let prose1 = "Hello, world!"

let tick t s fb = 
  Framebuffer.shader_inplace (fun _ -> 0) fb;
  match (Screen.font s) with 
  | None -> fb
  | Some font -> (
    let slow_t = t / 10 in
    let _ = Framebuffer.draw_string (240 - slow_t) 10 font prose1 (1 + ((slow_t / 10) mod 8)) fb in ();
    for i = 0 to ((String.length prose1) - 1) do
      let ft = (Float.of_int t) /. 100. in
      let c =  String.get prose1 i in
      let _ = Framebuffer.draw_char (240 - slow_t + (i * 15)) ((Int.of_float (40. *. sin (ft +. Float.of_int(i)))) + 80) font c 3 fb in ();
    done;
    fb
  )

let () = 
  match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    Palette.load_tic80_palette tic80_palette |>
    Screen.create_with_font 240 136 3 font |>
    Base.run "Font testing" None tick
  )
