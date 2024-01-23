open Claudius
let tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let prose1 = "Hello, world!"
let prose2 = "Hello to FieldFX, thanks for the inspirations and encouragement!"

let tick t s fb = 
  let width, height = Screen.dimensions s in
  Framebuffer.shader_inplace (fun _ -> 0) fb;
  match (Screen.font s) with 
  | None -> fb
  | Some font -> (
    let prose1_width = Framebuffer.draw_string 0 0 font prose1 0 fb in
    (* let prose2_width = Framebuffer.draw_string 0 0 font prose2 0 fb in *)
    let slow_t = t / 20 in
    let pos1 = (slow_t mod (width + (1 * prose1_width))) - prose1_width in
    let _ = Framebuffer.draw_string pos1 10 font prose1 (1 + ((slow_t / 10) mod 8)) fb in ();
    for i = 0 to ((String.length prose2) - 1) do
      let ft = (Float.of_int t) /. 100. in
      let c =  String.get prose2 i in
      let _ = Framebuffer.draw_char (width + ((width - slow_t + (i * 15)) mod width)) ((Int.of_float (40. *. sin ((ft +. Float.of_int(i)) *. 0.1))) + (height / 2)) font c 3 fb in ();
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
