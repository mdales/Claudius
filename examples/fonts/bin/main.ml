open Claudius

let tick t s fb = 
  Framebuffer.shader_inplace (fun _ -> 0) fb;
  match (Screen.font s) with 
  | None -> fb
  | Some font -> (
    let prose = "Hello, world!" in
    let slow_t = t / 10 in
    Framebuffer.draw_string (240 - slow_t) 10 font prose 15 fb; 
    fb
  )

let () = 
  match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 16))) |>
    Screen.create_with_font 240 136 3 font |>
    Base.run "Font testing" None tick
  )
