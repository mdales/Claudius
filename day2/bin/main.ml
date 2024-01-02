open Claudius

(* ----- *)

let tick (_t : int) (_screen : Tcc.screen) (prev : Framebuffer.t) : Framebuffer.t =
  let buffer = Array.map (fun row -> 
    Array.map (fun pixel ->
      if pixel > 2 then (pixel - 2) else 0
    ) row
  ) prev in

  buffer

(* ----- *)

let () =
  let screen : Tcc.screen = {
    width = 640 ;
    height = 480 ;
    scale = 1 ;
    palette = Palette.generate_mono_palette 16 ;
  } in
  Tcc.tcc_init screen "Genuary Day 2: No Palette" None tick
