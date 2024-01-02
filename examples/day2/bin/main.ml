open Claudius

(* ----- *)

let tick (t : int) (screen : Tcc.screen) (_prev : Framebuffer.t) : Framebuffer.t =
  let palsize = Palette.size screen.palette in
  Framebuffer.init screen.width screen.height (fun x y -> 
      let ft = (Float.of_int t) /. 10. and fx = (Float.of_int x) /. 140. and fy = (Float.of_int y) /. 140. in
      let z = 10.0 +. (sin (ft /. 1000.0) *. 5.0)
      and d = 10.0 +. (cos (ft /. 1000.0) *. 5.0) in
      let fc = (sin (sin ((fx +. ft) /. z)) +. sin (sin ((fy +. ft) /. d))) *. Float.of_int(palsize / 2) in
      let rc = ((int_of_float fc)) mod palsize in
      if rc >= 0 then rc else (rc + palsize)
  )

(* ----- *)

let () =
  let screen : Tcc.screen = {
    width = 640 ;
    height = 480 ;
    scale = 1 ;
    palette = Palette.generate_plasma_palette 1024 ;
  } in
  Tcc.tcc_init screen "Genuary Day 2: No Palette" None tick
