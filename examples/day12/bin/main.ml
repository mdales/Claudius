open Claudius

let tick t s _p =
  Random.init 42;
  let ft = (Float.of_int t) /. 20. in
  Framebuffer.init (Screen.dimensions s) (fun x y ->
    let fx = Float.of_int x and fy = Float.of_int y in
    let c = ((sin ((sin (ft /. 23.)) +. fx /. 150.) *. 1.) +. (atan(((fy +. fx) /. 70.) -. ft)) *. 1.) in
    (* let c = 0. in *)
    let d = ((sin ((sin (ft /. 23.)) +. fy /. 250.) *. 1.) +. (sin((fx /. 150.) -. ft)) *. 1.) in
    (* let d = 0. in *)
    let e = ((cos ((sin (fx /. 103.)) +. ft /. 250.) *. 1.) *. (sin((ft /. 10.) -. 0.)) *. 3.) in 
    (* let ic = Int.of_float (c +. d +. e) in *)
    (* let mc = ic mod 16 in  *)
    (* if ic < 0 then 0 else ic *)
    let col = c +. d +. e in
    let r = Float.rem (col) 1. in
    let m = if r > (Random.float 1.) then 1  else 0 in
    (Int.of_float col) + m 
    (* if (c > d) then 2 else 1 *)
  ) 
  (* |>
  Framebuffer.shaderi (fun x y buffer ->
    let pixels = [
      (-1, -1) ;
      (0, -1) ;
      (1, -1) ;
      (-1, 0) ;
      (0, 0) ;
      (1, 0) ;
      (-1, 1) ;
      (0, 1) ;
      (1, 1) ;
    ] in
    let vals = List.filter_map (fun p -> 
      let a, b = p in
      Framebuffer.pixel_read (x + a) (y + b) buffer
    ) pixels in
    let fvals = List.map Float.of_int vals in
    let avg = (List.fold_left Float.add 0. fvals) /. (Float.of_int (List.length fvals)) in
    Int.of_float avg
  ) *)

let () = 
  Palette.generate_plasma_palette 16 |>
  Screen.create 480 640 1 |>
  Tcc.run "Genuary 12/13: Lava Lamp/Wobbly Functions" None tick
  