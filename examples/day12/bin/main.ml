open Claudius

let tick t s _p =
  Random.init 42;
  let ft = (Float.of_int t) /. 20. in
  Framebuffer.init (Screen.dimensions s) (fun x y ->
    let fx = Float.of_int x and fy = Float.of_int y in
    let c = ((sin ((sin (ft /. 23.)) +. fx /. 150.) *. 1.) +. (atan(((fy +. fx) /. 70.) -. ft)) *. 1.)
    and d = ((sin ((sin (ft /. 23.)) +. fy /. 250.) *. 1.) +. (sin((fx /. 150.) -. ft)) *. 1.)
    and e = ((cos ((sin (fx /. 103.)) +. ft /. 250.) *. 1.) *. (sin((ft /. 10.) -. 0.)) *. 3.) in 
    let col = c +. d +. e in
    let r = Float.rem (col) 1. in
    let m = if r > (Random.float 1.) then 1 else 0 in
    (Int.of_float col) + m 
  ) 

let () = 
  Palette.generate_plasma_palette 16 |>
  Screen.create 480 640 1 |>
  Tcc.run "Genuary 12/13: Lava Lamp/Wobbly Functions" None tick
  