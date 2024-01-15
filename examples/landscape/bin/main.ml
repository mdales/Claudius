open Claudius

let tick (t : int) (screen : Screen.t) (_prev : Framebuffer.t) : Framebuffer.t =
  Random.init 42;
  let ft = (Float.of_int t) *. 0.02 
  and width, height = Screen.dimensions screen in
  let buffer = Framebuffer.init (width, height) (fun _x _y -> 0) in

  for z = 0 to 76 do
    let fz = (Float.of_int z) *. 8. in
    for x = -100 to 100 do 
      let fx = ((Float.of_int x) *. 3.) in
    
      let prey = ((sin ((sin (ft /. 2.)) +. fx /. 50.) *. 3.) +. (sin((fz /. 50.) -. ft)) *. 3.) in
      let y = if prey > 0. then prey *. 1.5 else prey in
      let px = (width / 2) + Int.of_float (fx /. (fz *. 1.8 -. 1200.) *. 1200.) 
      and py = (Int.of_float ((y -. 40.) /. (fz *. 1.8 -. 1200.) *. 1200.)) + 100
      and col = (Int.of_float y) + 6 in
      let dither = if (Float.rem y 1.) > (Random.float 1.) then 1 else 0 in
      let dot = (20. /. (78. -. (fz /. 8.))) in

     Framebuffer.filled_circle px py dot (col + dither) buffer
    done
  done;
  buffer

(* ----- *)

let () =
  Palette.of_list (0x666666 :: (Palette.to_list (Palette.generate_plasma_palette 15))) |>
  Screen.create 640 480 1 |>
  Base.run "TCC Day 9 extra" None tick