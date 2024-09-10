open Claudius

let tick t s _p _i =
  Random.init 42;
  let w, h = Screen.dimensions s in
  let ft = (Float.of_int t) *. 0.02 in
  let buffer = Framebuffer.init (w, h) (fun _x _y -> 0) in

  for z = 0 to 76 do
    let fz =  (Float.of_int z) *. 8. in
    for x = -100 to 100 do
      let fx = ((Float.of_int x) *. 3.) in

      let prey = ((sin ((sin (ft /. 2.)) +. (fx +. 0.0) /. 50.) *. 3.) +. (sin((fz /. 50.) -. ft)) *. 3.) in
      let y = if prey > 0. then prey *. 1.5 else prey in
      let px = (w / 2) + Int.of_float ((fx +.0.0) /. (fz *. 1.8 -. 1200.) *. 1200.)
      and py = (Int.of_float ((y -. 40.) /. (fz *. 1.8 -. 1200.) *. 1200.)) + 100
      and col = (Int.of_float y) + 7 in
      let dither = if (Float.rem y 1.) > (Random.float 1.) then 1 else 0 in
      let dot = (20. /. (78. -. (fz /. 8.))) in

      Framebuffer.filled_circle px py dot (col + dither) buffer
    done
  done;

  let font = Option.get (Screen.font s) in
  ignore(Framebuffer.draw_string (max 10 (680 - (t * 10))) 10 font "Learning OCaml with Tiny Code Christmas & Genuary" 1 buffer);
  ignore(Framebuffer.draw_string (max 10 (780 - (t * 10))) 30 font "Fun OCaml 2024" 1 buffer);
  ignore(Framebuffer.draw_string (max 10 (880 - (t * 10))) 70 font "Michael Dales" 1 buffer);
  ignore(Framebuffer.draw_string (max 10 (980 - (t * 10))) 90 font "@michael@mynameismwd.org" 1 buffer);

  buffer

let palette = Palette.of_list (0x222222 :: 0xFFFFFF :: (Palette.to_list (Palette.generate_plasma_palette 15)))

let slide = (palette, None, tick)
