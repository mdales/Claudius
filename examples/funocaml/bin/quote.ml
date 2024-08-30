open Claudius

let prose = "\"The enemy of art is the absence of limitations.\""
let author = "Orson Wells"

let prose_font = Result.get_ok (
  Bdfparser.Bdf.create "/Users/michael/dev/classic-mac-fonts/bdf/Geneva-24.bdf"
)
let author_font = Result.get_ok (
  Bdfparser.Bdf.create "/Users/michael/dev/classic-mac-fonts/bdf/Times-14.bdf"
)

let tick t s _p _i =
  let _w, h = Screen.dimensions s in
  let palsize = (Palette.size (Screen.palette s) - 1) in

  (* let fb = Framebuffer.init (w, h) (fun _ _ -> ((t / 100) mod 16)) in*)
  Random.init 42;

  let fb = Framebuffer.init (Screen.dimensions s) (fun x y ->
      let ft = (Float.of_int t) /. 10. and fx = (Float.of_int x) /. 140. and fy = (Float.of_int y) /. 140. in
      let z = 10. +. (sin (ft /. 1000.) *. 5.)
      and d = 10. +. (cos (ft /. 1000.) *. 5.) in
      let fc = (sin (sin ((fx +. ft) /. z)) +. sin (sin ((fy +. ft) /. d))) *. Float.of_int(palsize / 2) in

      let r = Float.rem fc 1. in
      let m = if r > (Random.float 1.) then 1  else 0 in

      let rc = ((int_of_float fc) + m) mod palsize in
      (if rc >= 0 then rc else (rc + palsize)) + 1
  ) in

  let w = Textslide.draw_string 20 (h / 2) prose_font prose 0 fb in
  ignore(Textslide.draw_string (w - 50) ((h / 2) + 20) author_font author 0 fb);
  fb


let palette = Palette.of_list (0 :: (Palette.to_list (Palette.generate_plasma_palette 16)))

let slide = (palette, None, tick)
