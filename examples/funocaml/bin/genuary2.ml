open Claudius

let tick t s _prev _i =
  let palsize = Palette.size (Screen.palette s) in
  Framebuffer.init (Screen.dimensions s) (fun x y ->
      let ft = (Float.of_int t) /. 10. and fx = (Float.of_int x) /. 140. and fy = (Float.of_int y) /. 140. in
      let z = 10. +. (sin (ft /. 1000.) *. 5.)
      and d = 10. +. (cos (ft /. 1000.) *. 5.) in
      let fc = (sin (sin ((fx +. ft) /. z)) +. sin (sin ((fy +. ft) /. d))) *. Float.of_int(palsize / 2) in
      let rc = ((int_of_float fc)) mod palsize in
      if rc >= 0 then rc else (rc + palsize)
  )

let palette = Palette.generate_plasma_palette 1024

let slide = (palette, None, tick)
