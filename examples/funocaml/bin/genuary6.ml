open Claudius

let tails = 50

let triangle (x : float) (amplitude : float) (period : float) : float =
  ((2. *. amplitude) /. Float.pi) *. asin (sin (x *. ((2. *. Float.pi) /. period)))

let point d ft =
  let fd = Float.of_int d in
  (d / 2) + Int.of_float (triangle (ft +. Random.float 50.) (fd /. 2.) (50. +. Random.float 100.))

let line t s col =
  Random.init 789123;
  let ft = Float.of_int t in
  let w, h = Screen.dimensions s in
  Primitives.Line (
    { x = point w ft ; y = point h ft },
    { x = point w ft ; y = point h ft },
    col
  )

let boot s = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0)

let tick t s p _i =
  Framebuffer.render p
    (List.concat
    [(if t >= tails then [(line (t - tails) s 0)] else []) ;
    [line t s ((t mod ((Palette.size (Screen.palette s)) -1)) + 1)]]);
  p

let palette = Palette.of_list (0x000000 :: (List.rev (Palette.to_list (Palette.generate_plasma_palette 31))))

let slide = (palette, None, tick)
