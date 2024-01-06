open Claudius

(* type point = {
  x : int ;
  y : int ;
  vx : int; 
  xy : int;
} *)

let tails = 50


let triangle (x : float) (amplitude : float) (period : float) : float =
  ((2. *. amplitude) /. Float.pi) *. asin (sin (x *. ((2. *. Float.pi) /. period)))


let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)

let line t s col = 
  Random.init 789123;
  let ft = Float.of_int t in
  let w, h = Screen.dimensions s in
  let fh = Float.of_int h 
  and fw = Float.of_int w in
  Primitives.Line (
    {
      x = (w / 2) + Int.of_float (triangle (ft +. Random.float 50.) (fw /. 2.) (50. +. Random.float 100.)) ;
      y = (h / 2) + Int.of_float (triangle (ft +. Random.float 50.) (fh /. 2.) (50. +. Random.float 100.))
    }, 
    {
      x = (w / 2) + Int.of_float (triangle (ft +. Random.float 50.) (fw /. 2.) (50. +. Random.float 100.)) ;
      y = (h / 2) + Int.of_float (triangle (ft +. Random.float 50.) (fh /. 2.) (50. +. Random.float 100.))
    }, 
    col
  )

let tick t s p = 
  if t >= tails then (
    let l = line (t - tails) s 0 in
    Framebuffer.render p [l];
  );
  let l = line t s ((t mod ((Palette.size (Screen.palette s)) -1)) + 1) in
  Framebuffer.render p [l];
  p

let () =
  let pal = 0x000000 :: (List.rev (Palette.to_list (Palette.generate_plasma_palette 31))) in
  let screen = Screen.create 512 384 1 (Palette.of_list pal) in
  Tcc.run screen "Genuary Day 6: Screen saver" (Some boot) tick
