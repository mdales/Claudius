open Claudius

(* ----- *)

let drawpoly (x : int) (y : int) (r : int) (sides : int) (a : float) (col : int) =
  let segment_angle = (2. *. Float.pi) /. (Float.of_int sides) in
  let fr = Float.of_int r in
  let rec loop = (fun (i : int) (rest) ->
    let angle = ((Float.of_int i) *. segment_angle) +. a in
    let next : Primitives.point = {
      x = Int.of_float (fr *. sin angle) ;
      y = Int.of_float (fr *. cos angle) ;
    } in
    match i with
    | 0 -> next :: rest
    | _ -> next :: loop (i - 1) rest
  ) in 
  let points = loop (sides - 1) [] in
  let shifted = List.map (fun (p : Primitives.point) : Primitives.point -> 
    {
      x = p.x + x ;
      y = p.y + y ;
    }
  ) points in
  Primitives.Polygon (shifted, col)
  
(* ----- *)

let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)
  
let tick t s prev =
  let w, h = Screen.dimensions s in
  (* Fade what came before *)
  let buffer = Framebuffer.shader (fun pixel ->
    match pixel with
    | 0 -> pixel
    | 16 -> 0
    | _ -> (pixel - 1)
  ) prev in
  (* let buffer = Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)  in *)

  let sides = (((t / 300) mod 5) + 3) in
  let col = (t mod 15) + (if sides == 6 then 16 else 0) in
  let hex = drawpoly ((t / 5) mod w) (h/2) 60 sides ((Float.of_int t) /. -50.) col in
  Framebuffer.render buffer [hex];

  buffer

(* ----- *)

let () =
  let pal = List.rev (Palette.to_list (Palette.generate_mono_palette 16)) in
  let redpal = List.map (fun x -> x lor 0xff0000) pal in
  let screen = Screen.create 240 136 3 (Palette.of_list (List.concat [pal ; redpal])) in
  Tcc.run "Genuary Day 10: Hexagons" (Some boot) tick screen
