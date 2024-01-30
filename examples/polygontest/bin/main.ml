open Claudius

let generate_poly (x : int) (y : int) (r : int) (sides : int) (a : float) (col : int) =
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
  Primitives.FilledPolygon (shifted, col)

let tick t s fb = 
  Framebuffer.shader_inplace (fun _ -> 0) fb;
  let w, h = Screen.dimensions s in
  let ft = Float.of_int t in
  let p = generate_poly (w/2) (h/2) 100 (3 + ((t /500) mod 5)) (ft /. 500.) 15 in
  Framebuffer.render fb [p];
  fb


let () = 
  Palette.of_list (0xffffff :: (Palette.to_list (Palette.generate_plasma_palette 15))) |>
  Screen.create 640 480 1 |>
  Base.run "Polygon test" None tick