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
  Primitives.Polygon (shifted, col)

let poly_points poly =
  match poly with
  | Primitives.Polygon (list, _) -> list
  | _ -> []

let generate_star (x : int) (y : int) (r1 : int) (r2 : int) (sides : int) (a : float) (col : int) =
  let s1 = generate_poly x y r1 sides a col |> poly_points in
  let s2 = generate_poly x y r2 sides (a +. ((Float.pi *. 2.) /. ((Float.of_int sides) *. 2.))) col |> poly_points in
  let mixed = List.concat (List.map2 (fun a b -> [b ; a]) s1 s2) in
  Primitives.Polygon (mixed, col)

let tick t _s p =
  Framebuffer.shader_inplace (fun _ -> 0) p;
  let stars = List.init 4 (fun i -> 
    List.init 4 (fun j -> 
      generate_star ((i * 131 * 2) + ((t / 10) mod 262) - 262) ((j * 131 * 2) + ((t / 10) mod 262) - 262) 100 131 8 ((2. *. Float.pi) /. 16.) 15
    )
  ) in
  Framebuffer.render p (List.concat stars);
  p


let () = 
  Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 16))) |>
  Screen.create 640 480 1 |>
  Base.run "Genuary Day 17: Islamic Patterns" None tick