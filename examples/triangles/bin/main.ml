open Claudius

let _draw_triangle x0 y0 x1 y1 x2 y2 col fb =
  Framebuffer.draw_line x0 y0 x1 y1 col fb;
  Framebuffer.draw_line x1 y1 x2 y2 col fb;
  Framebuffer.draw_line x2 y2 x0 y0 col fb


let interpolate_line (x0 : int) (y0 : int) (x1 : int) (y1 : int) : int list =
  let dx = abs (x1 - x0)
  and sx = if x0 < x1 then 1 else -1
  and dy = (abs (y1 - y0)) * -1
  and sy = if y0 < y1 then 1 else -1 in
  let initial_error = dx + dy in
    
  let rec loop (x : int) (y : int) (error : int) (tail : int list) : int list =
    match (x == x1) && (y == y1) with
    | true -> tail
    | false -> (
      let e2 = 2 * error in
      let nx = match e2 >= dy with
      | false -> x
      | true -> x + sx in
      let ny = match e2 <= dx with
      | false -> y
      | true -> y + sy in
      let nex = match (e2 >= dy) with
      | false -> 0
      | true -> dy in
      let ney = match (e2 <= dx) with
      | false -> 0
      | true -> dx in
      loop nx ny (error + nex + ney) (if ny == y then tail else nx :: tail)
    )
  in List.rev (loop x0 y0 initial_error [x0])

let filled_triangle x0 y0 x1 y1 x2 y2 col fb font =
  let points = [(x0, y0) ; (x1, y1) ; (x2, y2)] in
  let sorted_points = List.sort (fun a b -> 
    let _, ay = a and _, by = b in
    ay - by
  ) points in
  let x0, y0 = List.nth sorted_points 0
  and x1, y1 = List.nth sorted_points 1
  and x2, y2 = List.nth sorted_points 2 in

  let long_edge = interpolate_line x0 y0 x2 y2 in

  let s1 = interpolate_line x0 y0 x1 y1
  and s2 = interpolate_line x1 y1 x2 y2 in
  let other_edge = s1 @ (List.tl s2) in
  assert ((List.length long_edge) == (List.length other_edge));

  let spans = List.map2 (fun a b -> (a, b)) long_edge other_edge in
  List.iteri (fun i s -> 
    let l, r = s in
    Framebuffer.draw_line l (y0 + i) r (y0 + i) col fb
  ) spans;
  List.iteri (fun i p -> 
    let x, y = p in
    ignore(Framebuffer.draw_char x y font (char_of_int ((int_of_char '0') + i)) (col / 2) fb)
  ) sorted_points


let tick t s fb _i =
  Framebuffer.shader_inplace (fun _p -> 0) fb;
  let width, height = Screen.dimensions s
  and ft = (Float.of_int t) /. 500.
  and col = (Palette.size (Screen.palette s)) -1 in
  Framebuffer.draw_circle (width / 2) (height / 2) 150. (col / 4) fb;
  Framebuffer.draw_circle (width / 2) (height / 2) 100. (col / 3) fb;
  Framebuffer.draw_circle (width / 2) (height / 2) 50. (col / 2) fb;

  match (Screen.font s) with 
  | None -> fb
  | Some font -> (
    filled_triangle
      ((Int.of_float (50. *. (sin (ft)))) + (width / 2))
      ((Int.of_float (50. *. (cos (ft)))) + (height / 2))
      ((Int.of_float (150. *. (sin (ft +. (Float.pi *. 1.5))))) + (width / 2))
      ((Int.of_float (150. *. (cos (ft +. (Float.pi *. 1.5))))) + (height / 2))
      ((Int.of_float (100. *. (sin (ft +. 2.)))) + (width / 2))
      ((Int.of_float (100. *. (cos (ft +. 2.)))) + (height / 2))
      col fb font;
    fb
  )

let () = 
  match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 16))) |>
    Screen.create_with_font 640 480 1 font |>
    Base.run "Triangle testing" None tick
  )