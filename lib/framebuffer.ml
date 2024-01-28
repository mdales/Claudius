
type t = int array array

type shader_func = int -> int

type shaderi_func = int -> int -> t -> int

let to_array (buffer : t) : int array array =
  buffer

let init (dimensions : int * int) (f : int -> int -> int) : t =
  let width, height = dimensions in
  Array.init height (fun y ->
    Array.init width (fun x -> 
        f x y
      )
  ) 

let pixel_write (x : int) (y : int) (col : int) (buffer : t) =
  if (x >= 0) && (x < Array.length (buffer.(0))) && (y >= 0) && (y < Array.length buffer) then
    buffer.(y).(x) <- col

let pixel_read (x : int) (y : int) (buffer : t) : int option =
  if (x >= 0) && (x < Array.length (buffer.(0))) && (y >= 0) && (y < Array.length buffer) then
    Some buffer.(y).(x)
  else
    None

let draw_circle (x : int) (y : int) (r : float) (col : int) (buffer : t) =
  let fx = Float.of_int x
  and fy = Float.of_int y in

  for yo = 0 to (Int.of_float (r *. sin (Float.pi *. 0.25))) do 
    let yi = y + yo in
    let a = acos ((Float.of_int (yi - y)) /. r) in
    let xw = (sin a) *. r in

    pixel_write (Int.of_float (fx -. xw)) (y + yo) col buffer;
    pixel_write (Int.of_float (fx +. xw)) (y + yo) col buffer;
    pixel_write (Int.of_float (fx -. xw)) (y - yo) col buffer;
    pixel_write (Int.of_float (fx +. xw)) (y - yo) col buffer;

    pixel_write (x + yo) (Int.of_float (fy -. xw)) col buffer;
    pixel_write (x - yo) (Int.of_float (fy -. xw)) col buffer;
    pixel_write (x + yo) (Int.of_float (fy +. xw)) col buffer;
    pixel_write (x - yo) (Int.of_float (fy +. xw)) col buffer;

  done

let filled_circle (x : int) (y : int) (r : float) (col : int) (buffer : t) =
  let fx = Float.of_int x and fy = Float.of_int y in
  let my = Float.of_int ((Array.length buffer) - 1)
  and mx = Float.of_int ((Array.length buffer.(0)) - 1) in
  let pminy = fy -. r
  and pmaxy = fy +. r in
  let miny = if (pminy < 0.) then 0. else pminy
  and maxy = if (pmaxy > my) then my else pmaxy in
  for yi = (Int.of_float miny) to (Int.of_float maxy) do
    let row = buffer.(yi) in
    let a = acos ((Float.of_int (yi - y)) /. r) in
    let xw = (sin a) *. r in
    let pminx = fx -. xw
    and pmaxx = fx +. xw in
    let minx = if (pminx < 0.) then 0. else pminx
    and maxx = if (pmaxx > mx) then mx else pmaxx in
    if (maxx > 0.0) && (minx < mx) then
      for xi = (Int.of_float minx) to (Int.of_float maxx) do
        row.(xi) <- col
      done
  done

let draw_line (x0 : int) (y0 : int) (x1 : int) (y1 : int) (col : int) (buffer : t) =
  let dx = abs (x1 - x0)
  and sx = if x0 < x1 then 1 else -1
  and dy = (abs (y1 - y0)) * -1
  and sy = if y0 < y1 then 1 else -1 in
  let initial_error = dx + dy in
    
  let rec loop (x : int) (y : int) (error : int) =
    if (x >= 0) && (x < Array.length (buffer.(0))) && (y >= 0) && (y < Array.length buffer) then
      buffer.(y).(x) <- col;
    match (x == x1) && (y == y1) with
    | true -> ()
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
      loop nx ny (error + nex + ney)
    )
  in loop x0 y0 initial_error

let draw_polygon (points : (int * int) list) (col : int) (buffer : t) = 
  match points with 
  | [] -> ()
  | hd :: tl -> (
    let rec loop start prev rest =
      let x0, y0 = prev in
      match rest with
      | [] -> ()
      | ihd :: [] -> (
        let x1, y1 = ihd in
        draw_line x0 y0 x1 y1 col buffer;
        let xs, ys = start in
        draw_line x1 y1 xs ys col buffer;
      )
      | ihd :: itl -> (
        let x1, y1 = ihd in
        draw_line x0 y0 x1 y1 col buffer;
        loop start ihd itl
      ) in loop hd hd tl
  )

let draw_rect (x : int) (y : int) (width : int) (height : int) (col : int) (buffer : t) =
  draw_polygon [ (x, y) ; (x + width, y) ; (x + width, y + height) ; (x , y + height)] col buffer

let filled_rect (x : int) (y : int) (width : int) (height : int) (col : int) (buffer : t) =
  for oy = 0 to height do
    draw_line x (y + oy) (x + width) (y + oy) col buffer
  done

let draw_triangle (x0 : int) (y0 : int) (x1 : int) (y1 : int) (x2 : int) (y2 : int) (col : int) (buffer : t) =
  draw_line x0 y0 x1 y1 col buffer;
  draw_line x1 y1 x2 y2 col buffer;
  draw_line x2 y2 x0 y0 col buffer

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

let filled_triangle (x0 : int) (y0 : int) (x1 : int) (y1 : int) (x2 : int) (y2 : int) (col : int) (buffer : t) =
  let points = [(x0, y0) ; (x1, y1) ; (x2, y2)] in
  let sorted_points = List.sort (fun a b -> 
    let _, ay = a and _, by = b in
    ay - by
  ) points in
  let x0, y0 = List.nth sorted_points 0
  and x1, y1 = List.nth sorted_points 1
  and x2, y2 = List.nth sorted_points 2 in

  let long_edge = interpolate_line x0 y0 x2 y2 in

  let other_edge = if (y1 == y0) then (
    interpolate_line x1 y1 x2 y2
  ) else if (y1 == y2) then (
    interpolate_line x0 y0 x1 y1
  ) else (
    let s1 = interpolate_line x0 y0 x1 y1
    and s2 = interpolate_line x1 y1 x2 y2 in
    s1 @ (List.tl s2)
  ) in
  assert ((List.length long_edge) == (List.length other_edge));
  
  let spans = List.map2 (fun a b -> (a, b)) long_edge other_edge in
  List.iteri (fun i s ->
    let row = buffer.(y0 + i) in
    let stride = Array.length row in
    let p, q = s in
    let p0, p1 = if (p <= q) then p, q else q, p in
    let x0 = if (p0 < 0) then 0 else (if (p0 >= stride) then (stride - 1) else p0)
    and x1 = if (p1 < 0) then 0 else (if (p1 >= stride) then (stride - 1) else p1) in
    Array.fill row x0 ((x1 - x0) + 1) col
  ) spans

(* ----- *)

let draw_char (x : int) (y : int) (f : Font.t) (c : char) (col : int) (buffer : t) : int =
  match Font.glyph_of_char f (Uchar.of_char c) with
  | None -> 0
  | Some glyph -> (
    let gw, gh = Font.glyph_dimensions glyph in
    let bmp = Font.glyph_bitmap glyph in
    let bytes_per_line = (Bytes.length bmp) / gh in
    for h = 0 to (gh - 1) do
      for w = 0 to (bytes_per_line - 1) do
        let bitcount = if (((w + 1) * 8) < gw) then 8 else ((gw - (w * 8)) mod 8) in
        let b = int_of_char (Bytes.get bmp ((h * bytes_per_line) + w)) in
        for bit = 0 to (bitcount - 1) do
          let isbit = (b lsl bit) land 0x80 in
          match isbit with
          | 0 -> ()
          | _ -> 
          pixel_write 
            (x + (w * 8) + bit)
            (y + h) 
            col
            buffer
        done
      done
    done; gw
  )

let draw_string (x : int) (y : int) (f : Font.t) (s : string) (col : int) (buffer : t) = 
  let sl = List.init (String.length s) (String.get s) in
  let rec loop offset remaining = 
    match remaining with
    | [] -> offset
    | c :: remaining -> (
      let width = draw_char (x + offset) y f c col buffer in
      loop (offset + width) remaining
    )
  in loop 0 sl

(* ----- *)

let shader (f: shader_func) (buffer : t) : t =
  Array.map (fun row ->
    Array.map f row
  ) buffer

let shaderi (f: shaderi_func) (buffer : t) : t = 
  Array.mapi (fun y row ->
    Array.mapi (fun x _p -> f x y buffer) row
  ) buffer

let shader_inplace (f: shader_func) (buffer : t) =
  Array.iter (fun row ->
    Array.map_inplace f row
  ) buffer

let shaderi_inplace (f: shaderi_func) (buffer : t) = 
  Array.iteri (fun y row ->
    Array.mapi_inplace (fun x _p -> f x y buffer) row
  ) buffer

(* ---- *)

let render (buffer : t) (draw : Primitives.t list) =
  List.iter (fun prim ->
    match prim with
    | Primitives.Circle (point, r, col) -> draw_circle point.x point. y r col buffer
    | Primitives.FilledCircle (point, r, col) -> filled_circle point.x point.y r col buffer
    | Primitives.Line (p1, p2, col) -> draw_line p1.x p1.y p2.x p2.y col buffer
    | Primitives.Pixel (p, col) -> pixel_write p.x p.y col buffer
    | Primitives.Polygon (plist, col) -> draw_polygon (List.map (fun (p : Primitives.point) -> (p.x, p.y)) plist) col buffer
    | Primitives.Rect (p1, p2, col) -> draw_rect p1.x p1.y (p2.x - p1.x) (p2.y - p1.y) col buffer
    | Primitives.FilledRect (p1, p2, col) -> filled_rect p1.x p1.y (p2.x - p1.x) (p2.y - p1.y) col buffer
    | Primitives.Triangle (p1, p2, p3, col) -> draw_triangle p1.x p1.y p2.x p2.y p3.x p3.y col buffer
    | Primitives.FilledTriangle (p1, p2, p3, col) -> filled_triangle p1.x p1.y p2.x p2.y p3.x p3.y col buffer
    | Primitives.Char (p, font, c, col) -> ignore (draw_char p.x p.y font c col buffer)
    | Primitives.String (p, font, s, col) -> ignore (draw_string p.x p.y font s col buffer )
  ) draw

(* ----- *)

let merge (f : int -> int -> int) (origin : t) (delta : t) : t =
  Array.map2 (fun o_row d_row ->
    Array.map2 (fun o_pixel d_pixel ->
      f o_pixel d_pixel
    ) o_row d_row
  ) origin delta

let merge_inplace (f : int -> int -> int) (origin : t) (delta : t) =
  Array.iter2 (fun o_row d_row ->
    Array.iteri (fun index d_pixel ->
      o_row.(index) <- f o_row.(index) d_pixel
    ) d_row
  ) origin delta
