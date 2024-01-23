
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

let draw_glyph (x : int) (y : int) (f : Font.t) (c : char) (col : int) (buffer : t) : int =
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
    | [] -> ()
    | c :: remaining -> (
      let width = draw_glyph (x + offset) y f c col buffer in
      loop (offset + width) remaining
    )
  in loop 0 sl


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
  ) draw
