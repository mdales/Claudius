
type t = int array array

let init (dimensions : int * int) (f : int -> int -> int) : t =
  let width, height = dimensions in
  Array.init height (fun y ->
    Array.init width (fun x -> 
        f x y
      )
  ) 

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

let pixel_write (x : int) (y : int) (col : int) (buffer : t) =
  if (x >= 0) && (x < Array.length (buffer.(0))) && (y >= 0) && (y < Array.length buffer) then
    buffer.(y).(x) <- col

let pixel_read (x : int) (y : int) (buffer : t) : int option =
    if (x >= 0) && (x < Array.length (buffer.(0))) && (y >= 0) && (y < Array.length buffer) then
      Some buffer.(y).(x)
    else
      None
      
let render (buffer : t) (draw : Primitives.t list) =
  List.iter (fun prim -> 
    match prim with
    | Primitives.Circle (point, r, col) -> filled_circle point.x point.y r col buffer
    | Primitives.Line (p1, p2, col) -> draw_line p1.x p1.y p2.x p2.y col buffer
    | Primitives.Pixel (p, col) -> pixel_write p.x p.y col buffer
    | Primitives.Polygon (plist, col) -> draw_polygon (List.map (fun (p : Primitives.point) -> (p.x, p.y)) plist) col buffer
  ) draw
