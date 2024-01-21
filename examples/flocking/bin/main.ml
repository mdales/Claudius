open Claudius

let sigma = 12.
let rho = 28.
let beta = 2.667
let dt = 0.002

let cur = ref (List.init 1024 (fun i ->
    ((Random.float 30.) -. 15., (Random.float 120.) -. 60., (Random.float 310.) -. 150., i)
  ))


(* ----- *)

let lorenz (prev: float * float * float * int) : (float * float * float * int) =
  let x, y, z, col = prev in
  (
    x +. ((sigma *. (y -. x)) *. dt),
    y +. (((rho *. x) -. y -. (x *. z)) *. dt),
    z +. (((x *. y) -. (beta *. z)) *. dt),
    col
  )

let project (point: float * float * float * int) (t : int) (s : Screen.t) : (int * int * int) =
  let ft = (Float.of_int t) *. dt in
  let x, y, z, col = point in
  let width, height = Screen.dimensions s in
  let dx = (Int.of_float ((x *. 10. *. (sin ft)) +. (y *. 10. *. (cos ft)))) + (width / 2)
  and dy = height - ((Int.of_float (z *. 7.5)) + (height / 6)) + 30 in
  let palrange = 1 + (col mod ((Palette.size (Screen.palette s)) - 2)) in
  (dx, dy, palrange)

(* ----- *)
  
let tick t s fb = 
  (* Fade what came before *)
  Framebuffer.shader_inplace (fun _pixel -> 
    (* match pixel with
    | 0 -> pixel
    | _ -> (pixel - 1) *) 0
  ) fb;

  (* Work out next point *)
  let next = List.map lorenz !cur in
  
  (* Draw the latest update *)
  List.iter2 (fun cur next ->
    let x0, y0, _ = project cur t s in
    let x1, y1, col = project next t s in
    Framebuffer.draw_line x0 y0 x1 y1 col fb;
  ) !cur next;

  cur := next;
  fb

let () = 
  Palette.of_list [ 0xaaccff ; 0x0 ; 0xff0000 ; 0x0 ; 0x00ff00 ; 0x0 ; 0x0000ff ; 0x0 ] |>
  Screen.create 640 480 1 |>
  Base.run "Flocking" None tick
