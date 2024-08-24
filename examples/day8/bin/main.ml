open Claudius

let sigma = 10.
let rho = 28.
let beta = 2.667
let dt = 0.01

let cur = ref (0., 1., 1.05)

(* ----- *)

let lorenz (prev: float * float * float) : (float * float * float) =
  let x, y, z = prev in
  (
    x +. ((sigma *. (y -. x)) *. dt),
    y +. (((rho *. x) -. y -. (x *. z)) *. dt),
    z +. (((x *. y) -. (beta *. z)) *. dt)
  )

let project (point: float * float * float) (s : Screen.t) : (int * int * int) =
  let x, _, z = point in
  let width, height = Screen.dimensions s in
  let dx = (Int.of_float (x *. 10.)) + (width / 2)
  and dy = height - ((Int.of_float (z *. 7.5)) + (height / 6)) + 30 in
  let palrange = ((Palette.size (Screen.palette s)) - 1) in
  (dx, dy, palrange)

(* ----- *)

let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)

let tick _t s prev _i =
  (* Fade what came before *)
  let buffer = Framebuffer.shader (fun pixel ->
    match pixel with
    | 0 -> pixel
    | _ -> (pixel - 1)
  ) prev in

  (* Work out next point *)
  let next = lorenz !cur in

  (* Draw the latest update *)
  let x0, y0, _ = project !cur s in
  let x1, y1, col = project next s in

  cur := next;
  Framebuffer.draw_line x0 y0 x1 y1 col buffer;
  buffer

(* ----- *)

let () =
  Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 1024))) |>
  Screen.create 640 480 1 |>
  Base.run "Genuary Day 8: Chaotic" (Some boot) tick
