open Claudius

let sigma = 10.
let rho = 28.
let beta = 2.667
let dt = 0.01

let lorenz (prev: float * float * float) : (float * float * float) =
  let x, y, z = prev in
  (
    x +. ((sigma *. (y -. x)) *. dt),
    y +. (((rho *. x) -. y -. (x *. z)) *. dt),
    z +. (((x *. y) -. (beta *. z)) *. dt)
  )

let cur = ref (0., 1., 1.05)

let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)

let tick _t s prev = 
  let buffer = Array.map (fun row ->
    Array.map (fun pixel -> 
      match pixel with
      | 0 | 1 -> pixel
      | _ -> (pixel - 1)
    ) row
  ) prev in
  let next = lorenz !cur in
  let x, _y, z = next in
  let width, height = Screen.dimensions s in
  let dx = (Int.of_float (x *. 9.)) + (width / 2)
  and dy = height - ((Int.of_float (z *. 7.)) + (height / 6)) in
  let palrange = ((Palette.size (Screen.palette s)) - 1) in
  (* let col = 1 + (Int.of_float (palrange *. ((30. +. y) /. 60.))) in *)
  Framebuffer.pixel_write dx dy palrange buffer;
  (* Framebuffer.filled_circle dx dy (((30. +. y) /. 60.) *. 2.) col p; *)
  cur := next;
  buffer

let () =
  let pal = List.rev (Palette.to_list (Palette.generate_mono_palette 1024)) in
  let screen = Screen.create 640 480 1 (Palette.of_list pal) in
  Tcc.run screen "Genuary Day 8: Chaotic" (Some boot) tick
