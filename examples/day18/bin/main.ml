open Claudius

let outer_radius = 140.
let inner_radius = 100.

let boot s = 
  let max_col = (Palette.size (Screen.palette s)) - 1 in
  let w, h = Screen.dimensions s in
  let fb = Framebuffer.init (w, h) (fun _x _y -> 0) in
  Framebuffer.filled_circle (w / 2) (h / 2) outer_radius max_col fb;
  Framebuffer.filled_circle (w / 2) (h / 2) inner_radius 0 fb;
  for _ = 0 to 100 do
    let angle = Random.float (2. *. Float.pi) in
    let x = Int.of_float ((outer_radius +. 1.) *. (cos angle)) in
    let y = Int.of_float ((outer_radius +. 1.) *. (sin angle)) in
    Framebuffer.pixel_write (x + (w/2)) (y + (h/2)) (max_col - 1) fb
  done;
  fb


let tick_d t s fb =
  let w, h = Screen.dimensions s in
  let cx = (w/2) and cy = (h/2) in
  let delta = Framebuffer.init (w, h) (fun _x _y -> 0) in
  Framebuffer.shaderi_inplace (fun x y ofb -> 
    let op = Framebuffer.pixel_read x y ofb in
    match op with
    | None -> 0
    | Some (p) -> 
        match p with
        | 0 -> 0
        | 255 -> 255
        | i -> (
          let dx = Float.of_int(x - cx) 
          and dy = Float.of_int(y - cy) in
          let wobble = 0.05 *. sin((Float.of_int t) /. 100.) in
          let angle = (atan2 dy dx) +. ((Random.float wobble) -. (wobble /. 2.))
          and radius = sqrt ((dy *. dy) +. (dx *. dx)) in
          let nx = Int.of_float ((radius +. 2.) *. (cos angle)) in
          let ny = Int.of_float ((radius +. 2.) *. (sin angle)) in
          let col = i in
          Framebuffer.pixel_write (nx + cx) (ny + cy) col delta;
          col - 1
    )
  ) fb;
  Framebuffer.merge_inplace fb delta;
  fb

let tick t s fb =
  if ((t mod 200) == 0) then
    boot s
  else 
    tick_d t s fb

let () =
  Palette.generate_mono_palette 256 |>
  Screen.create 480 480 1 |>
  Base.run "Genuary 18: Bauhaus" (Some boot) tick 
