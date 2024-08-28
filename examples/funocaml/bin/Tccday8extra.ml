open Claudius

let vapour_palette = "000:2420387400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"

let palette = Palette.load_tic80_palette vapour_palette

let boot (screen : Screen.t) : Framebuffer.t =
  let width, height = Screen.dimensions screen in
  let buffer = Framebuffer.init (width, height) (fun _x _y -> 0) in
  Framebuffer.filled_circle (width / 2) (height / 2) (Float.of_int (height / 2)) 1 buffer;
  buffer

let tick (t : int) (screen : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t) : Framebuffer.t =
  let buffer = Framebuffer.shader (fun pixel ->
    match pixel with
    | 0 | 1 -> pixel
    | _ -> (pixel - 1)
  ) prev in

  let n = 40
  and m = 200
  and r = Float.pi *. 2. /. 235.
  and x = ref 0.
  and v = ref 0.
  and ft = (Float.of_int t) *. (0.025 /. 2.) in
  let width, height = Screen.dimensions screen in

  let qs = ((Float.of_int height) /. 4.) -. 1. in

  for i = 0 to n do
    for j = 0 to m do
      let fi = Float.of_int i in
      let a = fi +. !v
      and b = (r *. fi) +. !x in
      let u = (sin a) +. (sin b) in
      v := (cos a) +. (cos b);
      x := u +. ft;

      let col = 2 + ((i + (j / 36)) mod ((Palette.size (Screen.palette screen)) - 2)) in
      let xpos = ((width / 2) + Int.of_float (u *. qs))
      and ypos = ((height / 2) + Int.of_float (!v *. qs)) in
      Framebuffer.pixel_write xpos ypos col buffer
    done
  done;
  buffer

let slide = (palette, Some boot, tick)
