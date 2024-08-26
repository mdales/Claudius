open Claudius

let palette = Palette.load_tic80_palette "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"



(* ----- *)
(*
let random_line (width : int) (palette : color list) : (color array) =
  Array.init width (fun _x -> List.nth palette ((Random.int ((List.length palette) / 2)) + 0))

let random_screen (width: int) (height: int) (palette : color list) : (color array array) =
  Array.init height (fun _x -> random_line width palette)*)

(* ----- *)

let boot s =
  Random.init 42;
  let p = Screen.palette s in
  Framebuffer.init (Screen.dimensions s) (fun _ _ -> Random.int ((Palette.size p) / 2))

let draw_splots t s fb =
  let w, h = Screen.dimensions s in
  if (t mod 3) == 0 then
    let ft = float_of_int t in
    let dot_count = 5 in
    let col = ((t / 3) mod (dot_count - 1)) + 8 in
    for i = 0 to dot_count do
      let d = 90. +. ((sin (ft /. 20.)) *. 30.)
      and	a = (float_of_int i) *. ((Float.pi *. 2.) /. (float_of_int dot_count)) in
      let fx = cos(a +. (ft /. 30.)) *. d
      and fy = sin(a +. (ft /. 20.)) *. d in
      let x = int_of_float fx
      and y = int_of_float fy
      and rad = 14. +. (sin (ft /. 20.) *. 6.) in
      Framebuffer.filled_circle (((2 * w / 3)) + x) ((h / 2) + y) rad col fb
    done

let tick t s fb _i =
  draw_splots t s fb;
  let palsize = Palette.size (Screen.palette s) in
  Framebuffer.shaderi_inplace (fun x y tfb ->
    match Framebuffer.pixel_read (x + 1) y tfb with
    | None -> Random.int (palsize / 2)
    | Some v -> v
  ) fb; fb

let slide = (palette, boot, tick)
