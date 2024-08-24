open Claudius

let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"

(* ----- *)

let tick t s _p _i =
  let cols = Palette.size (Screen.palette s) in
  Framebuffer.init (Screen.dimensions s) (fun x y ->
    let fx = Float.of_int x and fy = Float.of_int y  and ft = Float.of_int t in
    let c = 1 + Int.of_float ((sin (sin (fx /. 8.))) *. (sin (sin (fy /. 8.))) *. (1. +. (5. *. sin (ft /. 500.)))) mod cols in
    if c < 0 then c + cols else c
  ) 

(* ----- *)

let () =
  Screen.create 126 226 3 (Palette.load_tic80_palette havrekaka_palette) |>
  Base.run "Genuary 11: Anni Albers" None tick
  