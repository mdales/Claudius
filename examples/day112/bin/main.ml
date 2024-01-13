open Claudius

let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"

(* ----- *)

let tick _t s _p =
  let buffer = Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0) in
  let w, h = Screen.dimensions s in
  let cols = 5
  and rows = 30 in

  for y = 0 to (rows - 1) do
    for x = 0 to (cols - 1) do
      (* let fx = Float.of_int x and fy = Float.of_int y  and ft = Float.of_int t in
      let c = 1 + Int.of_float ((sin (sin (fx /. 8.))) *. (sin (sin (fy /. 8.))) *. (1. +. (5. *. sin (ft /. 500.)))) mod cols in *)
      
      Framebuffer.filled_rect
        (x * (w / cols))
        (y * (h / rows))
        (w / cols)
        (h / rows)
        (* (if c < 0 then c + cols else c) *) 4
        buffer
    done
  done;


  buffer

(* ----- *)

let () =
  Palette.load_tic80_palette havrekaka_palette |>
  Screen.create 500 600 1 |>
  Tcc.run "Genuary 11: Anni Albers" None tick
  