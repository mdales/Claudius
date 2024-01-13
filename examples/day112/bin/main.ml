open Claudius

let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"

let band_height = 20

(* ----- *)

let generate_tile _t w h base_col over_col : Primitives.t list =
  let rec loop index rest = (
    let this = Primitives.FilledRect (
      {x = 0 ; y = index * band_height},
      {x = w ; y = (index + 1) * band_height},
      if ((Random.int 5) > 0) then over_col else base_col) in
    match index with
    | 0 -> this :: rest
    | _ -> this :: loop (index - 1) rest
  ) in loop ((h / band_height) - 1) []


let tick t s _p =
  (* Random.init 42; *)
  let buffer = Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0) in
  let w, h = Screen.dimensions s in
  let cols = 5
  and rows = 5 in
  let xstep = w / cols
  and ystep = h / rows in


  for iy = 0 to (rows - 1) do
    for ix = 0 to (cols - 1) do
      let x = ix * xstep
      and y = iy * ystep in
      let tile = generate_tile t (w/cols) (h/rows) (iy) ix in
      let shifted = List.map (fun (p : Primitives.t) : Primitives.t ->
        match p with
        | Primitives.FilledRect (p1, p2, col) -> Primitives.FilledRect ({x=p1.x + x; y = p1.y + y}, {x=p2.x + x ; y = p2.y + y}, col)
        | x -> x
      ) tile in
      Framebuffer.render buffer shifted
    done
  done;


  buffer

(* ----- *)

let () =
  Palette.load_tic80_palette havrekaka_palette |>
  Screen.create 500 600 1 |>
  Tcc.run "Genuary 11: Anni Albers" None tick
