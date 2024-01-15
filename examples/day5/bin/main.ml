open Claudius

let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"

let tick t s p =

  Unix.sleepf 0.02;

  let buffer = if (t mod 100) == 0 then 
    Framebuffer.init (Screen.dimensions s) (fun _x _y -> 15)
  else 
    p
  in

  let raw_squares = List.concat (
    List.init 5 (fun x -> 
      let xo = x * 100 in
      List.init 5 (fun y : (int * Primitives.point list) -> 
        let yo = y * 100 in
        ((x * 5) + y), 
        List.map (fun (p : Primitives.point) : Primitives.point -> 
          {x = p.x + xo ; y = p.y + yo}
        ) [{x=10 ; y=10} ; {x=90; y=10} ; {x=90; y=90} ; {x=10; y=90} ]
      )
    )
  ) in
  let shuffled_squares = List.map ( fun square_item -> 
    let col, square = square_item in
    col + ((t / 100) * 3), 
    List.map (fun (p : Primitives.point) : Primitives.point -> 
      {x = p.x + (Random.int (1 + col)) - 10 ; y = p.y + (Random.int (1 + col)) - 10}
    ) square
  ) raw_squares in
  let primitives_list = List.map ( fun item ->
    let col, square = item in Primitives.Polygon (square, (col mod ((Palette.size (Screen.palette s)) - 1))) 
  ) shuffled_squares in
  Framebuffer.render buffer primitives_list;

  buffer

let () = 
  Screen.create 500 500 1 (Palette.load_tic80_palette havrekaka_palette) |>
  Base.run "Genuary Day 5: Vera Molnár" None tick
