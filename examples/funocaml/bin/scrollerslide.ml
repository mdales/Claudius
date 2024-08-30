open Claudius

let palette = Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let generate_stars t s seed density radius =
  let width, height = Screen.dimensions s in
  Random.init seed;
  List.init density (fun _ ->
    let updated_i = (Random.int width) - (t mod width) in
      let adjusted_updated_i = (if updated_i >= 0 then updated_i else updated_i + width) in
      Primitives.FilledCircle (
        {x = adjusted_updated_i; y = Random.int height},
        radius,
        11 + Random.int 3
      )
  )

let tick prose t s _fb _i =

  let ft = float_of_int t in
  let w, h = Screen.dimensions s in
  let font = Option.get (Screen.font s) in
  let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in

  List.concat [
    generate_stars t s 42 100 0.5;
    generate_stars (t * 2) s 22 50 0.5;
    generate_stars (t * 4) s 32 25 1.0
  ] |> Framebuffer.render fb ;

  List.init (String.length prose) (String.get prose) |>
  List.mapi (fun i c ->
    let xpos = (w - ((t * 3) mod (w + 200)) + (i * 12)) in
    let fxpos = float_of_int xpos in
    let ypos = (h / 2) + (int_of_float ((sin (fxpos /. 30.)) *.  (10. +. (40. *. (cos (ft /. 100.)))))) in
    Primitives.Char (
      {x = xpos; y = ypos},
      font,
      c,
      12
    )
  ) |> Framebuffer.render fb;
  fb

let generate_slide prose = (palette, None, tick prose)
