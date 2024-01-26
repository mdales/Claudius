open Claudius

let boot s = 
  let buffer = Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0) in

  let rec loop x y n =
    match n-1 with 
    | 0 -> ()
    | nn -> (
        let r = Random.float 1. in
        let xn, yn = (
            if r < 0.01 then (0.0, 0.16 *. y) else (
                if r < 0.86 then (0.85 *. x +. 0.04 *. y, -0.04 *. x +. 0.85 *. y +. 1.6) else (
                    if r < 0.93 then (0.2 *. x -. 0.26 *. y, 0.23 *. x +. 0.22 *. y +. 1.6) else (
                        -0.15 *. x +. 0.28 *. y,  0.26 *. x +. 0.24 *. y +. 0.44
                    )
                )
            )
        ) in
        Framebuffer.pixel_write 
            ((Int.of_float (yn *. 40.) + 100))
            ((Int.of_float (xn *. 40.)) + 240)
            15 buffer;

        loop xn yn nn
    )
  in loop 0. 0. 50000;

  ;buffer

let tick _t _s p = p

let () = 
  Palette.of_list (List.map (fun x -> x land 0xff00) (List.rev (Palette.to_list (Palette.generate_mono_palette 16)))) |>
  Screen.create 640 480 1 |>
  Base.run "Fern" (Some boot) tick
